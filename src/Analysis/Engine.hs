{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Engine
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Engine where

import Analysis.API
import Analysis.Debug
import Analysis.Java.ClassInfo
import Analysis.Java.Flow
import Analysis.Dependence
import Analysis.Model.ArrayList
import Analysis.Model.Queue
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Maybe
import Language.Java.Pretty
import Language.Java.Syntax
import Util
import Z3.Monad hiding (Params)
import qualified Data.Map as M

-- encodePre :: generates the initial SSA map and pre-condition 
encodePre :: [MemberSig] -> Z3 (SSAMap,AST)
encodePre inp = do 
  (ssa,eqs) <- foldM encodeInput (M.empty,[]) inp 
  pre       <- if null eqs then mkTrue else mkAnd eqs
  return (ssa,pre)
 where
  -- | Auxiliary Function that encodes one Variable
  encodeInput :: (SSAMap,[AST]) -> MemberSig -> Z3 (SSAMap,[AST])
  encodeInput (ssa,pre) sig = do 
   (nssa,vars,matrix) <- foldM (encodeIVar sig) (ssa,[],[]) [1..4] 
   let tmatrix = transpose matrix
   axs  <- foldM encodeAxioms [] vars 
   pres <- foldM (\k row -> mapM (uncurry mkEq) (lin row) >>= \eqs -> return (k ++ eqs)) [] tmatrix
   return (nssa,pre++pres++axs)
  -- | Auxiliary Function 
  encodeIVar :: MemberSig -> (SSAMap,[SSAVar],[[AST]]) -> VId -> Z3 (SSAMap,[SSAVar],[[AST]])
  encodeIVar sig@(ident,tys) (ssa,vars,matrix) vId = do 
   var <- encodeVariable vId sig 
   let row     = getASTSSAVar var
       nssa    = insertSSAVar vId ident var ssa 
       nmatrix = row:matrix
   return (nssa,var:vars,nmatrix)

-- | Encode the axioms of the variables
encodeAxioms :: [AST] -> SSAVar -> Z3 [AST]
encodeAxioms axs v@SSAVar{..} =
 case _v_mty of
  Queue -> do
   ax <- queueAxioms v
   return (ax:axs)
  ArrayList -> do
   ax <- arrayListAxioms v
   return (ax:axs)
  _ -> return axs
 
-- encode post-condition
encodePost :: SSAMap -> [MemberSig] -> Z3 (SSAMap, AST)
encodePost ssa fields = do 
  (nssa,conds) <- foldM encodePostVar (ssa,[]) fields 
  post <- mkAnd conds
  return (nssa,post)
 where
   -- | encode the post-condition per variable
   encodePostVar :: (SSAMap,[AST]) -> MemberSig -> Z3 (SSAMap,[AST])
   encodePostVar (ssa,post) sig@(Ident id,tys) = do 
    let res_id  = if id == "" then "ret" else "ret_"++id
        res_sig = (Ident res_id,tys)
    vars <- mapM (\vId -> encodeVariable vId res_sig) [1..4] 
    let ver  = M.fromList $ zip [1..4] vars 
        nssa = M.insert (Ident res_id) ver ssa
        [r_o,r_a,r_b,r_m] = map getASTSSAVar vars 
    -- Comparison between original and A
    _oa <- mapM (uncurry mkEq) $ zip r_o r_a
    oa  <- mkAnd _oa
    noa <- mkNot oa
    -- Comparison between original and B
    _ob <- mapM (uncurry mkEq) $ zip r_o r_b
    ob  <- mkAnd _ob
    nob <- mkNot ob
    -- Comparison between original and M
    _om <- mapM (uncurry mkEq) $ zip r_o r_m
    om  <- mkAnd _om
    -- Comparison between M and A
    _ma <- mapM (uncurry mkEq) $ zip r_m r_a
    ma  <- mkAnd _ma
    -- Comparison between M and B
    _mb <- mapM (uncurry mkEq) $ zip r_m r_b
    mb  <- mkAnd _mb
    -- Remains the same
    c1 <- mkImplies noa ma
    c2 <- mkImplies nob mb
    c3 <- mkAnd [ma,mb,om]
    c4 <- mkAnd [c1,c2]
    p <- mkOr [c3,c4]    
    return (nssa,p:post)

-- @ Encodings related to SSA Variables
-- | Encode Variable Declaration 
encodeVarDecl :: [VId] -> Type -> VarDecl -> EnvOp ()
encodeVarDecl vIds ty v@(VarDecl varid mvarinit) = do
 wizPrint $ "encodeVarDecl: " ++ show v 
 env@Env{..} <- get
 let ident = case varid of
       VarId id -> id
       _ -> error $ "encodeVarDecl: not supported " ++ show varid
     sig = (ident,[ty])
 vars <- lift $ mapM (\vId -> encodeVariable vId sig) vIds
 pres <- lift $ foldM encodeAxioms [] vars 
 npre <- lift $ mkAnd (_e_pre:pres)
 updatePre npre
 let ann = zip vIds vars
     ssa = foldl (\m (vId,var) -> insertSSAVar vId ident var m) _e_ssamap ann 
     lhs = NameLhs $ Name [ident]
 updateSSAMap ssa 
 case mvarinit of
   Nothing -> return ()
   Just (InitExp exp) ->  case exp of 
    InstanceCreation tArg (ClassType [(Ident "ArrayList",_)]) args mBdy -> do
     newEnc <- mapM (arrayListNew args) $ zip vars [1..4] 
     env@Env{..} <- get
     npre <- lift $ mkAnd (_e_pre:newEnc)
     updatePre npre
    _ -> assign vIds exp lhs EqualA exp
   Just _ -> error $ "encodeVarDecl: not supported"

-- | Encode New Variable
encodeVariable :: VId -> MemberSig -> Z3 SSAVar
encodeVariable vId (Ident id,[ty]) = do 
 let name = id ++ "_" ++ show vId ++ "_0"
 (sort,model,mty) <- encodeType vId id ty
 ast              <- mkFreshConst name sort
 let var = SSAVar ast sort 0 model mty
 return var
encodeVariable vId inv = error $ "encodeVariable: invalid input " ++ show inv

equalVariable :: SSAVar -> SSAVar -> Z3 AST
equalVariable v1 v2 = do
 s1 <- sortToString (_v_typ v1) 
 s2 <- sortToString (_v_typ v2) 
 if s1 == s2
 then do
   eqV <- mkEq (_v_ast v1) (_v_ast v2) 
   eqM <- equalModel (_v_mod v1) (_v_mod v2)
   if null eqM 
   then return eqV
   else mkAnd (eqV:eqM)
 else mkFalse

equalModel :: SSAVarModel -> SSAVarModel -> Z3 [AST]
equalModel m1 m2 = 
 mapM (\(e1,e2) -> mkEq (fst3 e1) (fst3 e2)) $ zip (M.elems m1) (M.elems m2)

-- | Encode Type 
encodeType :: VId -> String -> Type -> Z3 (Sort,SSAVarModel,VarType)
encodeType vId ident ty = do 
 -- liftIO $ putStrLn $ "encodType: " ++ show ty
 case ty of 
  PrimType pty -> do
   sort <- case pty of
    BooleanT -> mkBoolSort
    ByteT    -> mkBvSort 8
    ShortT   -> mkIntSort
    IntT     -> mkIntSort
    LongT    -> mkRealSort
    CharT    -> error "enc_ty: CharT"
    FloatT   -> mkRealSort
    DoubleT  -> mkRealSort
   return (sort,M.empty,Primitive)
  RefType rty -> encodeRefType vId ident rty

encodeRefType :: VId -> String -> RefType -> Z3 (Sort,SSAVarModel,VarType)
encodeRefType vId ident rty = case rty of
 ClassRefType cty -> encodeClassType vId ident cty
 ArrayType    aty -> do
  (at,_,_)<- encodeType vId ident aty
  intSort <- mkIntSort
  sort    <- mkArraySort intSort at 
  return (sort,M.empty,Array)

encodeClassType :: VId -> String -> ClassType -> Z3 (Sort,SSAVarModel,VarType) 
encodeClassType vId ident (ClassType l) = case l of 
 [(Ident "List",[ta])] -> do 
   intSort <- mkIntSort
   taSort  <- encodeTypeArg vId ident ta
   sort    <- mkArraySort intSort taSort 
   return (sort,M.empty,Array)
 [(Ident "ArrayList",[ta])] -> do 
   intSort <- mkIntSort
   taSort  <- encodeTypeArg vId ident ta
   sort    <- mkArraySort intSort taSort 
   model   <- arrayListInit vId ident
   return (sort,model,ArrayList)
 [(Ident "Queue",[ta])] -> do
   intSort <- mkIntSort
   taSort  <- encodeTypeArg vId ident ta
   sort    <- mkArraySort intSort taSort 
   model   <- queueInit vId ident
   return (sort,model,Queue) 
 [(Ident l,_)] -> do
   sym  <- mkStringSymbol l
   sort <- mkIntSort -- mkUninterpretedSort sym
   return (sort,M.empty,Object)
 list -> 
   let (idents,_) = unzip list
       ty = concatIdent idents
   in encodeClassType vId ident (ClassType [(ty,[])]) 

encodeTypeArg :: VId -> String -> TypeArgument -> Z3 Sort
encodeTypeArg vId ident ta = case ta of
 Wildcard _     -> mkIntSort
 ActualType rty -> do
  r <- encodeRefType vId ident rty
  return $ fst3 r  

-- @ Encoding for Expressions
-- | Get the ASTs of an Expression
--   Currently, only used in analyseRet
getASTExp :: VId -> Exp -> EnvOp [AST]
getASTExp vId expr = do 
 env@Env{..} <- get
 case expr of
  ExpName name -> do
    let ident = toIdent name
    return $ getASTSSAMap "encodeExp" vId ident _e_ssamap
  _ -> 
   case _e_rety of 
    Nothing -> error $ "getASTExp: no return type with a return statement?"
    Just ty -> do 
     (sort,_,_) <- lift $ encodeType vId "ret" ty 
     ast <- encodeExp (Just sort) vId expr
     return [ast]

encodeExpName :: VId -> Name -> EnvOp AST
encodeExpName vId (Name [])  = error "encodeExpName: Name []"
encodeExpName vId name       = do
 env@Env{..} <- get
 let ident@(Ident i) = toIdent name
 case M.lookup ident _e_ssamap of
  Nothing -> encodeConstant i 
  Just l  -> case M.lookup vId l of
    Nothing -> encodeConstant i 
    Just v  -> return $ _v_ast v 

encodeConstant :: String -> EnvOp AST
encodeConstant s = do 
 env@Env{..} <- get
 let k = String s
 case M.lookup k _e_consts of
   Nothing  -> do
    iSort <- lift $ mkIntSort
    ast <- lift $ mkFreshConst s iSort
    insertConst k ast 
    return ast
   Just ast -> return ast

-- | Encode an expression for a version 
--   This function potentially needs to receive a Sort 
--   in the case where it calls some method.
encodeExp :: Maybe Sort -> VId -> Exp ->  EnvOp AST
encodeExp mSort vId expr = do 
 wizPrint $ "encodeExp: " ++ show expr
 env@Env{..} <- get
 bSort <- lift $ mkBoolSort >>= return . Just
 iSort <- lift $ mkIntSort  >>= return . Just
 case expr of
  Lit lit -> encodeLiteral lit 
  ExpName name -> encodeExpName vId name
  BinOp lhsE op rhsE -> do
  -- This likely needs to change
    lhs <- encodeExp mSort vId lhsE
    rhs <- encodeExp mSort vId rhsE
    lift $ enc_binop op lhs rhs
  PreMinus nexpr -> do 
    nexprEnc <- encodeExp iSort vId nexpr
    lift $ mkUnaryMinus nexprEnc
  Cond cond _then _else -> do
    condEnc  <- encodeExp bSort vId cond
    _thenEnc <- encodeExp mSort vId _then
    _elseEnc <- encodeExp mSort vId _else
    lift $ mkIte condEnc _thenEnc _elseEnc        
  PreNot nexpr -> do
    nexprEnc <- encodeExp bSort vId nexpr
    lift $ mkNot nexprEnc
  ArrayAccess ai -> enc_array_access vId ai
  FieldAccess fa -> do
    asts <- enc_field_access vId fa
    return $ head asts
  MethodInv m -> encodeCall m mSort vId 
  InstanceCreation tyArgs cTy args cBdy -> encodeNew mSort vId cTy args
  Cast typ exp -> do
    wizPrint $ "encodeExp: Ignoring Cast"
    encodeExp mSort vId exp
  _ -> error $  "encodeExp: " ++ show expr

-- | Encode New Instance via a call to an uninterpreted function
encodeNew :: Maybe Sort -> VId -> ClassType -> [Argument] -> EnvOp AST 
encodeNew mSort vId t@(ClassType ((Ident cTy,_):_)) args = do
 let name = Name [Ident $ "New"++cTy] 
     call = MethodCall name args
 (sort,_,_) <- lift $ encodeClassType vId "" t
 encodeCall call (Just sort) vId

-- | Encode Method Call 
--  The environment needs to have a list of all the possible methods that it can call 
-- 
encodeCall :: MethodInvocation -> Maybe Sort -> VId -> EnvOp AST
encodeCall m mSort vId = do
 wizPrint $ "encodeCall: " ++ show m
 case m of  
  MethodCall (Name name) args -> do
    argsAST <- mapM (encodeExp mSort vId) args
    encCall mSort name argsAST
  -- PrimaryMethodCall: fields?
  PrimaryMethodCall e tys name args -> do
   case e of
     MethodInv (MethodCall (Name i) args') -> 
      encodeCall (MethodCall (Name (i++[name])) (args'++args)) mSort vId
     _ -> do
      wizPrint "encodeCall: warning!"
      argsAST <- mapM (encodeExp mSort vId) args
      arg <- encodeExp mSort vId e
      encCall mSort [name] (arg:argsAST)
  _ -> error $ "encodeCall: " ++ show m
 where
  encCall nSort name args = do
   wizPrint $ "encCall: " ++ show name 
   env@Env{..} <- get
   case name of 
    [] -> error $ "encCall: name = []"
    [id@(Ident ident)] -> do 
     let arity     = length args
         class_sum = _e_classes !! (vId - 1) 
         meths     = findMethodGen id arity class_sum
         cfgs      = map computeGraphMember meths
         deps  = foldr (\cfg res -> M.union res $ blockDep class_sum cfg) M.empty cfgs
     case M.lookup (id,arity) _e_fnmap of
       Nothing -> do
         sorts <- lift $ mapM getSort args 
         rSort <- case nSort of
                   Nothing -> lift $ mkIntSort
                   Just s  -> return s 
         fn    <- lift $ mkFreshFuncDecl ident sorts rSort
         ast   <- lift $ mkApp fn args
         let fnmap = M.insert (id,arity) (fn,deps) _e_fnmap 
         updateFunctMap fnmap 
         return ast 
       Just (ast,dep) -> do
         wizPrint $ "encCall: Found " ++ ident ++ " in function map" 
         sorts <- lift $ mapM getSort args 
         str <- lift $ funcDeclToString ast
         wizPrint $ "encCall: " ++ str
         --wizBreak
         lift $ mkApp ast args 
    (oc@(Ident ident):meth) -> 
     if isUpper $ head ident
     then encCall nSort [foldr (\(Ident a) (Ident b) -> Ident (a ++ "." ++ b)) oc meth] args  
     else do
      let objVar = getVarSSAMap ("call: " ++ show oc) vId oc _e_ssamap
      case _e_mode of
       Model -> 
        case  _v_mty objVar of
         Queue -> do 
          wizPrint $ "encCall: Queue"
          queueModel oc objVar meth vId 
         ArrayList -> do 
          wizPrint $ "encCall: ArrayList"
          arrayListModel oc objVar meth vId args 
         -- Call to an object; assume that the object is changed; thus, need to
         -- update it
         Object -> do 
           rhsAst  <- encCall (Just (_v_typ objVar)) [concatIdent meth] ((_v_ast objVar):args)
           env@Env{..} <- get
           nLhsVar <- lift $ updateVariable vId oc objVar 
           let ssamap = insertSSAVar vId oc nLhsVar _e_ssamap
           ass <- lift $ mkEq (_v_ast nLhsVar) rhsAst 
           pre <- lift $ mkAnd [_e_pre,ass]
           updatePre pre
           updateSSAMap ssamap
           return rhsAst
         t -> error $ "encCall: unsupported calls to " ++ show t
       Dep -> encCall nSort [concatIdent meth] ((_v_ast objVar):args)

-- Analyse Assign
assign :: [VId] -> Exp -> Lhs -> AssignOp -> Exp -> EnvOp ()
assign vIds _exp lhs aOp rhs = mapM_ assignVId vIds
 where
  assignVId vId = do
   -- First retrieve the LHS variable
   e <- get
   let ident = case lhs of
        NameLhs  name                            -> toIdent name 
        FieldLhs (PrimaryFieldAccess This ident) -> ident 
        ArrayLhs (ArrayIndex e args)             -> expToIdent e
       lhsVar = getVarSSAMap "assign" vId ident (_e_ssamap e) 
   nLhsVar <- lift $ updateVariable vId ident lhsVar
   -- This is incorrect in the case of arrays: a[i] = x
   rhsAst  <- encodeExp (Just $ _v_typ lhsVar) vId rhs
   iSort   <- lift $ mkIntSort
   env@Env{..} <- get
   case lhs of
    NameLhs (Name [ident]) -> do
     let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
     ass <- lift $ processAssign lhsVar nLhsVar aOp rhsAst 
     pre <- lift $ mkAnd [_e_pre,ass]
     updatePre pre
     updateSSAMap ssamap
    FieldLhs (PrimaryFieldAccess This ident) -> do
     let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
     ass <- lift $ processAssign lhsVar nLhsVar aOp rhsAst 
     pre <- lift $ mkAnd [_e_pre,ass]
     updatePre pre
     updateSSAMap ssamap
    ArrayLhs (ArrayIndex e args) -> do
     nLhsVar <- lift $ updateVariable vId ident lhsVar
     let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
     a <- encodeExp (Just $ _v_typ lhsVar) vId e
     i <- case args of
            [x] -> encodeExp (Just iSort) vId x
            _   -> error $ "assign: ArrayLhs " ++ show lhs 
     _rhsAst <- lift $ mkStore a i rhsAst
     ass <- lift $ mkEq (_v_ast nLhsVar) rhsAst 
     pre <- lift $ mkAnd [_e_pre,ass]
     updatePre pre
     updateSSAMap ssamap
    _ -> error $ show _exp ++ " not supported"

processAssign :: SSAVar -> SSAVar -> AssignOp -> AST -> Z3 AST
processAssign plhs lhs op rhs =
 let lhs_ast  = _v_ast lhs
     plhs_ast = _v_ast plhs
 in case op of 
   EqualA -> mkEq lhs_ast rhs
   AddA -> do
     rhs' <- mkAdd [plhs_ast, rhs]
     mkEq lhs_ast rhs'
   _ -> error $ "processAssign: " ++ show op ++ " not supported"

enc_array_access :: Int -> ArrayIndex -> EnvOp AST
enc_array_access p exp@(ArrayIndex e args) = do
  iSort <- lift $ mkIntSort
  a <- encodeExp Nothing p e 
  case args of
    [x] -> do
      i <- encodeExp (Just iSort) p x
      lift $ mkSelect a i
    [x,y] -> do
      i <- encodeExp (Just iSort) p x
      j <- encodeExp (Just iSort) p y
      b <- lift $ mkSelect a i
      lift $ mkSelect b j
    _ -> error $ "enc_array_access: " ++ show exp

enc_field_access :: VId -> FieldAccess -> EnvOp [AST]
enc_field_access vId exp = do
  case exp of
    PrimaryFieldAccess This (ident@(Ident str)) -> do
      env@Env{..} <- get
      return $ getASTSSAMap "encFieldAccess" vId ident _e_ssamap
    _ -> error $ "enc_field_access: " ++ show exp
    
encodeLiteral :: Literal -> EnvOp AST
encodeLiteral lit = case lit of
 Boolean True  -> lift $ mkTrue
 Boolean False -> lift $ mkFalse
 Int i         -> lift $ mkIntNum i
 Null          -> lift $ mkIntNum 0 
 String s      -> do
  env@Env{..} <- get
  case M.lookup lit _e_consts of
   Nothing -> do
    iSort <- lift $ mkIntSort
    let n = "_STR_CONST_" ++ s
    ast <- lift $ mkFreshConst n iSort
    insertConst lit ast 
    return ast
   Just ast -> return ast
 Float f -> lift $ mkRealNum f 
 _ -> error "processLit: not supported"

enc_binop :: Op -> AST -> AST -> Z3 AST
enc_binop op lhs rhs = do 
  case op of
    NotEq  -> mkEq lhs rhs >>= \eq -> mkNot eq
    And    -> mkAnd [lhs,rhs]
    Add    -> mkAdd [lhs,rhs]
    Mult   -> mkMul [lhs,rhs]
    Sub    -> mkSub [lhs,rhs]
    Rem    -> mkRem lhs rhs
    Div    -> mkDiv lhs rhs 
    LThan  -> mkLt lhs rhs
    LThanE -> mkLe lhs rhs
    GThan  -> mkGt lhs rhs
    GThanE -> mkGe lhs rhs
    Equal  -> mkEq lhs rhs
    COr    -> mkOr [lhs, rhs]
    CAnd   -> mkAnd [lhs, rhs]
    _ -> error $ "enc_binop: not supported " ++ show op

-- Analyse Post De/Increment
-- @NOTE: April'17: These expressions were optimised?
post_op :: [VId] -> Exp -> Exp -> Op -> String -> EnvOp ()
post_op vIds _exp lhs op str = mapM_ (\p -> post_op_inner p _exp lhs op str) vIds
 where
  post_op_inner :: VId -> Exp -> Exp -> Op -> String -> EnvOp ()
  post_op_inner vId _exp lhs op str = do
   iSort <- lift $ mkIntSort
   rhsAst <- encodeExp (Just iSort) vId (BinOp lhs op (Lit $ Int 1))
   env@Env{..} <- get
   case lhs of
    ExpName (Name [ident]) -> do
     let lhsVar = getVarSSAMap "post_op_inner" vId ident _e_ssamap
     nLhsVar <- lift $ updateVariable vId ident lhsVar
     let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
     ass <- lift $ processAssign lhsVar nLhsVar EqualA rhsAst 
     pre <- lift $ mkAnd [_e_pre, ass]
     updatePre pre
     updateSSAMap ssamap
    _ -> error $ str ++ show _exp ++ " not supported"
