{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Engine
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Engine where

import Analysis.API
import Analysis.Java.ClassInfo
import Analysis.Java.Flow
import Analysis.Dependence
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.List
import Data.Map (Map)
import Data.Maybe
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe
import Util
import Z3.Monad hiding (Params)
import qualified Data.Map as M

-- |= pre => post 
-- UNSAT (not (pre => post))  
helper pre post = do
  formula <- mkImplies pre post >>= mkNot 
  assert formula
  (r, m)  <- getModel
  preStr  <- astToString pre
  -- trace ("helper: " ++ preStr) $ return (r,m)
  return (r,m)

-- \phi not models \psi
-- \phi |= \psi 
implies :: AST -> AST -> Z3 Bool
implies phi psi = do 
  formula <- mkImplies phi psi >>= mkNot 
  push 
  assert formula 
  res <- check
  pop 1
  -- phi_str <- astToString phi
  -- psi_str <- astToString psi
  -- liftIO $ putStrLn $ "implies:\n" ++ phi_str ++ "\n" ++ psi_str ++ "\nresult: = " ++ (show $ res == Unsat)
  -- _ <- liftIO $ getChar
  case res of
    Unsat -> return True
    _     -> return False 

-- encodeInputs :: generates the initial SSA map and pre-condition 
encodeInputs :: [MemberSig] -> Z3 (SSAMap,AST)
encodeInputs inp = do 
 (ssa,eqs) <- foldM encodeInput (M.empty,[]) inp 
 pre       <- if null eqs then mkTrue else mkAnd eqs
 return (ssa,pre)

encodeInput :: (SSAMap,[AST]) -> MemberSig -> Z3 (SSAMap,[AST])
encodeInput (ssa,pre) sig = do 
 (nssa,matrix) <- foldM (encodeIVar sig) (ssa,[]) [1..4] 
 let tmatrix = transpose matrix
 pres <- foldM (\k row -> mapM (uncurry mkEq) (lin row) >>= \eqs -> return (k ++ eqs)) [] tmatrix
 return (nssa,pre++pres)

encodeIVar :: MemberSig -> (SSAMap,[[AST]]) -> VId -> Z3 (SSAMap,[[AST]])
encodeIVar sig@(ident,tys) (ssa,matrix) vId = do 
 var <- encodeVariable vId sig 
 let row     = getASTSSAVar var
     nssa    = insertSSAVar vId ident var ssa 
     nmatrix = row:matrix
 return (nssa,nmatrix)

encodeVariable :: VId -> MemberSig -> Z3 SSAVar
encodeVariable vId (Ident id,[ty]) = do 
 let name = id ++ "_" ++ show vId ++ "_0"
 (sort,model) <- encodeType vId id ty
 ast <- mkFreshConst name sort
 let var = SSAVar ast sort 0 model
 return var
encodeVariable vId inv = error $ "encodeVariable: invalid input " ++ show inv

-- encode post-condition
encodePost :: SSAMap -> [MemberSig] -> Z3 (SSAMap, AST)
encodePost ssa fields = do 
 (nssa,conds) <- foldM encodePostVar (ssa,[]) fields 
 post <- mkAnd conds
 return (nssa,post)

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

encodeType :: VId -> String -> Type -> Z3 (Sort,SSAVarModel)
encodeType vId ident ty = case ty of 
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
  return (sort,M.empty)
 RefType rty -> case rty of
   ClassRefType cty@(ClassType l) -> 
     case l of 
       [(Ident "List",_)] -> do 
         intSort <- mkIntSort
         sort    <- mkArraySort intSort intSort
         return (sort,M.empty)
       [(Ident "ArrayList",_)] -> do 
         intSort <- mkIntSort
         sort    <- mkArraySort intSort intSort
         return (sort,M.empty)
       [(Ident "Queue",_)] -> do
         intSort <- mkIntSort
         sort    <- mkArraySort intSort intSort
         model   <- queueModel vId ident
         return (sort,model) 
       [(Ident l,_)] -> do
         sym  <- mkStringSymbol l
         sort <- mkUninterpretedSort sym
         return (sort,M.empty)
       _ -> error $ "enc_type: " ++ show cty 
   ArrayType    aty -> do
     (at,_)  <- encodeType vId ident aty
     intSort <- mkIntSort
     sort    <- mkArraySort intSort at 
     return (sort,M.empty)

queueModel :: VId -> String -> Z3 SSAVarModel
queueModel vId name = do 
 intSort <- mkIntSort
 let idx_start = name ++ "_idx_start_" ++  show vId ++ "_0"
 ast_start <- mkFreshConst idx_start intSort 
 let idx_end = name ++ "_idx_end_" ++  show vId ++ "_0"
 ast_end <- mkFreshConst idx_end intSort 
 return $ M.fromList [("idx_start",(ast_start,intSort,0))
                     ,("idx_end"  ,(ast_end  ,intSort,0))]

-- encode a variable
enc_var :: (String, Type) -> Z3 (AST,Sort)
enc_var (id,ty) = do
  tySort <- enc_type ty
  ast <- mkFreshConst id tySort
  return (ast,tySort)

enc_type :: Type -> Z3 Sort
enc_type ty = case ty of
  PrimType pty -> case pty of
    BooleanT -> mkBoolSort
    ByteT    -> mkBvSort 8
    ShortT   -> mkIntSort
    IntT     -> mkIntSort
    LongT    -> mkRealSort
    CharT    -> error "enc_ty: CharT"
    FloatT   -> mkRealSort
    DoubleT  -> mkRealSort
  RefType rty -> case rty of
    ClassRefType cty@(ClassType l) -> 
      case l of 
        [(Ident "List",_)] -> do 
          intSort <- mkIntSort
          mkArraySort intSort intSort
        [(Ident "ArrayList",_)] -> do 
          intSort <- mkIntSort
          mkArraySort intSort intSort
        [(Ident l,_)] -> do
          sym <- mkStringSymbol l
          mkUninterpretedSort sym
        _ -> error $ "enc_type: " ++ show cty 
    ArrayType    aty -> do
      at <- enc_type aty
      intSort <- mkIntSort
      mkArraySort intSort at

initial_FuncMap :: Z3 FunctMap
initial_FuncMap = do
  iSort <- mkIntSort
  iArray <- mkArraySort iSort iSort
  fn <- mkFreshFuncDecl "size" [iArray] iSort
  return $ M.singleton (Ident "size",1) (fn, M.empty)

-- Encoding functions 
processParam :: FormalParam -> Z3 Sort
processParam (FormalParam mods ty _ _) = processType ty 

processType :: Type -> Z3 Sort
processType (PrimType ty) =
  case ty of
    BooleanT -> mkBoolSort
    _ -> mkIntSort -- error $ "processType: " ++ show ty ++ " not supported"
processType (RefType (ClassRefType (ClassType [(Ident name,[])]))) = mkIntSort
--  do
--    sym <- mkStringSymbol name
--    mkUninterpretedSort sym
processType ty@(RefType _) = error $ "processType: not supported " ++ show ty

processAssign :: AST -> AssignOp -> AST -> AST -> Z3 AST
processAssign lhs op rhs plhs =
  case op of 
    EqualA -> mkEq lhs rhs
    AddA -> do
      rhs' <- mkAdd [plhs, rhs]
      mkEq lhs rhs'
    _ -> error $ "processAssign: " ++ show op ++ " not supported"

enc_ident :: [Int] -> String -> Int -> Sort -> Z3 [(Int, AST)]
enc_ident pids str i sort = 
  mapM (\j -> do
    let nstr = str ++ "_" ++ show j ++ "_" ++ show i
    sym <- mkStringSymbol nstr
    ast <- mkVar sym sort
    return (j,ast)) pids

-- encode the first variable definition 
enc_new_var :: [Int] -> Sort -> Int -> VarDecl -> EnvOp ()
enc_new_var pids sort i (VarDecl varid mvarinit) = do
  undefined
{-
  env@Env{..} <- get
  (ident, idAsts) <- lift $ 
    case varid of
      VarId ident@(Ident str) -> do
        vars <- enc_ident pids str i sort
        return (ident, vars)
      _ -> error $ "enc_new_var: not supported " ++ show varid
  let nssamap = 
       foldr (\(_pid,idAst) r -> update_ssamap _pid ident (idAst, sort, i) r) _ssamap idAsts
  updateSSAMap nssamap
  case mvarinit of
    Nothing -> return ()
    Just (InitExp expr) -> do
      expAsts <- enc_exp pids expr
      let id_exp = zip idAsts expAsts
      eqIdExps <- lift $ mapM (\((_,idAst),expAst) -> mkEq idAst expAst) id_exp
      npre <- lift $ mkAnd (_pre:eqIdExps)
      updatePre npre 
    Just _ -> error "enc_new_var: not supported"
-}

-- enc_exp: encodes an expression for a version 
encodeExp :: [VId] -> Exp -> EnvOp [AST]
encodeExp vIds expr = mapM (\p -> enc_exp_inner p expr) vIds

getASTExp :: VId -> Exp -> EnvOp [AST]
getASTExp vId expr = do 
 env@Env{..} <- get
 case expr of
  ExpName name -> do
    let ident = toIdent name
    return $ getASTSSAMap "enc_exp_inner" vId ident _e_ssamap
  _ -> do
    ast <- enc_exp_inner vId expr
    return [ast]

-- | Encode an expression for a version 
--   (ast,sort,count)pre-condition: pid != 0 
enc_exp_inner :: VId -> Exp -> EnvOp AST
enc_exp_inner vId expr = do 
 env@Env{..} <- get
 case expr of
  Lit lit -> lift $ enc_literal lit 
  ExpName name -> do
    let ident = toIdent name
        asts  = getASTSSAMap "enc_exp_inner" vId ident _e_ssamap
    return $ head asts
  BinOp lhsE op rhsE -> do
    lhs <- enc_exp_inner vId lhsE
    rhs <- enc_exp_inner vId rhsE
    lift $ enc_binop op lhs rhs
  PreMinus nexpr -> do 
    nexprEnc <- enc_exp_inner vId nexpr
    lift $ mkUnaryMinus nexprEnc
  Cond cond _then _else -> do
    condEnc  <- enc_exp_inner vId cond
    _thenEnc <- enc_exp_inner vId _then
    _elseEnc <- enc_exp_inner vId _else
    lift $ mkIte condEnc _thenEnc _elseEnc        
  PreNot nexpr -> do
    nexprEnc <- enc_exp_inner vId nexpr
    lift $ mkNot nexprEnc
  ArrayAccess ai -> enc_array_access vId ai
  FieldAccess fa -> do
    asts <- enc_field_access vId fa
    return $ head asts
  MethodInv m -> enc_meth_inv vId m
  _ -> error $  "enc_exp_inner: " ++ show expr

enc_meth_inv :: Int -> MethodInvocation -> EnvOp AST
enc_meth_inv p m = case m of  
  MethodCall name args -> do
    argsAST <- mapM (enc_exp_inner p) args
    enc_meth p (toIdent name) argsAST
  PrimaryMethodCall e tys mName args ->
    case mName of
      -- convert get into an array access
      Ident "get" -> enc_array_access p $ ArrayIndex e args 
      _ -> error $ "enc_meth_inv: " ++ show m
  _ -> error $ "enc_meth_inv: " ++ show m

enc_array_access :: Int -> ArrayIndex -> EnvOp AST
enc_array_access p exp@(ArrayIndex e args) = do
  a <- enc_exp_inner p e 
  case args of
    [x] -> do
      i <- enc_exp_inner p x
      lift $ mkSelect a i
    [x,y] -> do
      i <- enc_exp_inner p x
      j <- enc_exp_inner p y
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
    
enc_literal :: Literal -> Z3 AST
enc_literal lit =
  case lit of
    Boolean True -> mkTrue
    Boolean False -> mkFalse
    Int i -> mkIntNum i
    Null -> mkIntNum 0 -- case M.lookup (Ident "null") ssamap of
--    Nothing -> error "processLit: null not found"
--    Just (ast, _, _) -> return ast
    _ -> error "processLit: not supported"


-- | Hooks the dependence analysis 
enc_meth :: Int -> Ident -> [AST] -> EnvOp AST
enc_meth pid id@(Ident ident) args = do
  env@Env{..} <- get
  let arity = length args
      class_sum = _e_classes !! (pid - 1) 
      meths = findMethodGen id arity class_sum
      cfgs = map computeGraphMember meths
      deps = foldr (\cfg res -> M.union res $ blockDep class_sum cfg) M.empty cfgs
  case M.lookup (id,arity) _e_fnmap of
    Nothing -> do
      sorts <- lift $ mapM getSort args 
      iSort <- lift $ mkIntSort
      fn <- lift $ mkFreshFuncDecl ident sorts iSort
      ast <- lift $ mkApp fn args
      let fnmap = M.insert (id,arity) (fn,deps) _e_fnmap 
      updateFunctMap fnmap 
      return ast 
    Just (ast,dep) -> lift $ mkApp ast args 

enc_meth_special :: Int -> Ident -> Sort -> [AST] -> EnvOp AST
enc_meth_special pid id@(Ident ident) sort args = do
  env@Env{..} <- get
  let arity = length args
  sorts <- lift $ mapM getSort args 
  fn <- lift $ mkFreshFuncDecl ident sorts sort 
  ast <- lift $ mkApp fn args
  let fnmap = M.insert (id,arity) (fn,M.empty) _e_fnmap 
  updateFunctMap fnmap 
  return ast 

{-
processName env@(objSort, pars, res, fields, ssamap) (Name [ident]) args = do
  let fn = safeLookup ("processName: declared func")  ident fields
  mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [Ident "Character",fnName]) args = do
  let fn = safeLookup ("processName: Field" ++ show fnName)  fnName fields
  mkApp fn args
processName env@(objSort, pars, res, fields, ssamap) (Name [obj,field]) args = do
  let par = safeLookup ("processName: Object" ++ show obj) obj pars
      fn = safeLookup ("processName: Field" ++ show field)  field fields
  error "processName: TODO"
  -- mkApp fn (par:args)
processName env name args = error $  "processName: corner case" ++ show name
-}

enc_binop :: Op -> AST -> AST -> Z3 AST
enc_binop op lhs rhs = do 
  case op of
    NotEq -> mkEq lhs rhs >>= \eq -> mkNot eq
    And -> mkAnd [lhs,rhs]
    Add -> mkAdd [lhs,rhs]
    Mult -> mkMul [lhs,rhs]
    Sub -> mkSub [lhs,rhs]
    LThan -> mkLt lhs rhs
    LThanE -> mkLe lhs rhs
    GThan -> mkGt lhs rhs
    GThanE -> mkGe lhs rhs
    Equal -> mkEq lhs rhs
    COr -> mkOr [lhs, rhs]
    CAnd -> mkAnd [lhs, rhs]
    _ -> error $ "processBinOp: not supported " ++ show op

-- SMT Utility Functions
mkAttribute :: Sort -> Fields -> MemberDecl -> Z3 Fields
mkAttribute objSort m mDecl =
  case mDecl of
    FieldDecl  mods ty vardecls -> do
      retSort <- processType ty
      foldM (\nm vardecl -> mkField nm vardecl objSort retSort) m vardecls 
    MethodDecl mods ty (Just rty) (Ident name) pars exTy (MethodBody Nothing) -> do
      retSort <- processType rty
      i <- mkIntSort
      parsSort <- mapM processParam pars
      fn <- mkFreshFuncDecl name (objSort:parsSort) retSort
      return $ M.insert (Ident name) fn m

mkObjectSort :: String -> Z3 Sort
mkObjectSort str = do
  myint <- mkStringSymbol str
  mkUninterpretedSort myint

mkField :: Fields -> VarDecl -> Sort -> Sort -> Z3 Fields
mkField m (VarDecl (VarId (Ident name)) Nothing) parSort retSort = do
  fn <- mkFreshFuncDecl name [parSort] retSort
  return $ M.insert (Ident name) fn m

replaceVariable :: String -> FuncDecl -> AST -> Z3 AST
replaceVariable a fnB ast = do
  kind <- getAstKind ast
  case kind of
    Z3_NUMERAL_AST    -> return ast
    Z3_APP_AST        -> do
      app <- toApp ast
      fn <- getAppDecl app
      sym <- getDeclName fn >>= getSymbolString
      if sym == a
      then do
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        args' <- mapM (replaceVariable a fnB) args
        mkApp fnB args' --trace ("FN " ++ symName) $ mkApp fn args'
      else do 
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        args' <- mapM (replaceVariable a fnB) args
        mkApp fn args' --trace ("FN " ++ symName) $ mkApp fn args'
    Z3_VAR_AST        -> return ast
    Z3_QUANTIFIER_AST -> return ast --error "traverse"
    Z3_SORT_AST       -> return ast
    Z3_FUNC_DECL_AST  -> return ast
    Z3_UNKNOWN_AST    -> return ast

-- Analyse Post De/Increment
-- @NOTE: April'17: These expressions were optimised?
post_op :: [VId] -> Exp -> Exp -> Op -> String -> EnvOp ()
post_op vIds _exp lhs op str = mapM_ (\p -> post_op_inner p _exp lhs op str) vIds
 where
  post_op_inner :: VId -> Exp -> Exp -> Op -> String -> EnvOp ()
  post_op_inner vId _exp lhs op str = do
   rhsAst <- enc_exp_inner vId (BinOp lhs op (Lit $ Int 1))
   env@Env{..} <- get
   case lhs of
    ExpName (Name [ident@(Ident str)]) -> do
     let var@SSAVar{..} = getVarSSAMap "post_op_inner" vId ident _e_ssamap
         cstr = str ++ "_" ++ show vId ++ "_" ++ show _v_cnt
         ni   = _v_cnt+1
         nstr = str ++ "_" ++ show vId ++ "_" ++ show ni
     sym <- lift $ mkStringSymbol nstr
     var <- lift $ mkFreshFuncDecl nstr [] _v_typ
     astVar <- lift $ mkApp var []
     let nvar   = SSAVar astVar _v_typ ni _v_mod 
         ssamap = insertSSAVar vId ident nvar _e_ssamap
     ass <- lift $ processAssign astVar EqualA rhsAst _v_ast 
     pre <- lift $ mkAnd [_e_pre, ass]
     updatePre pre
     updateSSAMap ssamap
    _ -> error $ str ++ show _exp ++ " not supported"
