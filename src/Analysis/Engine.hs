{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Engine
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Engine where

import Analysis.Java.ClassInfo
import Analysis.Java.Flow
import Analysis.Dependence
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe
import Util
import Z3.Monad hiding (Params)
import qualified Data.Map as M


-- Performs a SAT-query.
checkSAT phi = local (assert phi >> check) 

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
  case res of
    Unsat -> return True
    _     -> return False 

-- z3_gen_inout :: generates the input and output vars
z3_gen_inout :: ([MemberSig], [MemberSig]) -> Z3 (Params, [[(AST,Sort)]], [[(AST,Sort)]])
z3_gen_inout (params, fields) = do
  -- encode fields
  let arity = 4
      fieldsSig = map (\(id,tys) -> (toString id,check_types tys)) fields
      fieldsId  = map fst fieldsSig
      inFields  = map (\(s,tys) -> map (\ar -> (s ++ "_" ++ show ar,tys)) [1..arity]) fieldsSig
      outFields = map (\(s,tys) -> map (\ar -> ("ret_"++s++show ar,tys))  [1..arity]) fieldsSig
  inFieldsZ3  <- mapM (mapM enc_var) inFields 
  outFieldsZ3 <- mapM (mapM enc_var) outFields 
  let pFieldsIn = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip fieldsId inFieldsZ3
      pFields   = foldl (\r (k,v) -> M.insert (Ident $ "ret_" ++ k) v r) pFieldsIn $ zip fieldsId outFieldsZ3 
  -- encode input parameters 
      inputsSig = map (\(id,tys) -> (toString id,check_types tys)) params 
      inputsId  = map fst inputsSig 
      inputs    = map (\(s,tys) -> map (\ar -> (s ++ "_" ++ show ar ++ "_0",tys)) [1..arity]) inputsSig 
  inZ3  <- mapM (mapM enc_var) inputs 
  -- encode return variable with default type int
  intSort <- mkIntSort
  let returns = map (\ar -> "ret" ++ show ar) [1..arity] 
  outZ3 <- mapM (\out -> mkFreshConst out intSort >>= \ast -> return (ast, intSort)) returns 
  -- tie up 
  let pInput = foldl (\r (k,v) -> M.insert (Ident k) v r) pFields $ zip inputsId inZ3
      pInOut = M.insert (Ident "ret") outZ3 pInput
  return (pInOut, inFieldsZ3 ++ inZ3, outZ3:outFieldsZ3)
 where
   check_types :: [Type] -> Type
   check_types [] = error "z3_gen_inout: no type for the variable"
   check_types [t] = t
   check_types _  = error "z3_gen:inout: more than one type for the variable" 

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

-- Generates the initial SSA Map for the fields and the parameters
initial_SSAMap :: Params -> Z3 SSAMap
initial_SSAMap params = do
  iSort <- mkIntSort
  fn <- mkFreshFuncDecl "null" [] iSort
  ast <- mkApp fn []
  let i = M.singleton (Ident "null") (M.fromList $ zip [1..4] (replicate 4 (ast, iSort, 0)))
      fn l = zip [1..4] $ map (\(e,s) -> (e,s,0)) l 
      ps = M.map (\a -> M.fromList $ fn a) params
  return $ M.union i ps

-- Verification pre-condition
initial_precond :: [[(AST,Sort)]] -> Z3 AST 
initial_precond inputs =  do 
  let l = map lin inputs
  eqs <- mapM (\inp -> mapM (\((a,_),(b,_)) -> mkEq a b) $ lin inp) inputs
  let _eqs = concat eqs
  if null _eqs
  then mkTrue
  else mkAnd _eqs

-- computes a triangle of equalities
comb :: [a] -> [(a,a)]
comb [] = []
comb (x:xs) = [(x,y) | y <- xs] ++ comb xs

lin :: [a] -> [(a,a)]
lin [] = []
lin (x:xs) = lin' x xs 
 where
  lin' :: a -> [a] -> [(a,a)]
  lin' l [] = []
  lin' l (x:xs) = (l,x):lin' x xs

postcond :: [[(AST,Sort)]] -> Z3 AST
postcond outs = do
  cond <- mapM postcond' outs 
  mkAnd cond 

postcond' :: [(AST,Sort)] -> Z3 AST
postcond' res = case map fst res of
  [r_o, r_a, r_b, r_m] -> do 
    oa <- mkEq r_o r_a
    noa <- mkNot oa
    ob <- mkEq r_o r_b
    nob <- mkNot ob
    om <- mkEq r_o r_m
    ma <- mkEq r_m r_a
    mb <- mkEq r_m r_b
    c1 <- mkImplies noa ma
    c2 <- mkImplies nob mb
    c3 <- mkAnd [ma,mb,om]
    c4 <- mkAnd [c1,c2]
    mkOr [c3,c4]    
  _ -> error "postcond: invalid input" 

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

-- enc_exp: encodes an expression for a version 
enc_exp :: [Int] -> Exp -> EnvOp [AST]
enc_exp pids expr = mapM (\p -> enc_exp_inner p expr) pids

-- | Encode an expression for a version 
--   (ast,sort,count)pre-condition: pid != 0 
enc_exp_inner :: Int -> Exp -> EnvOp AST
enc_exp_inner p expr = do -- trace ("enc_exp_inner: " ++ show expr) $ do
 case expr of
  Lit lit -> lift $ enc_literal lit 
  ExpName name -> enc_name p (toIdent name) []
  BinOp lhsE op rhsE -> do
    lhs <- enc_exp_inner p lhsE
    rhs <- enc_exp_inner p rhsE
    lift $ enc_binop op lhs rhs
  PreMinus nexpr -> do 
    nexprEnc <- enc_exp_inner p nexpr
    lift $ mkUnaryMinus nexprEnc
  Cond cond _then _else -> do
    condEnc <- enc_exp_inner p cond
    _thenEnc <- enc_exp_inner p _then
    _elseEnc <- enc_exp_inner p _else
    lift $ mkIte condEnc _thenEnc _elseEnc        
  PreNot nexpr -> do
    nexprEnc <- enc_exp_inner p nexpr
    lift $ mkNot nexprEnc
  ArrayAccess ai -> enc_array_access p ai
  FieldAccess fa -> enc_field_access p fa
  MethodInv m -> enc_meth_inv p m
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

enc_field_access :: Int -> FieldAccess -> EnvOp AST
enc_field_access p exp = do
  case exp of
    PrimaryFieldAccess This (ident@(Ident str)) -> do
      env@Env{..} <- get
      case M.lookup ident _ssamap of
        Nothing -> error $ "enc_field_access: " ++ show exp 
        Just l -> case M.lookup p l of
          Nothing -> error $ "enc_field_access: " ++ show exp 
          Just (ast,_,_) -> return ast 
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

-- can the pid be 0?
enc_name :: Int -> Ident -> [AST] -> EnvOp AST
enc_name 0 _ _ = error $ "enc_name: pid = 0? debug"
enc_name pid id@(Ident ident) [] = do
  env@Env{..} <- get
  case M.lookup id _ssamap of
    Nothing -> error $ "enc_name: id not in scope " ++ ident ++ " " ++ show (M.keys _ssamap) 
    Just l -> case M.lookup pid l of
        Nothing -> error "enc_name: can this happen?" 
        Just (ast,_,_) -> return ast
enc_name pid ident args = error "enc_name: not supported yet"

-- | Hooks the dependence analysis 
enc_meth :: Int -> Ident -> [AST] -> EnvOp AST
enc_meth pid id@(Ident ident) args = do
  env@Env{..} <- get
  let arity = length args
      class_sum = _classes !! (pid - 1) 
      meths = findMethodGen id arity class_sum
      cfgs = map computeGraphMember meths
      deps = foldr (\cfg res -> M.union res $ blockDep class_sum cfg) M.empty cfgs
  case M.lookup (id,arity) _fnmap of
    Nothing -> do
      sorts <- lift $ mapM getSort args 
      iSort <- lift $ mkIntSort
      fn <- lift $ mkFreshFuncDecl ident sorts iSort
      ast <- lift $ mkApp fn args
      let fnmap = M.insert (id,arity) (fn,deps) _fnmap 
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
  let fnmap = M.insert (id,arity) (fn,M.empty) _fnmap 
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

