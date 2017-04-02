{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Engine
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Engine where

import Analysis.Java.ClassInfo
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe
import Z3.Monad hiding (Params)
import qualified Data.Map as M
import qualified Debug.Trace as T

trace a b = b
--trace = T.trace

-- Performs a SAT-query.
checkSAT phi = do
  push 
  assert phi
  res <- check
  pop 1
  return res
  
helper pre post = do
  formula <- mkImplies pre post >>= \phi -> mkNot phi 
  assert formula
  (r, m) <- getModel
  preStr  <- astToString pre
  T.trace ("helper: " ++ preStr) $ return (r,m)

-- z3_gen_inout :: generates the input and output vars
z3_gen_inout :: ([MemberSig], [MemberSig]) -> Z3 (Params, [[AST]], [[AST]])
z3_gen_inout (params, fields) = do
  let arity = 4
      fieldsId = map (toString .fst) fields
      inFields = map (\s -> map (\ar -> s ++ show ar) [1..arity]) fieldsId
      outFields = map (\s -> map (\ar -> "ret_"++s++show ar) [1..arity]) fieldsId
  intSort <- mkIntSort
  inFieldsZ3  <- mapM (mapM (\inp -> mkFreshConst inp intSort)) inFields 
  outFieldsZ3 <- mapM (mapM (\inp -> mkFreshConst inp intSort)) outFields 
  let inoutFields = zip inFieldsZ3 outFieldsZ3 
      pFields = foldl (\r (k,v) -> M.insert (Ident k) v r) M.empty $ zip fieldsId inoutFields
  -- taking care of input and return
      inputSig = map (toString . fst) params 
      inputs = map (\s -> map (\ar -> s ++ show ar) [1..arity]) inputSig 
      returns = map (\ar -> "ret" ++ show ar) [1..arity] 
  inZ3  <- mapM (mapM (\inp -> mkFreshConst inp intSort)) inputs 
  outZ3 <- mapM (\out -> mkFreshConst out intSort) returns 
  let pInput = foldl (\r (k,v) -> M.insert (Ident k) (v,[]) r) pFields $ zip inputSig inZ3
      pInOut = M.insert (Ident "ret") ([],outZ3) pInput
  return (pInOut, inFieldsZ3 ++ inZ3, outZ3:outFieldsZ3)

-- Generates the initial SSA Map for the fields and the parameters
initial_SSAMap :: Params -> Z3 SSAMap
initial_SSAMap params = do
  iSort <- mkIntSort
  fn <- mkFreshFuncDecl "null" [] iSort
  ast <- mkApp fn []
  let i = M.singleton (Ident "null") (replicate 4 (ast, iSort, 0))
      ps = M.map (\(a,_) -> [(e, iSort, 0) | e <- a]) params
  return $ M.union i ps

-- Verification pre-condition
initial_precond :: [[AST]] -> Z3 AST 
initial_precond inputs = do 
  let inputs' = map (\x -> [(e,e') | e <- x, e' <- x]) inputs
  eqs <- mapM (\p -> mapM (\(e,e') -> mkEq e e') p) inputs'
  let _eqs = concat eqs
  if null _eqs
  then mkTrue
  else mkAnd _eqs

postcond :: [[AST]] -> Z3 AST
postcond outs = do
  cond <- mapM postcond' outs 
  mkAnd cond 

postcond' :: [AST] -> Z3 AST
postcond' res = case res of
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

enc_ident :: String -> Int -> Sort -> Z3 [AST]
enc_ident str i sort = mapM (\j -> do
  let nstr = str ++ "_" ++ show j ++ "_" ++ show i
  sym <- mkStringSymbol nstr
  mkVar sym sort) [1..4]

-- encode the first variable definition 
enc_new_var :: Int -> Sort -> Int -> VarDecl -> EnvOp ()
enc_new_var pid sort i (VarDecl varid mvarinit) = do
  env@Env{..} <- get
  (ident, idAsts) <- lift $ 
    case varid of
      VarId ident@(Ident str) -> do
        vars <- enc_ident str i sort
        return (ident, vars)
      _ -> error $ "enc_new_var: not supported " ++ show varid
  let nssamap = M.insert ident [(idAst, sort, i) | idAst <- idAsts] _ssamap
  updateSSAMap nssamap
  case mvarinit of
    Nothing -> return ()
    Just (InitExp expr) -> do
      expAsts <- enc_exp pid expr
      let id_exp = zip idAsts expAsts
      eqIdExps <- lift $ mapM (\(idAst,expAst) -> mkEq idAst expAst) id_exp
      let nassmap = M.insert ident expr _assmap
      updateAssignMap nassmap
      npre <- lift $ mkAnd (_pre:eqIdExps)
      updatePre npre 
    Just _ -> error "enc_new_var: not supported"

-- enc_exp: encodes an expression for a version 
enc_exp :: Int -> Exp -> EnvOp [AST]
enc_exp p expr = do
  let l = if p == 0 then [1..4] else [p]
  mapM (\p -> enc_exp_inner p expr) l

-- | Encode an expression for a version 
--   (ast,sort,count)pre-condition: pid != 0 
enc_exp_inner :: Int -> Exp -> EnvOp AST
enc_exp_inner p expr = do
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
  MethodInv (MethodCall name args) -> do
    argsAST <- mapM (enc_exp_inner p) args
    enc_name p (toIdent name) argsAST
  Cond cond _then _else -> do
    condEnc <- enc_exp_inner p cond
    _thenEnc <- enc_exp_inner p _then
    _elseEnc <- enc_exp_inner p _else
    lift $ mkIte condEnc _thenEnc _elseEnc        
  PreNot nexpr -> do
    nexprEnc <- enc_exp_inner p nexpr
    lift $ mkNot nexprEnc
  _ -> error $  "enc_exp_inner: " ++ show expr

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

toIdent :: Name -> Ident
toIdent (Name []) = error $ "nameToIdent: Name []"
toIdent (Name l) = foldr (\(Ident a) (Ident b) -> Ident (a ++ "." ++ b)) (Ident "") l 

enc_name :: Int -> Ident -> [AST] -> EnvOp AST
enc_name pid id@(Ident ident) [] = do
  env@Env{..} <- get
  case M.lookup id _ssamap of
    Nothing -> do
     iSort <- lift $ mkIntSort
     fn <- lift $ mkFreshFuncDecl ident [] iSort
     ast <- lift $ mkApp fn []
     let ssamap = M.insert id [(ast, iSort, n) | n <- [1..4]] _ssamap
     updateSSAMap ssamap
     return ast 
    --  error $ "enc_name: not in map " ++ show ident 
    Just l -> case l !! (pid-1) of
      (ast,_,_) -> return ast
enc_name pid ident args = error "enc_name: not supported yet"
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
        mkApp fnB args' --T.trace ("FN " ++ symName) $ mkApp fn args'
      else do 
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        args' <- mapM (replaceVariable a fnB) args
        mkApp fn args' --T.trace ("FN " ++ symName) $ mkApp fn args'
    Z3_VAR_AST        -> return ast
    Z3_QUANTIFIER_AST -> return ast --error "traverse"
    Z3_SORT_AST       -> return ast
    Z3_FUNC_DECL_AST  -> return ast
    Z3_UNKNOWN_AST    -> return ast

