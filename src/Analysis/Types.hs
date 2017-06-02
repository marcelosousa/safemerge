{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Types
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Types where

import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Analysis.Dependence
import Control.Monad.ST
import Control.Monad.State.Strict hiding (join)
import Data.Map (Map)
import Data.List
import Edit.Types
import Language.Java.Syntax
import Z3.Monad hiding (Params)
import qualified Data.Map as M
import qualified Debug.Trace as T

-- receives the parameters and returns to specify the pre and post-condition
-- need to use maps for the parameters, returns, fields
type Params = Map Ident [(AST,Sort)]
type Res  = [AST]
type Fields = Map Ident FuncDecl
type Prop = (Params, Res, Fields) -> Z3 (AST, AST)

-- SSA map to build a simple SSA representation on the fly
-- For each identifier, we need a copy per version
type SSAVar = Map Int (AST, Sort, Int)
type SSAMap = Map Ident SSAVar 

printSSAMap ::SSAMap -> String
printSSAMap = M.foldWithKey (\i m r -> show i ++ " -> " ++ show m ++ "\n" ++ r) "" 

update_ssamap :: Int -> Ident -> (AST, Sort, Int) -> SSAMap -> SSAMap
update_ssamap pid ident el ssamap =
  case M.lookup ident ssamap of
    Nothing -> M.insert ident (M.singleton pid el) ssamap
    Just l  -> let l' = M.insert pid el l
               in M.insert ident l' ssamap

get_ast :: String -> Int -> Ident -> SSAMap -> AST
get_ast err pid ident ssamap = 
  case M.lookup ident ssamap of
    Nothing -> error $ "get_ast: " ++ err
    Just l  -> case M.lookup pid l of
      Nothing -> error $ "get_ast pid: " ++ err
      Just (a,b,c) -> a

replace :: Int -> a -> [a] -> [a]
replace 0 a [] = [a] 
replace 0 a l  = a:(tail l)
replace i a [] = error "replace ..."
replace i a (h:hs) = h:(replace (i-1) a hs)

-- We need the assign map to understand the value of the loop counter
type AssignMap = Map Ident Exp

-- Function Map: Maps an abstract method signature (ident, arity) 
-- to the AST representation (shared by all versions) and the 
-- dependence graph of each version (currently abstracted to the join)
type FunctMap = Map AbsMethodSig (FuncDecl, DepMap) 

data Env = Env
  { 
    _ssamap  :: SSAMap
  , _fnmap   :: FunctMap  -- function map
  , _pre     :: AST
  , _post    :: AST
  , _invpost :: AST
  , _classes :: [ClassSum]
  , _edits   :: [AnnEdit]
  , _debug   :: Bool
  , _numret  :: Int
  , _pid     :: [Int] 
  , _anonym  :: Int       -- The number of anonymous functions
  }

type EnvOp a = StateT Env Z3 a

-- | Joinable fields are
--   _ssamap, functmap, pre, edits, numret, anonym
--   Pseudo code
--   1. Retrieve the part of the environments which have been modified
--   2. 

-- This function does the heavylifting in the join of the ssamap
--   It receives the original environemtn, one identifier that is 
--   shared in the new 2 environments and checks whether their 
--   keys is the same in which case it generates Nothing
--       otherwise it reports which versions need to updated
ssamap_mod :: SSAMap -> Ident -> SSAVar -> SSAVar -> [(Int,Int,Sort)] 
ssamap_mod orig m e1 e2 = 
  case M.lookup m orig of
    Nothing -> error $ "ssamap_mod: scoping issue for " ++ show m 
    Just v  -> 
      if e1 == v && e2 == v
      then [] 
      else M.foldWithKey (ssavar_mod e1 e2) [] v 

ssavar_mod :: SSAVar -> SSAVar -> Int -> (AST, Sort, Int) -> [(Int,Int,Sort)] -> [(Int,Int,Sort)]
ssavar_mod e1 e2 v (_,t,n) rest = 
  case (M.lookup v e1, M.lookup v e2) of
    (Just (_,_,n1), Just (_,_,n2)) ->
      if n1 == n && n2 == n
      then rest
      else (v,maximum [n,n1,n2] + 1,t):rest 
    _ -> error $ "ssavar_mod: version " ++ show v

enc_ident_single :: Int -> String -> Int -> Sort -> Z3 AST
enc_ident_single j str i sort = do 
  let nstr = str ++ "_" ++ show j ++ "_" ++ show i
  sym <- mkStringSymbol nstr
  ast <- mkVar sym sort
  return ast

to_ssavar :: SSAMap -> (Ident,[(Int,Int,Sort)]) -> EnvOp SSAMap 
to_ssavar r (Ident str,[]) = return r
to_ssavar r (Ident str,xs) = do
  el <- lift $ foldM (\var (v,i,ty) -> do
           ast <- enc_ident_single v str i ty
           return $ M.insert v (ast,ty,i) var) M.empty xs 
  return $ M.insert (Ident str) el r
   
partial_ssamap :: Map Ident [(Int,Int,Sort)] -> EnvOp SSAMap   
partial_ssamap m = do 
  let m' = M.toList m 
  foldM to_ssavar M.empty m'

join_pre :: SSAMap -> SSAMap -> SSAMap -> AST -> AST -> EnvOp AST
join_pre nmap m1 m2 pre1 pre2 = do
  let nm1  = M.intersectionWith (M.intersectionWith (\(a1,_,_) (a2,_,_) -> (a1,a2))) nmap m1 
      eqs1 = concatMap M.elems $  M.elems nm1
  r1 <- lift $ mapM (\(a,b) -> mkEq a b) eqs1
  fpre1 <- lift $ mkAnd (pre1:r1)  
  let nm2  = M.intersectionWith (M.intersectionWith (\(a1,_,_) (a2,_,_) -> (a1,a2))) nmap m2 
      eqs2 = concatMap M.elems $  M.elems nm2
  r2 <- lift $ mapM (\(a,b) -> mkEq a b) eqs2
  fpre2 <- lift $ mkAnd (pre2:r2)  
  lift $ mkOr [fpre1,fpre2] 

join_env :: Env -> Env -> Env -> EnvOp Env
join_env orig e1 e2 = do
  let vs = M.intersectionWithKey (ssamap_mod (_ssamap orig)) (_ssamap e1) (_ssamap e2) 
  new_ssa <- partial_ssamap vs 
  pre     <- join_pre new_ssa (_ssamap e1) (_ssamap e2) (_pre e1) (_pre e2)
  let ssa     = M.unionsWith M.union [new_ssa,_ssamap e1,_ssamap e2] 
      fnm     = _fnmap  e1 `M.union` _fnmap  e2
      post    = _post    e1
      invpost = _invpost e1
      classes = _classes e1
      eds     = _edits  e1 `intersect` _edits  e2
      debug   = _debug   e1
      numret  = if _numret e1 /= _numret e2 then error "join: numret" else _numret e1
      pid     = _pid     e1
      anonym  = _anonym e1 `max` _anonym e2
  return $ Env ssa fnm pre post invpost classes eds debug numret pid anonym 

_default = (Unsat, Nothing)

popEdits :: EnvOp [AnnBlockStmt]
popEdits = do
  s@Env{..} <- get
  let (res,edits) = unzip $ map (\e -> (head e, tail e)) _edits 
  put s { _edits = edits }
  return res 

-- @ update the pid
updatePid :: [Int] -> EnvOp ()
updatePid pid = do
  s@Env{..} <- get
  put s{ _pid = pid }

-- @ update the pre-condition
updatePre :: AST -> EnvOp ()
updatePre pre = do
  s@Env{..} <- get
  put s{ _pre = pre }

-- @ update the pre-condition
updatePost :: AST -> EnvOp ()
updatePost post = do
  s@Env{..} <- get
  put s{ _post = post}

updateInvPost :: AST -> EnvOp ()
updateInvPost invpost = do
  s@Env{..} <- get
  put s{ _invpost = invpost }

updateNumRet :: EnvOp ()
updateNumRet = do
  s@Env{..} <- get
  let numret = _numret + 1
  put s{ _numret = numret }
  
-- @ update the ssa map
updateSSAMap :: SSAMap -> EnvOp ()
updateSSAMap ssamap = do
  s@Env{..} <- get
  put s{ _ssamap = ssamap}

updateFunctMap :: FunctMap -> EnvOp ()
updateFunctMap fnmap = do
  s@Env{..} <- get
  put s{ _fnmap = fnmap }

incAnonym :: EnvOp Int
incAnonym = do
  s@Env{..} <- get
  let anonym = _anonym 
  put s{ _anonym =  anonym + 1}
  return anonym
