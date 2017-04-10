{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Types
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Types where

import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Analysis.Dependence
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Map (Map)
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
type SSAMap = Map Ident (Map Int (AST, Sort, Int))

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
  , _assmap  :: AssignMap
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

updateAssignMap :: AssignMap -> EnvOp ()
updateAssignMap assmap = do
  s@Env{..} <- get
  put s{ _assmap = assmap }

incrementAssignMap :: Ident -> Exp -> EnvOp ()
incrementAssignMap i e = do
  s@Env{..} <- get
  let assignMap = M.insert i e _assmap
  put s{ _assmap = assignMap }
  
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
