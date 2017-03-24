{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Types
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Types where

import Edit.Types
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Map (Map)
import Language.Java.Syntax
import Z3.Monad hiding (Params)

import qualified Data.Map as M
import qualified Debug.Trace as T

-- receives the parameters and returns to specify the pre and post-condition
-- need to use maps for the parameters, returns, fields
type Params = Map Ident [AST]
type Res  = [AST]
type Fields = Map Ident FuncDecl
type Prop = (Params, Res, Fields) -> Z3 (AST, AST)

-- SSA map to build a simple SSA representation on the fly
-- For each identifier, we need a copy per version
type SSAMap = Map Ident [(AST, Sort, Int)]

update_ssamap :: Int -> Ident -> (AST, Sort, Int) -> SSAMap -> SSAMap
update_ssamap pid ident el ssamap =
  case M.lookup ident ssamap of
    Nothing -> error $ "update_ssamap: cant find key " ++ show ident
    Just l  -> let l' = replace (pid-1) el l
               in M.insert ident l' ssamap

replace :: Int -> a -> [a] -> [a]
replace 0 a [] = [a] 
replace 0 a l  = a:(tail l)
replace i a [] = error "replace ..."
replace i a (h:hs) = h:(replace (i-1) a hs)

-- We need the assign map to understand the value of the loop counter
type AssignMap = Map Ident Exp

data Env = Env
  { 
    _ssamap  :: SSAMap
  , _assmap  :: AssignMap
  , _pre     :: AST
  , _post    :: AST
  , _invpost :: AST
  , _e_o     :: Edit
  , _e_a     :: Edit
  , _e_b     :: Edit
  , _e_m     :: Edit
  , _debug   :: Bool
  , _numret  :: Int
  , _pid     :: Int -- If the pid is 0, we are executing all versions 
  }

type EnvOp a = StateT Env Z3 a

_default = (Unsat, Nothing)

popEdits :: EnvOp (BlockStmt, BlockStmt, BlockStmt, BlockStmt)
popEdits = do
  s_o <- popEdit 1
  s_a <- popEdit 2
  s_b <- popEdit 3
  s_m <- popEdit 4
  return (s_o, s_a, s_b, s_m)

popEdit :: Int -> EnvOp BlockStmt
popEdit i = do
  s@Env{..} <- get
  case i of 
    1 -> case _e_o of
      [] -> error "popEdit: empty edit"
      (h:t) -> do
        put s{ _e_o = t}
        return h 
    2 -> case _e_a of
      [] -> error "popEdit: empty edit"
      (h:t) -> do
        put s{ _e_a = t}
        return h 
    3 -> case _e_b of
      [] -> error "popEdit: empty edit"
      (h:t) -> do
        put s{ _e_b = t}
        return h 
    4 -> case _e_m of
      [] -> error "popEdit: empty edit"
      (h:t) -> do
        put s{ _e_m = t}
        return h 

-- @ update the pid
updatePid :: Int -> EnvOp ()
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
  
