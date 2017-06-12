{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Types
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Types where

import Analysis.Java.ClassInfo
import Analysis.Dependence
import Control.Monad.State.Strict hiding (join)
import Data.Map (Map)
import Edit.Types
import Language.Java.Syntax
import Z3.Monad hiding (Params)

-- receives the parameters and returns to specify the pre and post-condition
-- need to use maps for the parameters, returns, fields
type Params = Map Ident [(AST,Sort)]
type Res    = [AST]
type Fields = Map Ident FuncDecl
type Prop   = (Params, Res, Fields) -> Z3 (AST, AST)
-- VId - Version Identifier
type VId    = Int 

-- SSA map to build a simple SSA representation on the fly
-- | Model of a SSA Variable
type SSAVarModel = Map String (AST,Sort,Int) 

-- | The SSA Variable type
data SSAVar = SSAVar 
  {
    _v_ast :: AST
  , _v_typ :: Sort
  , _v_cnt :: Int
  -- modelling variables
  , _v_mod :: SSAVarModel 
  , _v_mty :: VarType
  }
  deriving (Eq,Ord,Show)

data VarType = Primitive | Array | Queue | Object
  deriving (Eq,Ord,Show)

-- | The copy per version is just a map from version_id to variable 
type SSAVer = Map VId   SSAVar 

-- | Finally,for each identifier, we need a copy per version
type SSAMap = Map Ident SSAVer 

-- We need the assign map to understand the value of the loop counter
-- This is not used at the moment
-- type AssignMap = Map Ident Exp

-- Function Map: Maps an abstract method signature (ident, arity) 
-- to the AST representation (shared by all versions) and the 
-- dependence graph of each version (currently abstracted to the join)
type FunctMap = Map AbsMethodSig (FuncDecl, DepMap) 

data Env = Env
  { 
    _e_ssamap  :: SSAMap
  , _e_fnmap   :: FunctMap  -- function map
  , _e_pre     :: AST
  , _e_classes :: [ClassSum]
  , _e_edits   :: [AnnEdit]
  , _e_debug   :: Bool
  , _e_numret  :: Int
  , _e_vids    :: [VId] 
  , _e_anonym  :: Int       -- The number of anonymous functions
  }

type EnvOp a = StateT Env Z3 a
