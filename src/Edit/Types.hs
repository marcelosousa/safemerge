{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Types
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit.Types where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Analysis.Java.AST

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T

type Edit = [BlockStmt] 
type Edits = [(Edit,Edit,Edit,Edit)]
type AnnEdit = [AnnBlockStmt] 
type AnnAnnEdits = [(AnnEdit,AnnEdit,AnnEdit,AnnEdit)]
type Method = ([FormalParam],Block) 

