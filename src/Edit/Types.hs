{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Types
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit.Types where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T

type Edits = [(BlockStmt,BlockStmt,BlockStmt,BlockStmt)]
type Edit = [BlockStmt] 
type Program = CompilationUnit
type Method = ([FormalParam], Block) 
