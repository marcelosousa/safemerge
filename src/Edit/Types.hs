{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data Scope = SLoop | SCond 
  deriving (Eq,Ord,Show)
type SEdit = (BlockStmt,[Scope])
type Edit = [SEdit]
type Edits = [(Edit,Edit,Edit,Edit)]
type AnnEdit = [AnnBlockStmt] 
type AnnAnnEdits = [(AnnEdit,AnnEdit,AnnEdit,AnnEdit)]
type Method = ([FormalParam],Block) 

loop_scope :: Edit -> Bool
loop_scope = any (\(_,sc) -> any (==SLoop) sc) 

-- add each x to the ei
push :: Edit -> [Edit] -> [Edit]
push xs [] = map (:[]) xs
push xs eis =
  let a = zip xs eis
  in map (\(x,ei) -> ei++[x]) a

instance Annotate SEdit AnnBlockStmt where
  toAnn p (a,b) = toAnn p a
  fromAnn b = (fromAnn b, [])
