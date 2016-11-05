{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Calculus Rules 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Calculus where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T
import Edit.Types

type Pid = Int
type ProdProgram = [(Pid, [BlockStmt])]

miniproduct :: BlockStmt -> BlockStmt -> BlockStmt -> BlockStmt -> ProdProgram 
miniproduct = undefined 

-- reduce :: [Edit] -> Edit -> Edit 
-- reduce pp p =
--   let p' = miniproduct pp
--   in miniproduct [p',p]
-- 
-- seq :: Edit -> Edit -> Edit 
-- seq p1 p2 = p1 ++ p2 
-- 
-- loop :: Edit -> BlockStmt -> BlockStmt 
-- loop w1 w2 = undefined 
-- 
-- sequence :: [Edit] -> Edit
-- sequence es =
--   let (hs,ts) = unzip $ map (\e -> ([head e],tail e)) es
--   in miniproduct hs ++ miniproduct ts           
