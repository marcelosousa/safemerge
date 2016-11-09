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

-- | If any of them are ifs apply the if rule
--   If all of them are whiles apply the while rule
--   Otherwise, consume and move on
miniproduct :: [(Pid, BlockStmt)] -> ProdProgram 
miniproduct stmts = map (\(pid, s) -> (pid, [s])) stmts 
  
is_if :: BlockStmt -> Bool
is_if b = case b of
  BlockStmt s -> case s of
    IfThen _ _ -> True 
    IfThenElse _ _ _ -> True 
    _ -> False 
  _ -> False 

has_loop :: BlockStmt -> Bool
has_loop = undefined 

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
