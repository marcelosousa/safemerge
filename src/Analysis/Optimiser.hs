{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Optimiser
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Uses the dependence analysis to annotate 
-- blocks of statements in program with holes
-- with the read & write sets that will be used
-- in the encoding to model these with uninterpreted functions 
-------------------------------------------------------------------------------
module Analysis.Optimiser where

import Analysis.Dependence
import Analysis.Java.ClassInfo
import Analysis.Java.Liff
import Control.Monad.State.Lazy
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Debug.Trace as T
import Edit.Types
import Edit
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Data.Set as S


-- 1. Discover the largest non-hole sequence of blockstmts  
next_block :: [BlockStmt] -> (Either [BlockStmt] BlockStmt, [BlockStmt])
next_block [] = (Left [], [])
next_block (b:bs) =
  let (res,bs') = next_block bs
      (mLeft, mRight) = split_hole b
  in case split_hole b of
    (Nothing, Nothing) -> error $ "next_block: split_hole returned Nothing Nothing"
    (Nothing, Just b') -> (Right b, bs)
    (Just b', Nothing) -> -- no hole 
      case res of
        Left r  -> (Left (b:r), bs')
        Right _ -> (Left [b], bs)
    (Just bl, Just br) -> -- hole in the middle
      (Left [bl], br:bs)

-- 2. Checks if a blockstmt has a hole and splits it 
--                         (Maybe PartWithNoHole, Maybe PartWithHole)
split_hole :: BlockStmt -> (Maybe BlockStmt, Maybe BlockStmt) 
split_hole bstmt = case bstmt of
  BlockStmt s -> case s of 
    StmtBlock (Block b) -> case next_block b of
      (Left _, []) -> (Just bstmt, Nothing)
      (Left b',bs) -> (Just $ toBlockStmt b', Just $ toBlockStmt bs)
      (Right s, _) -> (Nothing, Just bstmt)
    _ -> if hasHole s
         then (Nothing, Just bstmt)
         else (Just bstmt, Nothing)
  -- Not a BlockStmt
  _ -> (Just bstmt, Nothing) 

toBlockStmt :: [BlockStmt] -> BlockStmt
toBlockStmt = BlockStmt . StmtBlock . Block

hasHoleBlock :: Block -> Bool
hasHoleBlock (Block b) = case next_block b of
  (Left _, []) -> False 
  _ -> True

hasHoleSwitchBlock :: SwitchBlock -> Bool
hasHoleSwitchBlock (SwitchBlock _ b) = hasHoleBlock (Block b)

hasHoleCatch :: Catch -> Bool
hasHoleCatch (Catch _ b) = hasHoleBlock b

hasHole :: Stmt -> Bool
hasHole s = case s of
  StmtBlock b             -> hasHoleBlock b 
  IfThen _ stmt           -> hasHole stmt
  IfThenElse _ t e        -> hasHole t || hasHole e
  While _ bdy             -> hasHole bdy
  BasicFor _ _ _ bdy      -> hasHole bdy 
  EnhancedFor _ _ _ _ bdy -> hasHole bdy 
  Switch _ sblocks        -> any hasHoleSwitchBlock sblocks
  Do bdy _                -> hasHole bdy
  Synchronized _ b        -> hasHoleBlock b 
  Try b1 catchs mB2       -> hasHoleBlock b1 || any hasHoleCatch catchs || 
                             maybe True hasHoleBlock mB2 
  Labeled _ stmt          -> hasHole stmt
  Hole                    -> True
  _                       -> False

-- removes redudant instances of StmtBlock
flatten_block :: [BlockStmt] -> [BlockStmt]
flatten_block [] = []
flatten_block (b:bs) = case b of
  BlockStmt s -> case s of
    StmtBlock (Block bbs) -> flatten_block $ bbs ++ bs
    _ -> let s' = BlockStmt $ flatten_stmt s
         in s':(flatten_block bs)
  _ -> b:(flatten_block bs)

flatten_stmt :: Stmt -> Stmt
flatten_stmt s = case s of
  StmtBlock (Block bbs) -> StmtBlock $ Block $ flatten_block bbs 
  _ -> s
