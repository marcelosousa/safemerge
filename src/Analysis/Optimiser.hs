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
import Analysis.Java.AST
import Data.List
import Debug.Trace as T
import Language.Java.Syntax
import Calculus 

-- Check the PIDs to make sure that we are always returning
--  statements which fully agree on the PIDs [1,2,3,4]
-- 1. Discover the largest non-hole & non-return sequence of blockstmts  
next_block :: ProdProgram -> (Either ProdProgram AnnBlockStmt, ProdProgram)
next_block [] = (Left [], [])
next_block (b:bs) =
  let (res,bs') = next_block bs
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
split_hole :: AnnBlockStmt -> (Maybe AnnBlockStmt, Maybe AnnBlockStmt) 
split_hole bstmt = case bstmt of
  AnnBlockStmt s -> case s of 
    AnnStmtBlock pids (AnnBlock b) -> case next_block b of
      (Left _, []) -> (Just bstmt, Nothing)
      (Left b',bs) -> (Just $ toBlockStmt pids b', Just $ toBlockStmt pids bs)
      (Right s, _) -> (Nothing, Just bstmt)
    _ -> if hasHole s
         then (Nothing, Just bstmt)
         else (Just bstmt, Nothing)
  -- Not a BlockStmt
  _ -> (Just bstmt, Nothing) 

toBlockStmt :: [Int] -> ProdProgram -> AnnBlockStmt
toBlockStmt p = AnnBlockStmt . AnnStmtBlock p . AnnBlock

hasHoleBlock :: AnnBlock -> Bool
hasHoleBlock (AnnBlock b) = case next_block b of
  (Left _, []) -> False 
  _ -> True

hasHoleSwitchBlock :: AnnSwitchBlock -> Bool
hasHoleSwitchBlock (AnnSwitchBlock _ _ b) = hasHoleBlock (AnnBlock b)

hasHoleCatch :: AnnCatch -> Bool
hasHoleCatch (AnnCatch _ _ b) = hasHoleBlock b

hasHole :: AnnStmt -> Bool
hasHole s = case s of
  AnnStmtBlock    _ b             -> hasHoleBlock b 
  AnnIfThen       _ _ stmt        -> hasHole stmt
  AnnIfThenElse   _ _ t e         -> hasHole t || hasHole e
  AnnWhile        _ _ bdy         -> hasHole bdy
  AnnBasicFor     _ _ _ _ bdy     -> hasHole bdy 
  AnnEnhancedFor  _ _ _ _ _ bdy   -> hasHole bdy 
  AnnSwitch       _ _ sblocks     -> any hasHoleSwitchBlock sblocks
  AnnDo           _ bdy _         -> hasHole bdy
  AnnSynchronized _ _ b           -> hasHoleBlock b 
  AnnTry          _ b1 catchs mB2 -> hasHoleBlock b1 || any hasHoleCatch catchs || 
                                     maybe True hasHoleBlock mB2 
  AnnLabeled      _ _ stmt        -> hasHole stmt
  AnnHole         _               -> True
  AnnReturn       _ _             -> True
  _                               -> False

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
