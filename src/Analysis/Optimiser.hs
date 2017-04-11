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
  in case split_bstmt b of
    (Nothing, Nothing) -> error $ "next_block: split_bstmt returned Nothing Nothing"
    (Nothing, Just b') -> (Right b, bs)
    (Just b', Nothing) -> -- no hole 
      case res of
        Left r  -> (Left (b:r), bs')
        Right _ -> (Left [b], bs)
    (Just bl, Just br) -> -- hole in the middle
      (Left [bl], br:bs)

-- 2. Checks if a blockstmt has a hole and splits it 
--                         (Maybe PartWithNoHole, Maybe PartWithHole)
split_bstmt :: AnnBlockStmt -> (Maybe AnnBlockStmt, Maybe AnnBlockStmt) 
split_bstmt bstmt = case bstmt of
  AnnBlockStmt s -> case s of 
    AnnStmtBlock pids (AnnBlock b) -> case next_block b of
      (Left _, []) -> (Just bstmt, Nothing)
      (Left b',bs) -> (Just $ toBlockStmt pids b', Just $ toBlockStmt pids bs)
      (Right s, _) -> (Nothing, Just bstmt)
    _ -> if not $ is_common s
         then (Nothing, Just bstmt)
         else (Just bstmt, Nothing)
  -- Not a BlockStmt
  _ -> (Just bstmt, Nothing) 

toBlockStmt :: [Int] -> ProdProgram -> AnnBlockStmt
toBlockStmt p = AnnBlockStmt . AnnStmtBlock p . AnnBlock

is_common_block :: AnnBlock -> Bool
is_common_block (AnnBlock b) = case next_block b of
  (Left _, []) -> False 
  _ -> True

is_common_switch_block :: AnnSwitchBlock -> Bool
is_common_switch_block (AnnSwitchBlock _ _ b) = is_common_block (AnnBlock b)

is_common_catch :: AnnCatch -> Bool
is_common_catch (AnnCatch _ _ b) = is_common_block b

is_common :: AnnStmt -> Bool
is_common s = case s of
  AnnStmtBlock    p b             -> every p && is_common_block b 
  AnnIfThen       p _ stmt        -> every p && is_common stmt
  AnnIfThenElse   p _ t e         -> every p && is_common t || is_common e
  AnnWhile        p bdy           -> every (fst $ unzip p) && is_common bdy
  AnnBasicFor     p _ _ _ bdy     -> every p && is_common bdy 
  AnnEnhancedFor  p _ _ _ _ bdy   -> every p && is_common bdy 
  AnnSwitch       p _ sblocks     -> every p && all is_common_switch_block sblocks
  AnnDo           p bdy _         -> every p && is_common bdy
  AnnSynchronized p _ b           -> every p && is_common_block b 
  AnnTry          p b1 catchs mB2 -> every p && is_common_block b1 || all is_common_catch catchs || 
                                     maybe True is_common_block mB2 
  AnnLabeled      p _ stmt        -> every p && is_common stmt
  AnnHole         p               -> False 
  AnnReturn       p _             -> False 
  _                               -> every $ getAnn s
