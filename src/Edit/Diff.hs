{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Diff
-- Purpose   :  Diff2 Algorithm 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit.Diff where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T
import Edit.Types

type M = Map (Int,Int) Int

lk :: (Int,Int) -> M -> Int
lk ij c =
  case M.lookup ij c of
    Nothing -> error "lk"
    Just v  -> v

iM :: Int -> Int -> M
iM m n = 
  let keys = [ (i,j) | i <- [0..m], j <- [0..n] ]
  in M.fromList $ zip keys $ repeat 0   

lcs :: (Show a, Eq a) => [a] -> [a] -> M 
lcs xs ys =
  let -- create the initial matrix
      c = iM (length xs) (length ys) 
      -- create (x_i, i) array
      xs' = zip xs [1..]
      -- create (y_j, j) array
      ys' = zip ys [1..]
      c' = foldl (\c_i (x,i) -> foldl (lcslen (x,i)) c_i ys') c xs' 
  in c'
 where 
    lcslen :: (Show a, Eq a) => (a,Int) -> M -> (a,Int) -> M  
    lcslen (x,i) c (y,j) = 
       if x == y 
       then let p = lk (i-1,j-1) c in M.insert (i,j) (p+1) c
       else M.insert (i,j) (max (lk (i,j-1) c) (lk (i-1,j) c)) c

hole = BlockStmt Hole
skip = BlockStmt Skip

-- printdiff :: (Eq a,Show a) -> [a] -> [a] -> M -> (Int,Int) -> IO ()
diff2edit xs ys c (i,j) (o,a,b) 
  | i > 0 && j > 0 && xs!!(i-1) == ys!!(j-1)          = diff2edit xs ys c (i-1,j-1) (xs!!(i-1):o,a,b)
  | j > 0 && (i == 0 || lk (i,j-1) c >= lk (i-1,j) c) = diff2edit xs ys c (i,j-1)   (hole:o,skip:a, (ys!!(j-1)):b)
  | i > 0 && (j == 0 || lk (i,j-1) c < lk (i-1,j) c)  = diff2edit xs ys c (i-1,j)   (hole:o,xs!!(i-1):a, skip:b)
  | otherwise = (o,a,b)

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d):(zip4 as bs cs ds)
