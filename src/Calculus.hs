{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Calculus Rules 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Calculus where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Analysis.Java.AST

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T
import Edit.Types
import qualified Data.List as L

type Pid = Int
type ProdProgram = [AnnBlockStmt]

every :: [Pid] -> Bool
every [1,2,3,4] = True
every _ = False

miniproduct :: ProdProgram -> ProdProgram
miniproduct = concatMap pproduct . match  . map flatten_block

-- post condition: no more AnnStmtBlock
match :: ProdProgram -> [ProdProgram]
match list = 
  let max_size = maximum $ map size_of list
      list' = map (\k -> add_skip (max_size - size_of k) k) list 
  in L.transpose list' 

-- | mini product construction 
--   in the case that one of the ann block statement
--   is a block, we need to match them
pproduct :: ProdProgram -> ProdProgram 
pproduct []    = error $ "miniproduct: input should not be []" 
pproduct [s]   = [s]
pproduct (s:r) = 
  case s of
    AnnBlockStmt stmt -> case stmt of
      AnnStmtBlock  p b -> error $ "miniproduct: AnnStmtBlock should not happen" 
      AnnIfThenElse p e sThen sElse -> 
        let tr = AnnStmtBlock p $ AnnBlock $ miniproduct $ (AnnBlockStmt sThen):r 
            er = AnnStmtBlock p $ AnnBlock $ miniproduct $ (AnnBlockStmt sElse):r
            ns = AnnIfThenElse p e tr er
        in [AnnBlockStmt ns] 
      AnnWhile _ _ _  -> 
        if all is_loop (s:r)
        then let w = apply_loop (s:r)
             in w 
        else pproduct (r ++ [s])
      AnnSwitch p e b  -> error $ "miniproduct: switch" 
      AnnHole   p      -> error $ "miniproduct: no support for nested holes" 
      -- in all other cases, resort to Seq-Compose
      _ -> s:(miniproduct r)
    AnnLocalClass cl -> error $ "miniproduct: local class is part of edit" 
    -- Applies Seq-Compose which is just (:)
    AnnLocalVars pids mods ty varDecls -> s:(miniproduct r) 

apply_loop :: ProdProgram -> ProdProgram
apply_loop stmts = 
  let parts = map decomposeWhile stmts
      pids = [a | (a,b,c) <- parts]
      conds = [b | (a,b,c) <- parts]
      bodies = [c | (a,b,c) <- parts]
      body = AnnStmtBlock [] $ AnnBlock $ miniproduct $ map AnnBlockStmt bodies
      true = Lit $ Boolean True
      exp = foldr (\a b -> BinOp a And b) true conds
      (checkPid,npids) = foldr checkPids (True,[]) pids
      while = if checkPid then AnnWhile npids exp body else error "apply_loop:overlapping pids" 
      ifs = toIfs parts
  in map AnnBlockStmt [flatten_stmt while, flatten_stmt ifs] 

checkPids :: [Int] -> (Bool, [Int]) -> (Bool, [Int])
checkPids pid (b,pids) =
  let check = null $ L.intersect pid pids
  in (b && check, pid ++ pids) 

decomposeWhile :: AnnBlockStmt -> ([Int], Exp, AnnStmt)
decomposeWhile ann = case ann of
  AnnBlockStmt (AnnWhile pid e s) -> (pid,e,s)
  _ -> error $ "decomposeWhile: not a While" 

toAnnStmt :: AnnBlockStmt -> AnnStmt
toAnnStmt ann = case ann of
  AnnBlockStmt s -> s
  _ -> error $ "toAnnStmt: invalid input " ++ show ann

toIfs :: [([Int],Exp,AnnStmt)] -> AnnStmt
toIfs [] = error "toIfs: invalid input []"
toIfs [(pid,e,bdy)] = toIf pid e bdy (AnnSkip pid)
toIfs ((pid,e,bdy):xs) =
  toIf pid e bdy $ toIfs xs 
  
toIf :: [Int] -> Exp -> AnnStmt -> AnnStmt -> AnnStmt
toIf pid e bdy rest =
  let whl = AnnWhile pid e bdy 
      body = AnnStmtBlock pid $ AnnBlock $ map AnnBlockStmt [bdy,whl] 
  in AnnIfThenElse pid e body rest 

flatten_block :: AnnBlockStmt -> AnnBlockStmt
flatten_block a = case a of
  AnnBlockStmt stmt -> case stmt of
    AnnStmtBlock pid (AnnBlock [x]) -> flatten_block x
    AnnStmtBlock pid (AnnBlock l) -> 
      AnnBlockStmt $ AnnStmtBlock pid $ AnnBlock $ map flatten_block l
    _ -> AnnBlockStmt $ flatten_stmt stmt 
  _ -> a

flatten_stmt :: AnnStmt -> AnnStmt
flatten_stmt stmt = case stmt of
  AnnStmtBlock pid (AnnBlock l) -> AnnStmtBlock pid $ AnnBlock $ map flatten_block l 
  AnnIfThen pid e s  -> AnnIfThen pid e $ flatten_stmt s 
  AnnIfThenElse pid e s t -> AnnIfThenElse pid e (flatten_stmt s) (flatten_stmt t)
  AnnWhile pid e s -> AnnWhile pid e $ flatten_stmt s
  AnnSwitch pid e b -> AnnSwitch pid e $ map flatten_switch_block b
  AnnLabeled pid i s -> AnnLabeled pid i $ flatten_stmt s
  _ -> stmt

flatten_switch_block :: AnnSwitchBlock -> AnnSwitchBlock
flatten_switch_block (AnnSwitchBlock pid l b) =
  AnnSwitchBlock pid l $ map flatten_block b
