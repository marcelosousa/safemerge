{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Calculus Rules 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Calculus where

import Analysis.Java.AST
import Analysis.Util
import Data.Map (Map)
import Data.Maybe (fromJust)
import Edit.Apply
import Edit.Types
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import qualified Data.List as L
import qualified Data.Map as M
import qualified Debug.Trace as T

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
      AnnWhile _ _  -> 
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

-- | Apply a simple metric to select a set of loops to fuse
apply_loop :: ProdProgram -> ProdProgram
apply_loop stmts = 
  let parts = map decomposeWhile stmts
      loops = select_loop_rule parts
  in foldr (\l r -> apply_loop_rule l ++ r) [] loops 

select_loop_rule :: [([(Int,Exp)], AnnStmt)] -> [[([(Int,Exp)], AnnStmt)]]
select_loop_rule [] = []
select_loop_rule (h:t) = 
  let (t',r) = select_loops h t
      f = select_loop_rule r
      e = h:t'
  in e:f

select_loops :: ([(Int,Exp)], AnnStmt) -> [([(Int,Exp)], AnnStmt)] -> ([([(Int,Exp)], AnnStmt)], [([(Int,Exp)], AnnStmt)])  
select_loops (exps,_) l =
  let vars = L.nub $ concatMap getIdentsExp $ snd $ unzip exps 
      l'   = filter (isSimilar vars) l 
  in (l', l L.\\ l')

isSimilar :: [Ident] -> ([(Int,Exp)], AnnStmt) -> Bool
isSimilar vars (exps,_) =
  let vars' = L.nub $ concatMap getIdentsExp $ snd $ unzip exps  
  in not $ null $ L.intersect vars vars'

-- | Apply fusion to a set of loops
apply_loop_rule :: [([(Int,Exp)], AnnStmt)] -> ProdProgram 
apply_loop_rule parts = 
  let (conds,bodies) = unzip parts 
      cond = concat conds
      body = AnnStmtBlock [] $ AnnBlock $ miniproduct $ map AnnBlockStmt bodies
      while = AnnWhile cond body 
      ifs = toIfs parts
  in map AnnBlockStmt [flatten_stmt while, flatten_stmt ifs] 

decomposeWhile :: AnnBlockStmt -> ([(Int,Exp)], AnnStmt)
decomposeWhile ann = case ann of
  AnnBlockStmt (AnnWhile e s) -> (e,s)
  _ -> error $ "decomposeWhile: not a While" 

toAnnStmt :: AnnBlockStmt -> AnnStmt
toAnnStmt ann = case ann of
  AnnBlockStmt s -> s
  _ -> error $ "toAnnStmt: invalid input " ++ show ann

toIfs :: [([(Int,Exp)],AnnStmt)] -> AnnStmt
toIfs [] = error "toIfs: invalid input []"
toIfs [([(pid,e)],bdy)] = toIf [(pid,e)] bdy (AnnSkip [pid])
toIfs ((e,bdy):xs) =
  toIf e bdy $ toIfs xs 
  
toIf :: [(Int,Exp)] -> AnnStmt -> AnnStmt -> AnnStmt
toIf w@[(pid,e)] bdy rest =
  let whl = AnnWhile w bdy 
      body = AnnStmtBlock [pid] $ AnnBlock $ map AnnBlockStmt [bdy,whl] 
  in AnnIfThenElse [pid] e body rest 

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
  AnnWhile e s -> AnnWhile e $ flatten_stmt s
  AnnSwitch pid e b -> AnnSwitch pid e $ map flatten_switch_block b
  AnnLabeled pid i s -> AnnLabeled pid i $ flatten_stmt s
  _ -> stmt

flatten_switch_block :: AnnSwitchBlock -> AnnSwitchBlock
flatten_switch_block (AnnSwitchBlock pid l b) =
  AnnSwitchBlock pid l $ map flatten_block b

--
-- | Whole product construction
wholeProduct :: MemberDecl -> [Edit] -> (AnnMemberDecl,AnnEdits) 
wholeProduct _mth _es = 
  let _n_es = map (\(vId,_e) -> toAnn [vId] $ fst $ apply_edit_member _mth _e) $ zip [1,2,3,4] _es 
      _n_bs = map ann_mth_body _n_es
      _n_bss = map (\(AnnMethodBody m) -> fromJust m) _n_bs
      _n_ss = map (\(vId,_e) -> AnnBlockStmt $ AnnStmtBlock [vId] _e) $ zip [1,2,3,4] _n_bss
      body = miniproduct _n_ss 
      AnnMethodDecl _1 _2 _3 _4 _5 _6 _ = head _n_es
      n_o = AnnMethodDecl _1 _2 _3 _4 _5 _6 (AnnMethodBody (Just (AnnBlock body)))
  in (n_o,M.empty)
