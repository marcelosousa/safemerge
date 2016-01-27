module Product where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace
import Prelude hiding (product)
import Printer
import Types

generate_product :: Program -> Edit -> Edit -> Edit -> ProdProgram
generate_product base a b m = flatten_product base m $ gen_product base a b m

-- Main Product Generation
-- For each label in the base program, generate the product program associated with it.
-- Note that it is possible that merge edit is going to change the exit label
changed_labels :: [Label] -> Edit -> [(Label, [Label])]
changed_labels [] edit = []
changed_labels (x:xs) edit =
  case M.lookup x edit of
    Nothing -> changed_labels xs edit
    Just (_,_,y) -> (x,y):(changed_labels xs edit)

changed_labels' :: [Label] -> Prog -> [(Label, [Label])]
changed_labels' [] p = []
changed_labels' (x:xs) p =
  case M.lookup x p of
    Nothing -> changed_labels' xs p 
    Just (_,y) -> (x,y):(changed_labels' xs p)

-- Retrieve all labels in x and not in y. 
difference :: [(Label, [Label])] -> [Label] -> [(Label, [Label])]
difference [] y = []
difference ((x,nx):xs) y =
  if x `elem` y
  then difference xs y
  else (x,nx):difference xs y 

missing_base :: [(Label, [Label])] -> Prog
missing_base nx =
  let nx' = map (\(e,n_x) -> (e, (Skip, n_x))) nx
  in M.fromList nx'

-- Generate the product program. Modifications to the exit labels require special attention.
gen_product :: Program -> Edit -> Edit -> Edit -> (Label, EditMap, [Label])
gen_product (n_e, base, n_x) e_a e_b e_m =
  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
      base_changed = changed_labels' n_x base
      merge_changed = changed_labels n_x e_m
      n_x' = concat $ snd $ unzip merge_changed
      nbase = missing_base $ difference merge_changed (fst $ unzip base_changed)
  in case merge_changed of
    [] -> (n_e, prod_base, n_x)
    _ -> 
      let prod_nbase = M.mapWithKey (generate_product' e_a e_b e_m) nbase
      in (n_e, M.union prod_base prod_nbase, n_x') 

-- This is the function that for each node_label of the base generates a product program
generate_product' :: Edit -> Edit -> Edit -> Label -> (Stat, [Label]) -> (ProdProgram, Bool)
generate_product' e_a e_b e_m node_label node_stat@(stat, succs) = 
  let prog_a' = M.lookup node_label e_a
      prog_b' = M.lookup node_label e_b
      prog_m' = M.lookup node_label e_m
      (b, prog_m@(m_entry, m_prog, m_exit)) =
       case prog_m' of
        Nothing -> (False, (node_label, M.fromList [(node_label, node_stat)], succs))
        Just m_edit -> (True, m_edit)
      succ_first =
       case M.lookup m_entry m_prog of
        Nothing -> error "generate_product': getting first"
        Just v -> snd v
      id_edit = (node_label, M.fromList [(m_entry, (stat, succ_first))], succs)
      prog_a = fromMaybe id_edit prog_a'
      prog_b = fromMaybe id_edit prog_b'
      base = id_edit
  in (product (base, prog_a, prog_b, prog_m), b)

-- Flatten the product for each label in the base program
flatten_product :: Program -> Edit -> (Label, EditMap, [Label]) -> ProdProgram
flatten_product (_, base, _n_x) e_m f@(n_e, mapToEdits, n_x) = -- trace (pp_gen_product f) $  
  let entry = n_e
      exit = n_x
      base_changed = changed_labels' _n_x base
      merge_changed = changed_labels _n_x e_m
      n_x' = concat $ snd $ unzip merge_changed
      nbase = missing_base $ difference merge_changed (fst $ unzip base_changed)
      new_base = M.union base nbase
      prog = M.foldWithKey (\k v m -> local_flatten_product k v m mapToEdits) M.empty new_base
  in (entry, prog, exit)
  --in trace ("New base: " ++ pp_prog new_base) $ (entry, prog, exit)

-- For each label in the original program, produce the product program
local_flatten_product :: Label -> (Stat, [Label]) -> ProdProg -> EditMap -> ProdProg
local_flatten_product n_c (stat, succs) prog editsMap = -- trace ("local : " ++ show (n_c, stat, succs, prog) ++"\n") $
 case M.lookup n_c editsMap of
  Nothing -> error "local_flatten_product"
  Just ((n_e, prodprog, n_x), ch) -> 
   if ch
   then
     let goto_e = [(n_c, replicate 4 (Skip, [n_e]))]
       --  goto_x = if n_x == succs
       --           then []
       --           else map (\n_xx -> (n_xx, replicate 4 (Skip, succs))) n_x
         gotos = M.fromList $ goto_e -- ++ goto_x
     in M.union gotos $ M.union prodprog prog
   else M.union prodprog prog

-- Product construction
-- 4-way product construction
-- receives the base, variant_A, variant_B, variant_C and merge
-- returns the product construction
product :: (Program, Program, Program, Program) -> ProdProgram
product ((_,base,_), (_,a,_), (_,b,_), merge@(n_e,cmerge,n_x)) = 
  let pprog = M.mapWithKey (product_prog (base, a, b)) cmerge
  in (n_e, pprog, n_x)
  
product_prog :: (Prog, Prog, Prog) -> Label -> (Stat, [Label]) -> [(Stat, [Label])]
product_prog (base, a, b) node_label node_merge@(stat, succs) =
  let node_base' = M.lookup node_label base -- [(Stat, Label)]
      node_a' = M.lookup node_label a
      node_b' = M.lookup node_label b
      node_base = fromMaybe (Skip, succs) node_base'
      node_a = (fst $ fromMaybe (Skip, undefined) node_a', succs)
      node_b = (fst $ fromMaybe (Skip, undefined) node_b', succs)
  in [node_base, node_a, node_b, node_merge]

combine :: [(Stat, Label)] -> [(Stat, Label)] -> [(Stat, Label)] -> [(Stat, Label)] -> [[(Stat, Label)]]
combine base a b merge = [ [_base, _a, _b, _m] | _base <- base, _a <- a, _b <- b, _m <- merge]

project :: ProdProgram -> Int -> Program
project (n_e, prog, n_x) n =
  let prog' = M.map (\l -> l!!n) prog
  in (n_e, prog', n_x)

-- Equality with respect to skips.
soudness :: (Program, Program, Program, Program) -> ProdProgram -> Bool
soudness (base, a, b, merge) prodprogram =
  let base' = project prodprogram 0
      a' = project prodprogram 1
      b' = project prodprogram 2
      merge' = project prodprogram 3
  in base == base' && a == a' && b == b' && merge == merge'

