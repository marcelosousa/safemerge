module Product where

import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Prelude hiding (product)
import Types

type ProdProg = Map Label [(Stat, [Label])]
type ProdProgram = (Label, ProdProg, [Label])
type EditMap = Map Label (ProdProgram, Bool)

generate_product :: Program -> Edit -> Edit -> Edit -> ProdProgram
generate_product base a b m = flatten_product base $ gen_product base a b m

-- Main Product Generation
-- For each label in the base program, generate the product program associated with it.
gen_product :: Program -> Edit -> Edit -> Edit -> (Label, EditMap, [Label])
gen_product (n_e, base, n_x) e_a e_b e_m =
  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
  in (n_e, prod_base, n_x)

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
flatten_product :: Program -> (Label, EditMap, [Label]) -> ProdProgram
flatten_product (n_e, base, n_x) (_, mapToEdits, _) =
  let entry = n_e
      exit = n_x
      prog = M.foldWithKey (\k v m -> local_flatten_product k v m mapToEdits) M.empty base
  in (entry, prog, exit)
--  (n_e, flatten_product' n_e n_x base mapToEdits [] M.empty, n_x)

-- For each label in the original program, produce the product program
local_flatten_product :: Label -> (Stat, [Label]) -> ProdProg -> EditMap -> ProdProg
local_flatten_product n_c (stat, succs) prog editsMap =
 case M.lookup n_c editsMap of
  Nothing -> error "local_flatten_product"
  Just ((n_e, prodprog, n_x), ch) -> --trace ("flt: " ++ show (n_c, n_e, n_x, succs) ++ "\n") $
   if ch
   then
     let goto_e = [(n_c, replicate 4 (Skip, [n_e]))]
         goto_x = if n_x == succs
                  then []
                  else map (\n_xx -> (n_xx, replicate 4 (Skip, succs))) n_x
         gotos = M.fromList $ goto_e ++ goto_x
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
      node_a = (fst $ fromMaybe node_base node_a', succs)
      node_b = (fst $ fromMaybe node_base node_b', succs)
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

