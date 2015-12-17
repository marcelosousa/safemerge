module Product where

import Data.Map
import Data.Maybe
import Types
import qualified Data.Map as M

type ProdProg = Map Label [(Stat, [Label])]
type ProdProgram = (Label, ProdProg, Label)
type EditMap = Map Label ProdProgram

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
      node_a = fromMaybe (Skip, succs) node_a'
      node_b = fromMaybe (Skip, succs) node_b'
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

