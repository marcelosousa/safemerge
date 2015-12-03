module Product where

import Data.Map
import Data.Maybe
import Types
import qualified Data.Map as M

type ProdProg = Map Label (Stat, Stat, Stat, Stat)
type ProdProgram = (Label, ProdProg, Label)

-- Product construction
-- 4-way product construction
-- receives the base, variant_A, variant_B, variant_C and merge
-- returns the product construction
product :: (Program, Program, Program, Program) -> ProdProgram
product ((_,base,_), (_,a,_), (_,b,_), merge@(n_e,cmerge,n_x)) =
  let pprog = M.mapWithKey (product_prog (base, a, b)) cmerge
  in (n_e, pprog, n_x)
  
product_prog :: (Prog, Prog, Prog) -> Label -> Stat -> (Stat, Stat, Stat, Stat)
product_prog (base, a, b) node node_merge =
  let node_base' = M.lookup node base
      node_a' = M.lookup node a
      node_b' = M.lookup node b
      node_base = fromMaybe Skip node_base'
      node_a = fromMaybe Skip node_a'
      node_b = fromMaybe Skip node_b'
  in (node_base, node_a, node_b, node_merge)


