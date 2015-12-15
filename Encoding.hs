module Encoding where

import qualified Data.Map as M
import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer
import Product
import Types
import Prelude hiding (product)
import Data.Maybe
import Examples.SimpleEncoding

-- Pre-processing: get the number of variables. the number of nodes is computed on the fly.
-- Pre-process unedited edges to have id edits.
-- Builds the product program:
-- Map each edge of base: e
--   For each e: 
--     Get the edits a, b and merge.
--     Build the product of e using the edits.
-- Calls the main_encoding for the product program.
--encoding :: Program -> Edit -> Edit -> Edit -> String
--encoding base a b merge =
--  let prodprogram = generate_product base a b merge
--      vars = get_vars prodprogram      
--  in show $ prettyprint $ main_encoding prodprogram vars

generate_product :: Program -> Edit -> Edit -> Edit -> ProdProgram
generate_product (n_e, base, n_x) e_a e_b e_m = undefined
--  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
--  in (n_e, prod_base, n_x)

gen_product :: Program -> Edit -> Edit -> Edit -> (Label, M.Map Label ProdProgram, Label)
gen_product (n_e, base, n_x) e_a e_b e_m = 
  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
  in (n_e, prod_base, n_x)
  
id_prog :: Label -> Program
id_prog label = (label, M.fromList [(label, [(Skip, label)])], label)

generate_product' :: Edit -> Edit -> Edit -> Label -> [(Stat, Label)] -> ProdProgram
generate_product' e_a e_b e_m node_label node_stat =
  let prog_a' = M.lookup node_label e_a
      prog_b' = M.lookup node_label e_b
      prog_m' = M.lookup node_label e_m
      prog_a = fromMaybe (id_prog node_label) prog_a' 
      prog_b = fromMaybe (id_prog node_label) prog_b'
      prog_m = fromMaybe (id_prog node_label) prog_m'
      base = (node_label, M.fromList [(node_label, node_stat)], node_label)
  in product (base, prog_a, prog_b, prog_m)

--flatten_product :: [ProdProgram] -> ProdProgram
--flatten_product (p1,p2) = undefined

get_vars :: ProdProgram -> Vars
get_vars (a, prog, b) = M.fold (\l r -> (concatMap get_vars_p l) ++ r) [] prog

get_vars_p :: [(Stat, Label)] -> Vars
get_vars_p = concatMap (get_vars_s . fst)

get_vars_s :: Stat -> Vars
get_vars_s s = case s of
  Skip -> []
  Assume e -> get_vars_e e
  Assign v e -> v:(get_vars_e e) 
  Goto _ -> []

get_vars_e :: Expr -> Vars
get_vars_e e = case e of
  Op lhs _ rhs -> get_vars_e lhs ++ get_vars_e rhs
  C _ -> []
  V v -> [v]
  F v e1 e2 -> [v] ++ get_vars_e e1 ++ get_vars_e e2

-- Encoding for the product program P'
-- Start node: Eq of all vars implies the predicate of the start node
-- Similar for exit node: postcondition implies merge criteria.
--  For each edge e \in P', e = (base, a, b, m)
--   Define an encoding for each type of statements
main_encoding :: ProdProgram -> Vars -> SMod
main_encoding prodprogram vars =
  let logic = setlogic HORN
  in [logic]