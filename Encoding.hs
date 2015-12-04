module Encoding where

import qualified Data.Map as M
import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer
import Product
import Types

-- Pre-processing: get the number of variables. the number of nodes is computed on the fly.
-- Pre-process unedited edges to have id edits.
-- Builds the product program:
-- Map each edge of base: e
--   For each e: 
--     Get the edits a, b and merge.
--     Build the product of e using the edits.
-- Calls the main_encoding for the product program.
encoding :: Program -> Edit -> Edit -> Edit -> String
encoding base a b merge =
  let prodprogram = generate_product base a b merge
      vars = get_vars prodprogram      
  in show $ prettyprint $ main_encoding prodprogram vars

generate_product :: Program -> Edit -> Edit -> Edit -> ProdProgram
generate_product (n_e, base, n_x) e_a e_b e_m =
  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
  in (n_e, prod_base, n_x)
  
generate_product' :: Edit -> Edit -> Edit -> Label -> Stat -> [Stat]
generate_product' e_a e_b e_m node_label node_stat =
  let prog_base' = M.lookup node_label e_a
      prog_a' = M.lookup node_label e_b
      prog_b' = M.lookup node_label e_m
      prog_base = fromMaybe Skip node_base'
      prog_a = fromMaybe Skip node_a'
      prog_b = fromMaybe Skip node_b'
  in undefined
-- product :: (Program, Program, Program, Program) -> ProdProgram

get_vars :: ProdProgram -> Vars
get_vars = undefined

-- Encoding for the product program P'
-- Start node: Eq of all vars implies the predicate of the start node
-- Similar for exit node: postcondition implies merge criteria.
--  For each edge e \in P', e = (base, a, b, m)
--   Define an encoding for each type of statements
main_encoding :: ProdProgram -> Vars -> SMod
main_encoding prodprogram vars =
  let logic = setlogic HORN
  in [logic]