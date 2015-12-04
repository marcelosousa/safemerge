module Encoding where

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
encoding base a b merge = show $ prettyprint $ main_encoding base a b merge

-- Information needed for the encoding:
--  Number of variables? For the cardinality of Q_node?
--  Number of nodes?
     
-- Encoding for the product program P'
-- Start node: Eq of all vars implies the predicate of the start node
-- Similar for exit node: postcondition implies merge criteria.
--  For each edge e \in P', e = (base, a, b, m)
--   Define an encoding for each type of statements
main_encoding' :: ProdProgram -> SMod
main_encoding' = undefined

main_encoding :: Program -> Edit -> Edit -> Edit -> SMod
main_encoding base a b merge =
  let logic = setlogic HORN
  in [logic]