module Encoding where

import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer

import Product
import Types

encoding :: Program -> Edit -> Edit -> Edit -> String
encoding base a b merge = show $ prettyprint $ main_encoding base a b merge

-- Information needed for the encoding:
--  Number of variables? For the cardinality of Q_node?
--  Number of nodes?
main_encoding :: Program -> Edit -> Edit -> Edit -> SMod
main_encoding base a b merge =
  let logic = setlogic HORN
  in [logic]