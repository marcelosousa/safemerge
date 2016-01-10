module Ex1 where

import Data.Map
import Types


{- Two plausible ways of encoding Ex1. Does it matter which is chosen?
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First:

   parent:
   ~~~~~~~~~~~~~~~~~~~~~
   n0: skip  [n1]
   n1: skip  [n2]
   n2: r = x [exit]

   edit a:
   ~~~~~~~~~~~~~~~~~~~~~
   n0 |-> 
      n0_1: x = x+1 [n1]

   edit b:
   ~~~~~~~~~~~~~~~~~~~~~
   n1 |-> 
      n1_1: x = x+1 [n2]

   merge-edit:
   ~~~~~~~~~~~~~~~~~~~~~~~
   n0 |-> 
      n0_1: x = x+1 [n1_1]
   n1 |-> 
      n1_1: x = x+1 [n2]

Second:

   parent:
   ~~~~~~~~~~~~~~~~~~~~~
   n0: skip  [n1]
   n2: r = x [exit]

   edit a:
   ~~~~~~~~~~~~~~~~~~~~~
   n0 |-> 
      n0_1: x = x+1 [n1]

   edit b:
   ~~~~~~~~~~~~~~~~~~~~~
   n1 |-> 
      n1_1: x = x+1 [n1_2]
      n1_2: r = x [exit]

   merge-edit:
   ~~~~~~~~~~~~~~~~~~~~~~~
   n0 |-> 
      n0_1: x = x+1 [n1_1]
   n1 |-> 
      n1_1: x = x+1 [n1_2]
      n1_2: r = x [exit]

-----------------------------------------------------------------------}



