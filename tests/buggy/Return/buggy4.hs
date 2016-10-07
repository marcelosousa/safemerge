
module Buggy4 where

import Data.Map
import Types

{- 
parent:
~~~~~~~
n0: skip   [n1]
n1: r := 1 [exit]

edit a:
~~~~~~~
n0 |->
   n0_1: skip           [n0_1, n1]
   n0_1: assume (x > 0) [n0_2]
   n0_2: r := 0         [exit]

edit b:
~~~~~~~
n0 |->
   n0_0: skip           [n0_1,n1_1]
   n0_1: assume (x > 0) [n0_1]
   n0_2: r := 1         [exit]
n1 |->
   n1_1: r := 0         [exit]

merge-edit:
~~~~~~~~~~~
n0 |-> 
   n0_0: skip           [n0_1,n0_3]
   n0_1: assume (x > 0) [n0_2]
   n0_2: r := 0         [n0_3]
   n0_3: r := 1         [n1_1]

n1 |-> 
   n1_1: r := 0 [exit]

-}
