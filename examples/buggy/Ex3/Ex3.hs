module Ex3 where

import Data.Map
import Types


{------------------------------------

parent:
~~~~~~~
n0: x = 1   [n1]
n1: r = x   [exit]

edit a:
~~~~~~~
n0 |-> 
   n0_1: x = 2 [n1]

edit b:
~~~~~~~
n0 |->
   n0_1: x = 1   [n0_2]
   n0_2: x = x+1 [n1]

merge edit:
~~~~~~~~~~~
n0 |-> 
   n0_1: x = 2   [n0_2]
   n0_2: x = x+1 [n1]
-----------------------------------}


