module RearViewMirror where

import Data.Map
import Types

-- parent:
-- assume x == 1 
-- y = 2

-- edit a:
-- assume x == 1 && z == 3
-- y = 2

-- edit b:
-- assume x == 1
-- assume z == 3
-- y = 2

-- merge candidate:
-- assume x == 1 && z == 3
-- y = 2

p :: Program
p =
  let n0 = ("n0", Goto [1])
      n1 = ("n1", Assume $ Op (V "x") Eq (C 1))
      n2 = ("n2", Assign "y" (C 2))
      n3 = ("n3", Skip)
      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, n3, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n1_a1 = ("n1_a1", Assume $ Op (Op (V "x") Eq (C 1)) And (Op (V "z") Eq (C 3)))
      eprog = ("n1_a1", fromList [n1_a1], "n1_a1")
  in fromList [("n1_a1", eprog)]

b :: Edit
b =
  let n3_b1 = ("n3_b1", Assume $ Op (V "z") Eq (C 3)) 
      eprog = ("n3_b1", fromList [n3_b1], "n3_b1")
  in fromList [("n3_b1", eprog)]

m :: Edit
m =
  let n1_m1 = ("n1_m1", Assume $ Op (Op (V "x") Eq (C 1)) And (Op (V "z") Eq (C 3)))
      eprog = ("n1_m1", fromList [n1_m1], "n1_m1")
  in fromList [("n1_m1", eprog)]
