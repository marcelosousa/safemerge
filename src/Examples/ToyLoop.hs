module Examples.ToyLoop where

import Data.Map
import Types

-- shuvendu's write-up example
p :: Program
p =
  let start = ("start", (Assign "x" (C 0), ["start1"]))
      start1 = ("start1", (Assign "y" (C 10), ["n0"]))
      n0  = ("n0",  (Skip, ["n1", "n2"]))
      n1  = ("n1",  (Assume (Op (V "i") Le (V "n")), ["n11"]))
      n11 = ("n11", (Assign "i" (Op (V "i") Add (C 1)), ["n12"]))
      n12 = ("n12", (Skip, ["n0"]))
      n2  = ("n2",  (Assume (Op (V "i") Geq (V "n")), ["n21"]))
      n21 = ("n21", (Skip, ["exit"]))
      prog = fromList [start,start1,n0,n1,n11,n12,n2,n21]
  in ("start", prog, ["exit"])

-- edit for variant A:
-- n12: x = x+1
a :: Edit
a =
  let n12_a = ("n12_a",(Assign "x" (Op (V "x") Add (C 1)), ["n0"]))
      eprog = ("n12_a",fromList [n12_a],["n0"])
  in fromList [("n12",eprog)]

-- edit for variant B:
-- n12: y = y-1
b :: Edit
b =
  let n12_b = ("n12_b",(Assign "y" (Op (V "y") Sub (C 1)), ["n0"]))
      eprog = ("n12_b",fromList [n12_b],["n0"])
  in fromList [("n12",eprog)]

-- merge candidant
-- n12 :-> x = x+1; y = y-1; 
m :: Edit
m = 
  let n12_a = ("n12_a",(Assign "x" (Op (V "x") Add (C 1)), ["n12_b"]))
      n12_b = ("n12_b",(Assign "y" (Op (V "y") Sub (C 1)), ["n0"]))
      eprog = ("n12_a",fromList [n12_a,n12_b],["n0"])
  in fromList [("n12",eprog)]
