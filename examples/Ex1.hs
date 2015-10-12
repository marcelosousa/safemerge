module Examples.Ex1 where

import Data.Map
import Types

-- example 1
p :: Program
p =
  let n0 = ("n0",Goto ["n1","n2"])
      n1 = ("n1",Assume $ Op (V "i") Le (V "n"))
      n11 = ("n11",Assign "i" $ Op (V "i") Add (C 1))
      n12 = ("n12",Skip)
      n13 = ("n13",Goto ["n0"])
      n2 = ("n2",Assume $ Op (V "i") Geq (V "n"))
      n21 = ("n21",Skip)
      n22 = ("n22",Goto ["exit"])
      exit = ("exit",Skip)
      prog = fromList [n0,n1,n11,n12,n13,n2,n21,n22,exit]
  in ("n0",prog,"exit")

a :: Edit
a =
  let n12_a = ("n12_a",Assign "x" $ Op (V "x") Add (C 1))
      eprog = ("n12_a",fromList [n12_a],"n12_a")
  in fromList [("n12",eprog)]
  
b :: Edit
b =
  let n12_b = ("n12_b",Assign "y" $ Op (V "y") Sub (C 1))
      eprog = ("n12_b",fromList [n12_b],"n12_b")
  in fromList [("n12",eprog)]
  
m :: Edit
m =
  let n12_m = ("n12_m",Assign "x" $ Op (V "x") Add (C 1))
      n13_m = ("n13_m",Assign "y" $ Op (V "y") Sub (C 1))
      eprog = ("n12_m",fromList [n12_m,n13_m],"n13_m")
  in fromList [("n12",eprog)]
