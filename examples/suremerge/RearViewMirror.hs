-- from suremerge/blobs benchmark

module RearViewMirror where

import Data.Map
import Types

{- 
parent:
if (x == 1) { y = 2; } else {}

n0: goto n1, n4;
n1: assume x == 1;
n2: skip;
n3: y = 2;
n4: goto exit;
n5: assume x != 1;
n6: goto exit;
Exit: skip;

edit a:
if (x == 1 && z == 3) { y = 2; } else {}

n1_1a: assume x == 1 && z == 3;
n1_2a: y = 2;
n1_3a: goto exit;

edit b:
if (x == 1) { 
  if (z == 3) { 
    y = 2;
  }
}
else
{}

n2_1b: goto n2_2b, n2_5b;
n2_2b: assume z == 3;
n2_3b: y = 2;
n2_4b: goto exit;
n2_5b: assume z != 3;
n2_6b: goto exit;

merge candidate: edit a
-}

p :: Program
p =
  let n0 = ("n0", Goto ["n1", "n4"])
      n1 = ("n1", Assume $ Op (V "x") Eq (C 1))
      n2 = ("n2", Skip)
      n3 = ("n3", Assign "y" (C 2))
      n4 = ("n4", Goto ["exit"])
      n5 = ("n5", Assume $ Op (V "x") Neq (C 1))
      n6 = ("n6", Goto ["exit"])
      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, n3, n4, n5, n6, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n1_1a = ("n1_1a", Assume $ Op (Op (V "x") Eq (C 1)) And (Op (V "z") Eq (C 3)))
      n1_2a = ("n1_2a", Assign "y" (C 2))
      n1_3a = ("n1_3a", Goto ["exit"])
      eprog = ("n1_1a", fromList [n1_1a, n1_2a, n1_3a], "n1_3a")
  in fromList [("n1", eprog)] -- replace parent program starting at n1

b :: Edit
b =
  let n2_1b = ("n2_1b", Goto ["n2_2b", "n2_5b"])
      n2_2b = ("n2_2b", Assume $ Op (V "z") Eq (C 3)) 
      n2_3b = ("n2_3b", Assign "y" (C 2))
      n2_4b = ("n2_4b", Goto ["exit"])
      n2_5b = ("n2_5b", Assume $ Op (V "z") Neq (C 3))
      n2_6b = ("n2_6b", Goto ["exit"])
      eprog = ("n2_1b", fromList [n2_1b, n2_2b, n2_3b, n2_4b, n2_5b, n2_6b], "n2_6b")
  in fromList [("n2", eprog)] -- replace parent program starting at n2

m :: Edit
m = a
