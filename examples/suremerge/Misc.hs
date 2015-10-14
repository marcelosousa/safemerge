-- from suremerge/eofsting
module Misc where

import Data.Map
import Types

{- 
Note: the actual code appended an extra function definition
at the end of the parent, and the extra function is not 
explicitly invoked. I am pretending it to be invoked to make
the example more interesting. -akc

parent:
n0: skip;
n1: x = 1;
n2: goto exit;

edit a:
x = 1;
if (y == 2) { 
  if (z == 3) {
    a = 4;
  } else {}
} else {
  b = 5;
}

n2_1a: goto n2_2a, n2_9a;
n2_2a: assume y == 2;
n2_3a: goto n2_4a, n2_7a;
n2_4a: assume z == 3;
n2_5a: a = 4;
n2_6a: goto n2_12a;
n2_7a: assume z != 3;
n2_8a: goto n2_12a;
n2_9a: assume y != 2;
n2_10a: b = 5;
n2_11a: goto exit;
n2_12a: goto exit;


edit b:
k = 3;
x = 1;
if (y == 2) { 
  if (z == 3) {
    a = 4;
  } else {}
} else {
  c = 5; // note: different from edit a
}

n0_1b: k = 3;
n0_2b: goto n1;

n2_1b: goto n2_2b, n2_9b;
n2_2b: assume y == 2;
n2_3b: goto n2_4b, n2_7b;
n2_4b: assume z == 3;
n2_5b: a = 4;
n2_6b: goto n2_12b;
n2_7b: assume z != 3;
n2_8b: goto n2_12b;
n2_9b: assume y != 2;
n2_10b: c = 5;
n2_11b: goto exit;
n2_12b: goto exit;


merge candidate: 

k = 3;
x = 1;
if (y == 2) { 
  if (z == 3) {
    a = 4;
  } else {}
} else {
  b = 5; // note: take from edit a
}

n0_1m: k = 3;
n0_2m: goto n1;

n2_1m: goto n2_2m, n2_9m;
n2_2m: assume y == 2;
n2_3m: goto n2_4m, n2_7m;
n2_4m: assume z == 3;
n2_5m: a = 4;
n2_6m: goto n2_12m;
n2_7m: assume z != 3;
n2_8m: goto n2_12m;
n2_9m: assume y != 2;
n2_10m: b = 5;
n2_11m: goto exit;
n2_12m: goto exit;

-}

p :: Program
p =
  let n0 = ("n0", Skip)
      n1 = ("n1", Assign "x" (C 1))
      n2 = ("n2", Goto ["exit"])
      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n2_1a = ("n2_1a", Goto ["n2_2a", "n2_8a"])
      n2_2a = ("n2_2a", Assume $ Op (V "y") Eq (C 2)) 
      n2_3a = ("n2_3a", Goto ["n2_4a", "n2_7a"])
      n2_4a = ("n2_4a", Assume $ Op (V "z") Eq (C 3))
      n2_5a = ("n2_5a", Assign "a" (C 4))
      n2_6a = ("n2_6a", Goto ["n2_12a"])
      n2_7a = ("n2_7a", Assume $ Op (V "z") Neq (C 3))
      n2_8a = ("n2_8a", Goto ["n2_12a"])
      n2_9a = ("n2_9a", Assume $ Op (V "y") Neq (C 2))
      n2_10a = ("n2_10a", Assign "b" (C 5))
      n2_11a = ("n2_11a", Goto ["exit"])
      n2_12a = ("n2_12a", Goto ["exit"])
      eprog = ("n2_1a", fromList [n2_1a, n2_2a, n2_3a, n2_4a, n2_5a, n2_6a, 
                                  n2_7a, n2_8a, n2_9a, n2_10a, n2_11a, n2_12a], 
               "n2_12a")
  in fromList [("n2", eprog)] -- replace n2 from parent

b :: Edit
b =
  let n0_1b = ("n0_1b", Assign "k" (C 3))
      n0_2b = ("n0_2b", Goto ["n1"])

      n2_1b = ("n2_1b", Goto ["n2_2b", "n2_8b"])
      n2_2b = ("n2_2b", Assume $ Op (V "y") Eq (C 2))
      n2_3b = ("n2_3b", Goto ["n2_4b", "n2_7b"])
      n2_4b = ("n2_4b", Assume $ Op (V "z") Eq (C 3))
      n2_5b = ("n2_5b", Assign "a" (C 4))
      n2_6b = ("n2_6b", Goto ["n2_12b"])
      n2_7b = ("n2_7b", Assume $ Op (V "z") Neq (C 3))
      n2_8b = ("n2_7b", Goto ["n2_12b"])
      n2_9b = ("n2_9b", Assume $ Op (V "y") Neq (C 2))
      n2_10b = ("n2_10b", Assign "c" (C 5))
      n2_11b = ("n2_9b", Goto ["exit"])
      n2_12b = ("n2_10b", Goto ["exit"])

      eprog1 = ("n0_1b", fromList [n0_1b, n0_2b], "n0_2b")
      eprog2 = ("n2_1b", fromList [n2_1b, n2_2b, n2_3b, n2_4b, n2_5b, n2_6b, 
                                   n2_7b, n2_8b, n2_9b, n2_10b, n2_11b, n2_12b], 
                "n2_12b")
  in fromList [("n0", eprog1), ("n2", eprog2)] -- replace n0 and n2 from parent

m :: Edit
m = 
  let n0_1m = ("n0_1m", Assign "k" (C 3))
      n0_2m = ("n0_2m", Goto ["n1"])

      n2_1m = ("n2_1m", Goto ["n2_2m", "n2_9m"])
      n2_2m = ("n2_2m", Assume $ Op (V "y") Eq (C 2))
      n2_3m = ("n2_3m", Goto ["n2_4m", "n2_7m"])
      n2_4m = ("n2_4m", Assume $ Op (V "z") Eq (C 3))
      n2_5m = ("n2_5m", Assign "a" (C 4))
      n2_6m = ("n2_6m", Goto ["n2_12m"])
      n2_7m = ("n2_7m", Assume $ Op (V "z") Neq (C 3))
      n2_8m = ("n2_8m", Goto ["n2_12m"])
      n2_9m = ("n2_9m", Assume $ Op (V "y") Neq (C 2))
      n2_10m = ("n2_10m", Assign "b" (C 5))
      n2_11m = ("n2_11m", Goto ["exit"])
      n2_10m = ("n2_10m", Goto ["exit"])

      eprog1 = ("n0_1m", fromList [n0_1m, n0_2m], "n0_2m")
      eprog2 = ("n2_1m", fromList [n2_1m, n2_2m, n2_3m, n2_4m, n2_5m, n2_6m, 
                                   n2_7m, n2_8m, n2_9m, n2_10m, n2_11m, n2_12m], 
                "n2_12m")
  in fromList [("n0", eprog1), ("n2", eprog2)] -- replace n0 and n2 from parent
