module Examples.Ex2 where

import Data.Map
import Types

-- An example with loop and two variants

{-
P:  //vars x, y, i
 start: x = 0; y = 10;
 n0: goto n1, n2;
 n1: assume i < n;
 n11:  /* x = 1;*/ goto exit; //difficult if P steps alone
 n2: assume i >= n;
 n21: goto exit;
 Exit: skip
-}
p :: Program
p = 
  let n0 = ("n0",Goto ["n1","n2"])
      n1 = ("n1",Assume $ Op (V "i") Le (V "n"))
      n11 = ("n11",Goto ["exit"]) -- 
      n2 = ("n2",Assume $ Op (V "i") Geq (V "n"))
      n21 = ("n21",Goto ["exit"])
      exit = ("exit",Skip)
      prog = fromList [n0,n1,n11,n2,n21,exit]
  in ("n0",prog,"exit")

{-
//Only showing the non-identity edits (n11)
A: n11 -> n11_a
n11_a: goto n11_1a, n11_2a;
n11_1a: assume x < m;
n11_3a: x+=3;
n11_4a: goto n11_a;
n11_2a: assume x >= m;
n11_5a: goto exit11_a;
exit11_a: skip;
-}
a :: Edit
a =
  let n11_a = ("n11_a",Goto ["n11_1a", "n11_2a"])
      n11_1a = ("n11_1a",Assume $ Op (V "x") Le (V "m"))
      n11_3a = ("n11_3a",Assign "x" $ Op (V "x") Add (C 3))
      n11_4a = ("n11_4a",Goto ["n11_a"])
      n11_2a = ("n11_2a",Assume $ Op (V "x") Geq (V "m"))
      n11_5a = ("n11_5a",Goto ["exit11_a"])
      exit11_a = ("exit11_a",Skip)
      eprog = ("n11_a",fromList [n11_a,n11_1a,n11_2a,n11_3a,n11_4a,n11_5a,exit11_a],"exit11_a")
  in fromList [("n11",eprog)]

{-
B: n11 -> n11_b
n11_b: y--;
n11_1b: goto exit11_b;
Exit11_b: skip;
-}
b :: Edit
b =
  let n11_b = ("n11_b",Assign "y" $ Op (V "y") Sub (C 1))
      n11_1b = ("n11_1b",Goto ["exit11_b"])
      exit11_b = ("exit11_b",Skip)
      eprog = ("n11_b",fromList [n11_b,n11_1b,exit11_b],"exit11_b")
  in fromList [("n11",eprog)]
  
{-
C: n11 -> n11_a
n11_a: goto n11_1a, n11_2a;
n11_1a: assume x < m;
n11_3a: x+=3; 
n11_4a: goto n11_a;
n11_2a: assume x >= 1m;
n11_5a: goto n11_b;
n11_b: y--;
n11_1b: goto exi11_a;
exit11_a: goto exit11_b;
exit11_b: skip;
-}
m :: Edit
m =
  let n11_a = ("n11_a",Goto ["n11_1a","n11_2a"])
      n11_1a = ("n11_1a",Assume $ Op (V "x") Le (V "m"))
      n11_3a = ("n11_3a",Assign "x" $ Op (V "x") Add (C 3))
      n11_4a = ("n11_4a",Goto ["n11_a"])
      n11_2a = ("n11_2a",Assume $ Op (V "x") Geq (V "m"))
      n11_5a = ("n11_5a",Goto ["exit11_a"])
      n11_b = ("n11_b",Assign "y" $ Op (V "y") Sub (C 1))
      n11_1b = ("n11_1b",Goto ["exit11_a"])
      exit11_a = ("exit11_a",Goto ["exit11_b"])
      exit11_b = ("exit11_b",Skip)
      eprog = ("n11_a",fromList [n11_a,n11_1a,n11_2a,n11_3a,n11_4a,n11_5a,exit11_a,n11_b,n11_1b,exit11_b],"exit11_b")
  in fromList [("n11",eprog)]
