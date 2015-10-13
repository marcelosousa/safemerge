-- from suremerge/idtwins
module About where

import Data.Map
import Types

{- 
parent:
n0: x = 1;
n1: y = 2;
n2: z = 3;

edit a:
n0_1a: x = 2;
n0_2a: x += 1;
n0_3a: goto n1;

edit b:
n1_1b: goto exit;

merge candidate:
n0_1m: x = 2;
n0_2m: x += 1;
n0_3m: goto exit;
-}

p :: Program
p =
  let n0 = ("n0", Assign "x" (C 1))
      n1 = ("n1", Assign "y" (C 2))
      n2 = ("n2", Assign "z" (C 3))
      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n0_1a = ("n0_1a", Assign "x" (C 2))
      n0_2a = ("n0_2a", Assign "x" $ Op (V "x") Add (C 1))
      n0_3a = ("n0_3a", Goto ["n1"])
      eprog = ("n0_1a", fromList [n0_1a, n0_2a, n0_3a], "n0_3a")
  in fromList [("n0", eprog)] -- replace n0 from parent

b :: Edit
b =
  let n1_1b = ("n1_1b", Goto ["exit"])
      eprog = ("n1_1b", fromList [n1_1b], "n1_1b")
  in fromList [("n1", eprog)] -- replace n1 from parent

m :: Edit
m =
  let n0_1m = ("n0_1m", Assign "x" (C 2))
      n0_2m = ("n0_2m", Assign "x" $ Op (V "x") Add (C 1))
      n0_3m = ("n0_3m", Goto ["exit"])
      eprog = ("n0_1m", fromList [n0_1m, n0_2m, n0_3m], "n0_3m")
  in fromList [("n0", eprog)] -- replace n0 from parent
