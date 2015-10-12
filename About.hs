module About where

import Data.Map
import Types

-- parent:
-- x = 1;
-- y = 2;
-- z = 3;

-- edit a:
-- x = 2;
-- x += 1;
-- y = 2;
-- z = 3;

-- edit b:
-- x = 1;

-- merge candidate:
-- x = 2;
-- x += 1;

p :: Program
p =
  let n0 = ("n0", Goto ["n1"])
      n1 = ("n1", Assign "x" (C 1))
      n2 = ("n2", Skip)
      n3 = ("n3", Assign "y" (C 2))
      n4 = ("n4", Assign "z" (C 3))
      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, n3, n4, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n11 = ("n11", Assign "x" (C 2))
      n12 = ("n12", Assign "x" $ Op (V "x") Add (C 2))
      eprog = ("n11", fromList [n11, n12], "n12")
  in fromList [("n1", eprog)]

b :: Edit
b =
  let n21 = ("n21", Assign "x" (C 1))
      n22 = ("n22", Skip)
      n23 = ("n23", Skip)
      n24 = ("n24", Skip)
      eprog = ("n21", fromList [n21, n22, n23, n24], "n24")
  in fromList [("n1", eprog)]

m :: Edit
m =
  let n1m = ("n31", Assign "x" (C 2))
      n2m = ("n32", Assign "x" $ Op (V "x") Add (C 2))
      n3m = ("n33", Skip)
      eprog = ("n31", fromList [n1m,n1m, n3m], "n33")
  in fromList [("n1", eprog)]
