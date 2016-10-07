-- from suremerge/conins
module ErrorMsg where

import Data.Map
import Types

{- 
parent:
n0: x = 42;

edit a:
n0_1a: y = 42;

edit b:
n0_1b: z = 42;

merge candidate: 
x = 42
y = 42
z = 42
-}

p :: Program
p =
  let n0 = ("n0", Assign "x" (C 42))
      exit = ("exit", Skip)
      prog = fromList [n0, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n0_1a = ("n0_1a", Assign "y" (C 2))
      eprog = ("n0_1a", fromList [n0_1a], "n0_1a")
  in fromList [("n0", eprog)] -- replace n0 from parent

b :: Edit
b =
  let n0_1b = ("n0_1b", Assign "z" (C 42))
      eprog = ("n0_1b", fromList [n0_1b], "n0_1b")
  in fromList [("n0", eprog)] -- replace n0 from parent

m :: Edit
m = a
