-- fig. 3.5 from Olaf's thesis (http://www.infosun.fim.uni-passau.de/cl/publications/docs/Lessenich12.pdf). 
module VarRename where

import Data.Map
import Types

{- 
parent:
n0: goto n1, n10;
n1: assume n != 1;
n2: goto n3, n6;
n3: assume n % 2 = 0;
n4: n = n / 2;
n5: goto n9;
n6: assume n % 2 != 0;
n7: n = 3*n + 1;
n8: goto n9;
n9: goto n0;
n10: assume n == 1;
n11: goto Exit; // return n

edit a:
n0_0a: goto n0_1a, n0_10a;
n0_1a: assume a != 1;
n0_2a: goto n0_3a, n0_6a;
n0_3a: assume a % 2 = 0;
n0_4a: a = a / 2;
n0_5a: goto n0_9a;
n0_6a: assume a % 2 != 0;
n0_7a: a = 3*a + 1;
n0_8a: goto n0_9a;
n0_9a: goto n0_0a;
n0_10a: assume a == 1;
n0_11a: goto Exit; // return a

edit b:
n0_0b: goto n0_1b, n0_4b;
n0_1b: assume n < 0;
n0_2b: n = n * -1;
n0_3b: goto n0_6b;
n0_4b: assume n >= 0;
n0_5b: goto n0_6b;

n0_6b: goto n0_7b, n0_16b;
n0_7b: assume n != 1;
n0_8b: goto n0_9b, n0_12b;
n0_9b: assume n % 2 = 0;
n0_10b: n = n / 2;
n0_11b: goto n0_15b;
n0_12b: assume n % 2 != 0;
n0_13b: n = 3*n + 1;
n0_14b: goto n0_15b;
n0_15b: goto n0_6b;
n0_16b: assume n == 1;
n0_17b: goto Exit; // return n


correct merge candidate: edit b
incorrect merge candidate (as stated in fig 3.5):
n0_0m: goto n0_1m, n0_4m;
n0_1m: assume n < 0;
n0_2m: n = n * -1;
n0_3m: goto n0_6m;
n0_4m: assume n >= 0;
n0_5m: goto n0_6m;

n0_6m: goto n0_7m, n0_16m;
n0_7m: assume a != 1;
n0_8m: goto n0_9m, n0_12m;
n0_9m: assume a % 2 = 0;
n0_10m: a = a / 2;
n0_11m: goto n0_15m;
n0_12m: assume a % 2 != 0;
n0_13m: a = 3*a + 1;
n0_14m: goto n0_15m;
n0_15m: goto n0_6m;
n0_16m: assume a == 1;
n0_17m: goto Exit; // return a

-}

p :: Program
p =
  let n0 = ("n0", Goto ["n1", "n10"])
      n1 = ("n1", Assume $ Op (V "n") Neq (C 1))
      n2 = ("n2", Goto ["n3", "n6"])
      n3 = ("n3", Assume $ Op (Op (V "n") Mod (C 2)) Eq (C 0))
      n4 = ("n4", Assign "n" (Op (V "n") Div (C 2))) 
      n5 = ("n5", Goto ["n9"])
      n6 = ("n6", Assume $ Op (Op (V "n") Mod (C 2)) Neq (C 0))
      n7 = ("n7", Assign "n" (Op (Op (C 3) Mult (V "n")) Add (C 1)))
      n8 = ("n8", Goto ["n9"])
      n9 = ("n9", Goto ["n0"])
      n10 = ("n10", Assume $ Op (V "n") Eq (C 1))
      n11 = ("n11", Goto ["exit"])      
      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, exit]
  in ("n0", prog, "exit") 

a :: Edit
a = 
  let n0_0a = ("n0_0a", Goto ["n0_1a", "n0_10a"])
      n0_1a = ("n0_1a", Assume $ Op (V "a") Neq (C 1))
      n0_2a = ("n0_2a", Goto ["n0_3a", "n0_6a"])
      n0_3a = ("n0_3a", Assume $ Op (Op (V "a") Mod (C 2)) Eq (C 0))
      n0_4a = ("n0_4a", Assign "a" (Op (V "a") Div (C 2))) 
      n0_5a = ("n0_5a", Goto ["n0_9a"])
      n0_6a = ("n0_6a", Assume $ Op (Op (V "a") Mod (C 2)) Neq (C 0))
      n0_7a = ("n0_7a", Assign "a" (Op (Op (C 3) Mult (V "a")) Add (C 1)))
      n0_8a = ("n0_8a", Goto ["n0_9a"])
      n0_9a = ("n0_9a", Goto ["n0_0a"])
      n0_10a = ("n0_10a", Assume $ Op (V "a") Eq (C 1))
      n0_11a = ("n0_11a", Goto ["exit"])      

      eprog = ("n0_0a", fromList [n0_0a, n0_1a, n0_2a, n0_3a, n0_4a, n0_5a, n0_6a, n0_7a, n0_8a, 
                                  n0_9a, n0_10a, n0_11a], "n0_11a")
  in fromList [("n0", eprog)] -- replace n0 from parent

b :: Edit
b = 
  let n0_0b = ("n0_0b", Goto["n0_1b", "n0_4b"])
      n0_1b = ("n0_1b", Assume $ Op (V "n") Le (C 0))
      n0_2b = ("n0_2b", Assign "n" $ Op (V "n") Mult (C (-1)))
      n0_3b = ("n0_3b", Goto ["n0_6b"])
      n0_4b = ("n0_4b", Assume $ Op (V "n") Geq (C 0))
      n0_5b = ("n0_5b", Goto ["n0_6b"])

      n0_6b = ("n0_6b", Goto ["n0_7b", "n0_16b"])
      n0_7b = ("n0_7b", Assume $ Op (V "n") Neq (C 1))
      n0_8b = ("n0_8b", Goto ["n0_9b", "n0_12b"])
      n0_9b = ("n0_9b", Assume $ Op (Op (V "n") Mod (C 2)) Eq (C 0))
      n0_10b = ("n0_10b", Assign "n" (Op (V "n") Div (C 2))) 
      n0_11b = ("n0_11b", Goto ["n0_15b"])
      n0_12b = ("n0_12b", Assume $ Op (Op (V "n") Mod (C 2)) Neq (C 0))
      n0_13b = ("n0_13b", Assign "n" (Op (Op (C 3) Mult (V "n")) Add (C 1)))
      n0_14b = ("n0_14b", Goto ["n0_15b"])
      n0_15b = ("n0_15b", Goto ["n0_6b"])
      n0_16b = ("n0_16b", Assume $ Op (V "n") Eq (C 1))
      n0_17b = ("n0_17b", Goto ["exit"])      

      eprog = ("n0_0b", fromList [n0_0b, n0_1b, n0_2b, n0_3b, n0_4b, n0_5b, n0_6b, n0_7b, n0_8b, 
                                  n0_9b, n0_10b, n0_11b, n0_12b, n0_13b, n0_14b, n0_15b, n0_16b, n0_17b], "n0_17b")
  in fromList [("n0", eprog)] -- replace n0 from parent

correctMerge :: Edit
correctMerge = b

wrongMerge :: Edit
wrongMerge = 
  let n0_0m = ("n0_0m", Goto["n0_1m", "n0_4m"])
      n0_1m = ("n0_1m", Assume $ Op (V "n") Le (C 0))
      n0_2m = ("n0_2m", Assign "n" $ Op (V "n") Mult (C (-1)))
      n0_3m = ("n0_3m", Goto ["n0_6m"])
      n0_4m = ("n0_4m", Assume $ Op (V "n") Geq (C 0))
      n0_5m = ("n0_5m", Goto ["n0_6m"])

      n0_6m = ("n0_6m", Goto ["n0_7m", "n0_16m"])
      n0_7m = ("n0_7m", Assume $ Op (V "a") Neq (C 1))
      n0_8m = ("n0_8m", Goto ["n0_9m", "n0_12m"])
      n0_9m = ("n0_9m", Assume $ Op (Op (V "a") Mod (C 2)) Eq (C 0))
      n0_10m = ("n0_10m", Assign "a" (Op (V "a") Div (C 2))) 
      n0_11m = ("n0_11m", Goto ["n0_15m"])
      n0_12m = ("n0_12m", Assume $ Op (Op (V "a") Mod (C 2)) Neq (C 0))
      n0_13m = ("n0_13m", Assign "a" (Op (Op (C 3) Mult (V "a")) Add (C 1)))
      n0_14m = ("n0_14m", Goto ["n0_15m"])
      n0_15m = ("n0_15m", Goto ["n0_6m"])
      n0_16m = ("n0_16m", Assume $ Op (V "a") Eq (C 1))
      n0_17m = ("n0_17m", Goto ["exit"])      

      eprog = ("n0_0m", fromList [n0_0m, n0_1m, n0_2m, n0_3m, n0_4m, n0_5m, n0_6m, n0_7m, n0_8m, 
                                  n0_9m, n0_10m, n0_11m, n0_12m, n0_13m, n0_14m, n0_15m, n0_16m, n0_17m], "n0_17m")
  in fromList [("n0", eprog)] -- replace n0 from parent
