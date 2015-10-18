-- from suremerge/tuplets
module GetCLIArgs where

import Data.Map
import Types

{- 
parent:
n0: goto n1, n9;

n1: assume argi < args_length;
n2: arg = getArg(argi);

n3: goto n4, n7;
n4: assume arg = 0;
n5: rvm_Ab = arg;
n6: goto n0;
n7: assume arg != 0;
n8: goto n0;

n9 : assume argi >= args_length;   -- loop end
n10: goto exit;


edit a:
n8_1a: goto n8_2a, n8_5a;
n8_2a: assume arg = 1;
n8_3a: rvm_Fb = arg;
n8_4a: goto n0;
n8_5a: assume arg != 1;
n8_6a: goto n0;


edit b:
n6_1b: errorCode_Ab = arg;
n6_2b: goto n0;

n8_1b: goto n8_2b, n8_6b;
n8_2b: assume arg = 1;
n8_3b: rvm_Fb = arg;
n8_4b: errorCode_Fb = arg;
n8_5b: goto n0;
n8_6b: assume arg != 1;
n8_7b: goto n0;


merge candidate: edit b
-}

p :: Program
p =
  let n0 = ("n0", Goto ["n1", "n9"])
      n1 = ("n1", Assume $ Op (V "argi") Le (V "args_length"))
      n2 = ("n2", Assign "arg" (F "getArg" (V "argi")))
      
      n3 = ("n3", Goto ["n4", "n7"])
      n4 = ("n4", Assume $ Op (V "arg") Eq (C 0))
      n5 = ("n5", Assign "rvm_Ab" (V "arg"))
      n6 = ("n6", Goto ["n0"])
      n7 = ("n7", Assume $ Op (V "arg") Neq (C 0))
      n8 = ("n8", Goto ["n0"])

      n9 = ("n9", Assume $ Op (V "argi") Ge (V "args_length"))
      n10 = ("n10", Goto ["exit"]) 

      exit = ("exit", Skip)
      prog = fromList [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, exit]
  in ("n0", prog, "exit") 

a :: Edit
a =
  let n8_1a = ("n8_1a", Goto ["n8_2a", "n8_5a"])
      n8_2a = ("n8_2a", Assume $ Op (V "arg") Eq (C 1)) 
      n8_3a = ("n8_3a", Assign "rvm_Fb" (V "arg"))
      n8_4a = ("n8_4a", Goto ["n0"])
      n8_5a = ("n8_5a", Assume $ Op (V "arg") Neq (C 1))
      n8_6a = ("n8_6a", Goto ["n0"])

      eprog = ("n8_1a", fromList [n8_1a, n8_2a, n8_3a, n8_4a, n8_5a, n8_6a], "n8_6a")
  in fromList [("n8", eprog)] -- replace n8 from parent

b :: Edit
b =
  let n6_1b = ("n6_1b", Assign "errorCode_Ab" (V "arg"))
      n6_2b = ("n6_2b", Goto ["n0"])
      
      n8_1b = ("n8_1b", Goto ["n8_2b", "n8_5b"])
      n8_2b = ("n8_2b", Assume $ Op (V "arg") Eq (C 1)) 
      n8_3b = ("n8_3b", Assign "rvm_Fb" (V "arg"))
      n8_4b = ("n8_4b", Assign "errorCode_Fb" (V "arg"))
      n8_5b = ("n8_5b", Goto ["n0"])
      n8_6b = ("n8_6b", Assume $ Op (V "arg") Neq (C 1))
      n8_7b = ("n8_7b", Goto ["n0"])

      eprog1 = ("n6_1b", fromList [n6_1b, n6_2b], "n6_2b")
      eprog2 = ("n8_1b", fromList [n8_1b, n8_2b, n8_3b, n8_4b, n8_5b, n8_6b, n8_7b], "n8_7b")
  in fromList [("n6", eprog1), ("n8", eprog2)] -- replace n6 and n8 from parent

m :: Edit
m = b
