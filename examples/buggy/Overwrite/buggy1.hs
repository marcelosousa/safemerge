module Buggy1 where

import Data.Map
import Types


{------------------------------------
parent:
~~~~~~~
n0: x = 2;
n1: r = x;

edit a:
~~~~~~~
n0 |-> 
   n0_1: x = 1 [n1]

edit b:
~~~~~~~
n0 |->
   n0_1: x = 2   [n0_2]
   n0_2: x = x+1 [n1]


merge edit:
~~~~~~~~~~~
n0 |-> 
   n0_1: x = 1   [n0_2]
   n0_2: x = x+1 [n1]
-----------------------------------}



-- exitNode  = ("exit", Skip);
-- exitLabel = "exit"


-- p :: Program
-- p = ("n0", prog, "exit")
--   where prog = fromList $ 
--                [ ("n0",   Assign "x" (C 2))
--                , ("n1",   Skip)
--                , ("n2",   Assign "r" (V "x"))
--                , ("n3",   Goto [exitLabel])
--                , exitNode
--                ]

-- a :: Edit
-- a = fromList $ [ ("n0",n0edit)]
--   where n0edit = ( "n0_1a"
--                  , fromList [ ("n0_1a", Assign "x" (C 1))
--                             , ("n0_2a", Goto ["n1"])
--                             ]
--                  , "n0_2a"
--                  )

-- b :: Edit
-- b = fromList $ [ ("n1",n1edit)]
--   where n1edit = ( "n1_1b"
--                  , fromList [ ("n1_1b", Assign "x" (Op (V "x") Add (C 1)))
--                             , ("n1_2b", Goto ["n2"])
--                             ]
--                  , "n1_2b"
--                  )


-- m :: Edit
-- m = fromList $ [ ("n0",n0edit), ("n1",n1edit) ]
--   where n0edit = ( "n0_1m"
--                  , fromList [ ("n0_1m", Assign "x" (C 1))
--                             , ("n0_2m", Goto ["n1"])
--                             ]
--                  , "n0_2m"
--                  )
--         n1edit = ( "n1_1m"
--                  , fromList [ ("n1_1m", Assign "x" (Op (V "x") Add (C 1)))
--                             , ("n1_2m", Goto ["n2"])
--                             ]
--                  , "n1_2m"
--                  )
