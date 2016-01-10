module Buggy3 where

import Data.Map
import Types


{------------------------

parent:
~~~~~~~~
n0: x = 2       [n1]
n1: x = x + 1   [n2]
n2: r = x       [exit]

edit a:
~~~~~~~
n1 |->
  n1_1 : skip   [n2]

edit b:
~~~~~~~
n0 |-> 
  n0_1 : x = 1  [n0_2]
  n0_2 : skip   [n1_1]
n1 |->
  n1_1 : skip   [n2]

merge-edit:
~~~~~~~~~~~
n0 |-> 
  n0_1 : x = 1  [n0_2]
  n0_2 : skip   [n1_1]
n1 |->
  n1_1 : skip   [n2]

--------------------------}




-- exitNode  = ("exit", Skip);
-- exitLabel = "exit"

-- p :: Program
-- p = ("n0", prog, "exit")
--   where prog = fromList $ 
--                [ ("n0",   Assign "x" (C 2))
--                , ("n1",   Assign "x" (Op (V "x") Add (C 1)))
--                , ("n2",   Assign "r" (V "x"))
--                , ("n3",   Goto [exitLabel])
--                , exitNode
--                ]

-- a :: Edit
-- a = fromList $ [ ("n1",n1edit)]
--   where n1edit = ( "n1_1a"
--                  , fromList [ ("n1_1a", Skip)
--                             , ("n1_2a", Goto ["n2"])
--                             ]
--                  , "n1_2a"
--                  )

-- b :: Edit
-- b = fromList $ [ ("n0",n0edit), ("n1",n1edit) ]
--   where n0edit = ( "n0_1b"
--                  , fromList [ ("n0_1b", Assign "x" (C 1))
--                             , ("n0_2b", Goto ["n1"])
--                             ]
--                  , "n0_2b"
--                  )
--         n1edit = ( "n1_1b"
--                  , fromList [ ("n1_1b", Skip)
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
--                  , fromList [ ("n1_1m", Skip)
--                             , ("n1_2m", Goto ["n2"])
--                             ]
--                  , "n1_2m"
--                  )
