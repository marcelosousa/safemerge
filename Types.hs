module Types where

import Data.Map

type Var = String
type Value = Int
type Valuation = Map Var Value

type Label = Int
type LStat = (Label, Stat)
data Stat = Skip | Assume Expr | Assign Var Expr | Goto [Label]
  deriving (Show, Eq, Ord)
data Expr = Op Expr OpCode Expr
          | C Value
          | V Var
  deriving (Show, Eq, Ord)
data OpCode =
  And | Or | Add | Sub | Mult | Div | Le | Ge | Leq | Geq | Eq
  deriving (Show, Eq, Ord)

type Prog = Map Label Stat
-- A program is (entry_node, graph, exit_node, initial_state)
type Program = (Label, Prog, Label)

type Edit = Map Int Program

p :: Program
p =
  let n0 = (0,Goto [1,2])
      n1 = (1,Assume $ Op (V "i") Le (V "n"))
      n11 = (11,Assign "i" $ Op (V "i") Add (C 1))
      n12 = (12,Skip)
      n13 = (13,Goto [0])
      n2 = (2,Assume $ Op (V "i") Geq (V "n"))
      n21 = (21,Skip)
      n22 = (22,Goto [(-1)])
      exit = ((-1),Skip)
      prog = fromList [n0,n1,n11,n12,n13,n2,n21,n22,exit]
  in (0,prog,(-1))

a :: Edit
a =
  let n12 = (121,Assign "x" $ Op (V "x") Add (C 1))
      eprog = (121,fromList [n12],121)
  in fromList [(12,eprog)]
  
b :: Edit
b =
  let n12 = (122,Assign "y" $ Op (V "y") Sub (C 1))
      eprog = (122,fromList [n12],122)
  in fromList [(12,eprog)]
  
m :: Edit
m =
  let n12m = (123,Assign "x" $ Op (V "x") Add (C 1))
      n13m = (133,Assign "y" $ Op (V "y") Sub (C 1))
      eprog = (123,fromList [n12m,n13m],133)
  in fromList [(12,eprog)]
  