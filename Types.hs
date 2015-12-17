module Types where

import Data.Map

type Var = String
type Value = Int
type Valuation = Map Var Value

type Label = String
type LStat = (Label, Stat)
data Stat = Skip | Assume Expr | Assign Var Expr | Goto [Label]
  deriving (Show, Eq, Ord)
data Expr = Op Expr OpCode Expr
          | C Value
          | V Var
          | F Var Expr Expr 
  deriving (Show, Eq, Ord)
data OpCode =
  And | Or | Add | Sub | Mult | Div | Mod | Le | Ge | Leq | Geq | Eq | Neq
  deriving (Show, Eq, Ord)

-- n0: if (x > 0)
-- n1: then 
 -- n11: stat_1
-- n2: else stat_2

-- n0: (Skip, [n1, n2])
-- n1: (assume (x>0), [n11])
-- n11: (stat_1, [n3])
-- n2: (assume (x<=0), [n21])
-- n21: (stat_2, [n3])

-- n0: prog

type Prog = Map Label (Stat, [Label])
-- A program is (entry_node, graph, exit_node, initial_state)
type Program = (Label, Prog, Label)

type Edit = Map Label Program

type Vars = [Var]
