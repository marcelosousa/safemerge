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
  deriving (Show, Eq, Ord)
data OpCode =
  And | Or | Add | Sub | Mult | Div | Le | Ge | Leq | Geq | Eq | Neq
  deriving (Show, Eq, Ord)

type Prog = Map Label Stat
-- A program is (entry_node, graph, exit_node, initial_state)
type Program = (Label, Prog, Label)

type Edit = Map Label Program
