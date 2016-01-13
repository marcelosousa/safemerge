module Types where

import Data.Map

type Var = String
type Value = Int
type Valuation = Map Var Value

type Label = String
type LStat = (Label, Stat)

data Stat = Skip | Assume Expr | Assign Lhs Expr 
  deriving (Show, Eq, Ord)

-- Lhs representation of an assignment
data Lhs = LhsVar Var
         | LhsArray Var Expr
  deriving (Show, Eq, Ord)
     
data Expr = Op Expr OpCode Expr -- Binary Operations
          | C Value             -- Constants
          | V Var               -- Variable
          | A Var Expr          -- Array Index
          | F Var [Expr]        -- Function Call
  deriving (Show, Eq, Ord)

data OpCode =
  And | Or | Add | Sub | Mult | Div | Mod | Le | Ge | Leq | Geq | Eq | Neq
  deriving (Show, Eq, Ord)

type Prog = Map Label (Stat, [Label])

-- A program is (entry_node, graph, exit_node, initial_state)
type Program = (Label, Prog, [Label])

type Edit = Map Label Program

type Vars = [Var]

-- methods to assist
(~=) :: Lhs -> Expr -> Stat
(~=) lhs expr = Assign lhs expr
