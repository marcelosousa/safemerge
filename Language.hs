{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
import Language.C.Syntax.AST (CBinaryOp,CUnaryOp)

type Ident = String
data Value = 
    IntValue Integer
  | FloatValue Float
  | StrValue String
  deriving (Eq,Ord)
   
type OpCode  = CBinaryOp
type UOpCode = CUnaryOp
type PC      = Int

data Type = IntType
  deriving (Show,Eq,Ord)
  
data Program = Program (Decls, Defs)
  deriving (Eq,Ord)
        
type Decls = [Declaration]
type Defs  = [Definition]

data Declaration = 
    FunctionDecl PC Ident Params
  | GlobalDecl PC Ident (Maybe Value)
  deriving (Eq,Ord)

type Params = [Parameter]
data Parameter = Param Ident
  deriving (Show,Eq,Ord)
 
data Definition = 
    FunctionDef PC Ident Params Statement
  deriving (Eq,Ord)

--type Statements = [(PC, Statement)]
type Statement = AnnStatement PC

data AnnStatement a = 
    Assign a Expression Expression                      -- x := expr
  | Local  a Expression (Maybe Expression)                   -- decl x
  | Sequence (AnnStatement a) (AnnStatement a)          -- S1;S2;
  | IfThen a Expression (AnnStatement a)                -- if expr then { S1; }
  | If a Expression (AnnStatement a) (AnnStatement a)   -- if expr then { S1; } else { S2; }
  | While a Expression (AnnStatement a)                 -- while expr { S; }
  | Return a Expression                                 -- return expr
  | CallS a Ident [Expression]                          -- call fname [arguments]
  deriving (Ord)

instance Eq (AnnStatement a) where
    (==) (Assign _ i1 e1) (Assign _ i2 e2) = i1 == i2 && e1 == e2
    (==) (Local _ i1 e1)  (Local _ i2 e2)  = i1 == i2 && e1 == e2
    (==) (Sequence s11 s21) (Sequence s12 s22) = s11 == s12 && s21 == s22
    (==) (IfThen _ c1 s1) (IfThen _ c2 s2) = c1 == c2 && s1 == s2
    (==) (If _ c1 st1 se1) (If _ c2 st2 se2) = c1 == c2 && st1 == st2 && se1 == se2
    (==) (While _ c1 s1) (While _ c2 s2) = c1 == c2 && s1 == s2
    (==) (Return _ e1) (Return _ e2) = e1 == e2
    (==) (CallS _ fn1 ps1) (CallS _ fn2 ps2) = fn1 == fn2 && ps1 == ps2
    (==) _ _ = False

data Expression = 
    Call Ident [Expression]               -- call fname [arguments]
  | BinOp OpCode Expression Expression    -- expr BINOP expr
  | UnaryOp UOpCode Expression            -- UOP expr
  | Const Value                           -- value
  | Ident Ident                           -- x
  | Index Expression Expression
  deriving (Eq,Ord)
  
