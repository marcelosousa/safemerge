{-#LANGUAGE RecordWildCards, FlexibleInstances #-}
module Printer where

import qualified Data.Map as M
import Types

instance Show OpCode where
  show op = case op of
    And -> "&&"
    Or -> "||"
    Add -> "+"
    Sub -> "-"
    Mult -> "*"
    Div -> "/"
    Mod -> "%"
    Le -> "<"
    Ge -> ">"
    Leq -> "<="
    Geq -> ">="
    Eq -> "=="
    Neq -> "!"

instance Show Lhs where
  show lhs = case lhs of
    LhsVar var -> var
    LhsArray var e -> var ++ "[" ++ show e ++ "]"

instance Show Stat where
  show s = case s of
    Skip -> "skip"
    Assume exp -> "assume(" ++ show exp ++ ")"
    Assign lhs exp -> show lhs ++ ":=" ++ show exp

instance Show Expr where
  show e = case e of
    Op lhs opc rhs -> show lhs ++ show opc ++ show rhs
    C v -> show v
    V v -> v
    A v e -> v ++ "[" ++ show e ++ "]"
    F v a -> v ++ "(" ++ showArgs a ++ ")"

showArgs :: [Expr] -> String
showArgs [] = ""
showArgs [x] = show x
showArgs (x:xs) = show x ++ "," ++ showArgs xs
