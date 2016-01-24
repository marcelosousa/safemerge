{-#LANGUAGE RecordWildCards, FlexibleInstances #-}
module Printer where

import qualified Data.Map as M
import Types

pp_prod_prog :: ProdProgram -> String
pp_prod_prog (n_e, m, n_x) =
  let n_e_s = "Entry label: " ++ n_e
      n_x_s = "Exit label: " ++ show n_x
      prog_s = M.foldWithKey pp_prod_prog_line "" m
  in n_e_s ++ "\n" ++ prog_s ++ n_x_s
  where
pp_prod_prog_line pre r@[(ba,pos_ba), (a,pos_a), (b,pos_b), (m,pos_m)] rest =
  let ba_s = "\t" ++ pre ++ ": " ++ show ba ++ ": " ++ show pos_ba
      a_s = "; " ++ show a ++ ": " ++ show pos_a
      b_s = "; " ++ show b ++ ": " ++ show pos_b
      m_s = "; " ++ show m ++ ": " ++ show pos_m
  in ba_s ++ a_s ++ b_s ++ m_s ++ "\n" ++ rest

pp_gen_product :: (Label, EditMap, [Label]) -> String
pp_gen_product (n_e, p, n_x) =
  let n_e_s = "Entry label: " ++ n_e
      n_x_s = "Exit label: " ++ show n_x
      prog_s = M.foldWithKey pp_edit_map "" p
  in unlines [n_e_s, prog_s, n_x_s]
 where
pp_edit_map n_e (prodprog, b) rest =
  let n_e_s = "Label from Base: " ++ n_e
      prodprog_s = unlines $ map (\t -> "\t" ++ t) $ lines $ pp_prod_prog prodprog
  in unlines [n_e_s, prodprog_s, rest]

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
    Neq -> "!="

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
