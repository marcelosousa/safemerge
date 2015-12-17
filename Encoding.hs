module Encoding where

import qualified Data.Map as M
import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer
import Product
import Types
import Prelude hiding (product)
import Data.Maybe
import Examples.SimpleEncoding
import Data.List hiding (product)

-- Pre-processing: get the number of variables. the number of nodes is computed on the fly.
-- Pre-process unedited edges to have id edits.
-- Builds the product program:
-- Map each edge of base: e
--   For each e: 
--     Get the edits a, b and merge.
--     Build the product of e using the edits.
-- Calls the main_encoding for the product program.
--encoding :: Program -> Edit -> Edit -> Edit -> String
--encoding base a b merge =
--  let prodprogram = generate_product base a b merge
--      vars = get_vars prodprogram      
--  in show $ prettyprint $ main_encoding prodprogram vars

generate_product :: Program -> Edit -> Edit -> Edit -> ProdProgram
generate_product base a b m = flatten_product base $ gen_product base a b m

showProdProg :: ProdProgram -> String
showProdProg (n_e, m, n_x) = 
  let n_e_s = "Entry label: " ++ n_e
      n_x_s = "Exit label: " ++ n_x
      prog_s = M.foldWithKey showProdProgLine "" m
  in n_e_s ++ "\n" ++ prog_s ++ n_x_s

showProdProgLine :: Label -> [(Stat, [Label])] -> String -> String
showProdProgLine pre [(ba,pos_ba), (a,pos_a), (b,pos_b), (m,pos_m)] rest =
  let ba_s = "\t" ++ pre ++ ": " ++ show ba ++ ": " ++ show pos_ba
      a_s = "\t" ++ pre ++ ": " ++ show a ++ ": " ++ show pos_a
      b_s = "\t" ++ pre ++ ": " ++ show b ++ ": " ++ show pos_b
      m_s = "\t" ++ pre ++ ": " ++ show m ++ ": " ++ show pos_m
  in unlines [ba_s, a_s, b_s, m_s] ++ rest

print_gen_product :: (Label, M.Map Label ProdProgram, Label) -> String
print_gen_product (n_e, p, n_x) =
  let n_e_s = "Entry label: " ++ n_e
      n_x_s = "Exit label: " ++ n_x
      prog_s = M.foldWithKey print_gen_product' "" p
  in unlines [n_e_s, prog_s, n_x_s]

print_gen_product' :: Label -> ProdProgram -> String -> String
print_gen_product' n_e prodprog rest =
  let n_e_s = "Label from Base: " ++ n_e
      prodprog_s = unlines $ map (\t -> "\t" ++ t) $ lines $ showProdProg prodprog
  in unlines [n_e_s, prodprog_s, rest]

gen_product :: Program -> Edit -> Edit -> Edit -> (Label, M.Map Label ProdProgram, Label)
gen_product (n_e, base, n_x) e_a e_b e_m = 
  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
  in (n_e, prod_base, n_x)
  
id_prog :: Label -> Program
id_prog label = (label, M.fromList [(label, (Skip, [label]))], label)

generate_product' :: Edit -> Edit -> Edit -> Label -> (Stat, [Label]) -> ProdProgram
generate_product' e_a e_b e_m node_label node_stat =
  let prog_a' = M.lookup node_label e_a
      prog_b' = M.lookup node_label e_b
      prog_m' = M.lookup node_label e_m
      prog_a = fromMaybe (id_prog node_label) prog_a' 
      prog_b = fromMaybe (id_prog node_label) prog_b'
      prog_m = fromMaybe (id_prog node_label) prog_m'
      base = (node_label, M.fromList [(node_label, node_stat)], node_label)
  in product (base, prog_a, prog_b, prog_m)

flatten_product :: Program -> (Label, M.Map Label ProdProgram, Label) -> ProdProgram
flatten_product (n_e, base, n_x) (_, mapToEdits, _) =
  (n_e, flatten_product' n_e n_x base mapToEdits M.empty, n_x)

flatten_product' :: Label -> Label -> Prog -> EditMap -> ProdProg -> ProdProg
flatten_product' n_c n_x base editsMap current =
 if n_c == n_x
 then current
 else case M.lookup n_c editsMap of
  Nothing -> error "flatten_product"
  Just (n_e, prodprog, n_xx) ->
   let goto_e = M.fromList [(n_c, replicate 4 (Skip, [n_e]))] 
       current' = M.union goto_e $ M.union prodprog current
   in case M.lookup n_c base of
    Nothing -> error "flatten_product base"
    Just list ->
     let succs = snd list
         goto_x = map (\n_k -> M.fromList [(n_xx, replicate 4 (Skip, [n_k]))]) succs
         goto_x_final = foldr M.union current' goto_x
         rest = map (\n_k -> flatten_product' n_k n_x base editsMap goto_x_final) succs
     in foldr M.union M.empty rest

get_vars :: ProdProgram -> Vars
get_vars (a, prog, b) = nub $ M.fold (\l r -> (concatMap get_vars_p l) ++ r) [] prog

get_vars_p :: (Stat, [Label]) -> Vars
get_vars_p = (get_vars_s . fst)

get_vars_s :: Stat -> Vars
get_vars_s s = case s of
  Skip -> []
  Assume e -> get_vars_e e
  Assign v e -> v:(get_vars_e e) 
  Goto _ -> []

get_vars_e :: Expr -> Vars
get_vars_e e = case e of
  Op lhs _ rhs -> get_vars_e lhs ++ get_vars_e rhs
  C _ -> []
  V v -> [v]
  F v e1 e2 -> [v] ++ get_vars_e e1 ++ get_vars_e e2

-- Encoding for the product program P'
-- Start node: Eq of all vars implies the predicate of the start node
-- Similar for exit node: postcondition implies merge criteria.
--  For each edge e \in P', e = (base, a, b, m)
--   Define an encoding for each type of statements
main_encoding :: ProdProgram -> Vars -> SMod
main_encoding prodprogram@(ne,prod,nx) vars =
  let h = header prodprogram vars
      i = initial_state ne vars
      f = final_state nx vars
      prog = M.foldWithKey (encode_stat vars) [] prod
  in h ++ i ++ f

encode_Q :: Vars -> Label -> SExpr
encode_Q vars label =
  let q_label = SymIdent $ SimpleSym $ "Q_" ++ label
      enc_vars = map to_var vars
  in FnAppExpr q_label enc_vars

encode_stat :: Vars -> Label -> [(Stat, [Label])] -> SMod -> SMod
encode_stat vars n_e [(s_o,n_o), (s_a,n_a), (s_b,n_b), (s_c,n_c)] rest =
  undefined
{-
  let node_e = SimpleSym $ "Q_"++n_e
      node_xx = delete n_e $ nub [s_o, s_a, s_b, s_c]
  in case node_xx of
    [n_x] ->
      let node_x = SimpleSym $ "Q_" ++ n_x
          _vars = map forall_var vars -- [[xa, xb, xc, xd], ...]
      
    _ -> error "encode_s" 
      _vars = map forall_var vars
      vars_ = concat _vars
      and_vars = map (\[a,b,c,d] -> [mk_eq a b, mk_eq b c, mk_eq c d]) _vars
      vars_for = map (\v -> (SimpleSym v, "Int")) vars_
      pre = FnAppExpr (SymIdent $ SimpleSym "and") $ concat and_vars 
      post = FnAppExpr (SymIdent node_e) $ map (\v -> FnAppExpr (SymIdent $ SimpleSym v) []) vars_
      for_expr = FnAppExpr (SymIdent $ SimpleSym "=>") [pre, post]
  in [SE $ Assert $ ForallExpr vars_for for_expr]
--}

header :: ProdProgram -> Vars -> SMod
header (n_e,prodprogram,n_x) vars = 
  let nodes = nub $ n_e:n_x:(M.keys prodprogram)
      sig = concatMap (\v -> replicate 4 (SymSort "Int")) vars
      nodes_enc = map (\n -> SE $ DeclFun (SimpleSym $ "Q_"++n) sig (SymSort "Bool")) nodes
      logic = setlogic HORN
  in logic:nodes_enc

forall_var :: Var -> Vars
forall_var v = [v++"o", v++"a", v++"b", v++"m"]

to_var a = FnAppExpr (SymIdent $ SimpleSym a) []
mk_e op a b = FnAppExpr (SymIdent $ SimpleSym op)
  [a, b]

mk_eq a b = mk_e "=" (to_var a) (to_var b)
mk_or a b = mk_e "or" a b 
mk_ors l = FnAppExpr (SymIdent $ SimpleSym "or") l
mknot l =  FnAppExpr (SymIdent $ SimpleSym "not") [l]

initial_state :: Label -> Vars -> SMod
initial_state ne vars = 
  let node_e = SimpleSym $ "Q_"++ne
      _vars = map forall_var vars
      vars_ = concat _vars
      and_vars = map (\[a,b,c,d] -> [mk_eq a b, mk_eq b c, mk_eq c d]) _vars
      vars_for = map (\v -> (SimpleSym v, "Int")) vars_
      pre = FnAppExpr (SymIdent $ SimpleSym "and") $ concat and_vars 
      post = FnAppExpr (SymIdent node_e) $ map (\v -> FnAppExpr (SymIdent $ SimpleSym v) []) vars_
      for_expr = FnAppExpr (SymIdent $ SimpleSym "=>") [pre, post]
  in [SE $ Assert $ ForallExpr vars_for for_expr]

-- condition
exit_condition :: Vars -> SExpr
exit_condition [xo,xa,xb,xc] =
  FnAppExpr (SymIdent $ SimpleSym "and") [mk_or (mk_eq xo xa) (mk_eq xc xa)
                                         ,mk_or (mk_eq xo xb) (mk_eq xc xb)
                                         ,mk_ors [mknot $ mk_eq xo xa
                                                 ,mknot $ mk_eq xo xb
                                                 ,mknot $ mk_eq xc xo]
                                         ] 
  
final_state :: Label -> Vars -> SMod
final_state nx vars = 
  let node_x = SimpleSym $ "Q_"++nx
      _vars = map forall_var vars -- [[xa, xb, xc, xd], ...]
      vars_ = concat _vars
      vars_for = map (\v -> (SimpleSym v, "Int")) vars_
      pre = FnAppExpr (SymIdent node_x) $ map to_var vars_
      -- post condition
      and_vars = map (\l -> exit_condition l) _vars
      post = FnAppExpr (SymIdent $ SimpleSym "and") and_vars 
      for_expr = FnAppExpr (SymIdent $ SimpleSym "=>") [pre, post]
  in [SE $ Assert $ ForallExpr vars_for for_expr]
