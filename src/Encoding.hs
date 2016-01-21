module Encoding where

import qualified Data.Map as M
import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer
import Printer
import Product
import Types
import Prelude hiding (product)
import Data.Maybe
import Data.List hiding (product)
import Data.Map (Map)
import Debug.Trace

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

encode :: Program -> Edit -> Edit -> Edit -> SMod
encode base a b m = main_encoding $ generate_product base a b m

generate_product :: Program -> Edit -> Edit -> Edit -> ProdProgram
generate_product base a b m = flatten_product base $ gen_product base a b m

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

-- Main Product Generation
-- For each label in the base program, generate the product program associated with it.
gen_product :: Program -> Edit -> Edit -> Edit -> (Label, EditMap, [Label])
gen_product (n_e, base, n_x) e_a e_b e_m = 
  let prod_base = M.mapWithKey (generate_product' e_a e_b e_m) base
  in (n_e, prod_base, n_x)
  
generate_product' :: Edit -> Edit -> Edit -> Label -> (Stat, [Label]) -> (ProdProgram, Bool)
generate_product' e_a e_b e_m node_label node_stat@(stat, succs) =
  let prog_a' = M.lookup node_label e_a
      prog_b' = M.lookup node_label e_b
      prog_m' = M.lookup node_label e_m
      (b, prog_m@(m_entry, m_prog, m_exit)) =
       case prog_m' of
        Nothing -> (False, (node_label, M.fromList [(node_label, node_stat)], succs))
        Just m_edit -> (True, m_edit)
      succ_first =
       case M.lookup m_entry m_prog of
        Nothing -> error "generate_product': getting first"
        Just v -> snd v 
      id_edit = (node_label, M.fromList [(m_entry, (stat, succ_first))], succs)
      prog_a = fromMaybe id_edit prog_a'
      prog_b = fromMaybe id_edit prog_b'
      base = id_edit
  in (product (base, prog_a, prog_b, prog_m), b)

-- Flatten the product for each label in the base program
flatten_product :: Program -> (Label, EditMap, [Label]) -> ProdProgram
flatten_product (n_e, base, n_x) (_, mapToEdits, _) =
  let entry = n_e
      exit = n_x
      prog = M.foldWithKey (\k v m -> local_flatten_product k v m mapToEdits) M.empty base
  in (entry, prog, exit)
--  (n_e, flatten_product' n_e n_x base mapToEdits [] M.empty, n_x)

-- For each label in the original program, produce the product program
local_flatten_product :: Label -> (Stat, [Label]) -> ProdProg -> EditMap -> ProdProg
local_flatten_product n_c (stat, succs) prog editsMap =
 case M.lookup n_c editsMap of
  Nothing -> error "local_flatten_product"
  Just ((n_e, prodprog, n_x), ch) -> --trace ("flt: " ++ show (n_c, n_e, n_x, succs) ++ "\n") $ 
   if ch
   then
     let goto_e = [(n_c, replicate 4 (Skip, [n_e]))]
         goto_x = if n_x == succs
                  then []
                  else  map (\n_xx -> (n_xx, replicate 4 (Skip, succs))) n_x
         gotos = M.fromList $ goto_e ++ goto_x
     in M.union gotos $ M.union prodprog prog 
   else M.union prodprog prog 

get_fns :: ProdProgram -> [(Var, Int)]
get_fns (a, prog, b) = nub $ M.fold (\l r -> (concatMap get_fns_p l) ++ r) [] prog

get_fns_p :: (Stat, [Label]) -> [(Var, Int)]
get_fns_p = (get_fns_s . fst)

get_fns_s :: Stat -> [(Var, Int)]
get_fns_s s = case s of
  Skip -> []
  Assume e -> get_fns_e e 
  Assign v e -> get_fns_e e

get_fns_e :: Expr -> [(Var, Int)]
get_fns_e e = case e of 
  Op lhs _ rhs -> get_fns_e lhs ++ get_fns_e rhs
  C _ -> []
  V v -> []
  F v es -> (v, length es):(concatMap get_fns_e es)
  A v e -> get_fns_e e

get_vars :: ProdProgram -> Vars
get_vars (a, prog, b) = nub $ M.fold (\l r -> (concatMap get_vars_p l) ++ r) [] prog

get_vars_p :: (Stat, [Label]) -> Vars
get_vars_p = (get_vars_s . fst)

get_vars_s :: Stat -> Vars
get_vars_s s = case s of
  Skip -> []
  Assume e -> get_vars_e e
  Assign v e -> get_vars_lhs v ++ get_vars_e e

get_vars_lhs :: Lhs -> Vars
get_vars_lhs (LhsVar v) = [v]
get_vars_lhs (LhsArray v e) = v:(get_vars_e e)

get_vars_e :: Expr -> Vars
get_vars_e e = case e of
  Op lhs _ rhs -> get_vars_e lhs ++ get_vars_e rhs
  C _ -> []
  V v -> [v]
  F v es -> concatMap get_vars_e es
  A v e -> v:(get_vars_e e)

type VarMap = Map Var Vars

toVarMap :: Vars -> Map Var Vars
toVarMap =
  foldr (\v r -> M.insert v (variants_var v) r) M.empty

variants_var :: Var -> Vars
variants_var v = [v++"o", v++"a", v++"b", v++"m"]

-- Encoding for the product program P'
-- Start node: Eq of all vars implies the predicate of the start node
-- Similar for exit node: postcondition implies merge criteria.
--  For each edge e \in P', e = (base, a, b, m)
--   Define an encoding for each type of statements
main_encoding :: ProdProgram -> SMod
main_encoding prodprogram@(ne,prod,nx) =
  let vars = get_vars prodprogram
      varmap = toVarMap vars
      fns = ("get",2):get_fns prodprogram -- the get function is for arrays
      h = header prodprogram vars fns
      i = initial_state ne vars
      f = final_state nx vars
      prog = M.foldWithKey (encode_stat vars) [] prod
      csat = SE $ CheckSat
  in h ++ i ++ prog ++ f ++ [csat]

node_sig :: Label -> [SSortExpr] -> SExpression
node_sig n sig = SE $ DeclFun (SimpleSym $ "Q_"++n) sig (SymSort "Bool")

fn_sig :: (Var, Int) -> SExpression
fn_sig (fn_name, fn_arity) =
  let sig = replicate fn_arity $ SymSort "Int"
  in SE $ DeclFun (SimpleSym fn_name) sig (SymSort "Int")

header :: ProdProgram -> Vars -> [(Var, Int)] -> SMod
header (n_e,prodprogram,n_x) vars fns = 
  let nodes = nub $ n_e:(n_x ++ M.keys prodprogram)
      sig = concatMap (\v -> replicate 4 (SymSort "Int")) vars
      nodes_enc = map (\n -> node_sig n sig) nodes
      fns_enc = map fn_sig fns
      logic = setlogic HORN
  in logic:(fns_enc ++ nodes_enc)

initial_state :: Label -> Vars -> SMod
initial_state ne vars = 
  let _vars = toVarMap vars
      vars_ = concat $ M.elems _vars
      and_vars = map (\[a,b,c,d] -> [mk_eq a b, mk_eq b c, mk_eq c d]) $ M.elems _vars
      vars_for = map (\v -> (SimpleSym v, "Int")) vars_
      pre = FnAppExpr (SymIdent $ SimpleSym "and") $ concat and_vars 
      post = encode_Q vars_ ne
      for_expr = mk_e "=>" pre post
  in [SE $ Assert $ ForallExpr vars_for for_expr]

encode_Q :: Vars -> Label -> SExpr
encode_Q vars label =
  let q_label = SymIdent $ SimpleSym $ "Q_" ++ label
      enc_vars = map to_var vars
  in FnAppExpr q_label enc_vars

get_var :: Lhs -> Var
get_var lhs = case lhs of
  LhsVar v -> v
  LhsArray v _ -> v

mod_var :: Stat -> String -> Vars
mod_var (Assign v _) e = [get_var v ++ e ++ "1"]
mod_var _ _ = []

encode_s :: Stat -> String -> (Maybe SExpr, Maybe (Var, Var, Var))
encode_s Skip _version = (Nothing, Nothing)
encode_s (Assume e) _version = (Just $ encode_e e _version, Nothing) 
encode_s (Assign (LhsVar v) e) _version =
 let s_e = encode_e e _version
     var = v ++ _version ++ "1"
     a_enc = mk_e "=" (to_var var) s_e 
 in (Just a_enc, Just (v, v++_version, var))
encode_s (Assign (LhsArray v ev) e) _version = undefined
-- let s_e = encode_e e _version
--     var = v ++ _version ++ "1"
--     a_enc = mk_e "=" (to_var var) s_e 
-- in (Just a_enc, Just (v, v++_version, var))

encode_e :: Expr -> String -> SExpr
encode_e e _version = case e of
  C i -> LitExpr $ NumLit i
  V var -> IdentExpr $ SymIdent $ SimpleSym $ var ++ _version
  Op lhs opcode rhs ->
    let lhs_e = encode_e lhs _version
        rhs_e = encode_e rhs _version
        op_s = toOpcode opcode
    in case opcode of
      Neq -> let eq_e = FnAppExpr (SymIdent $ SimpleSym "=") [lhs_e, rhs_e]
             in FnAppExpr (SymIdent $ SimpleSym "not") [eq_e]
      _ -> FnAppExpr (SymIdent $ SimpleSym op_s) [lhs_e, rhs_e]
  F v es ->
    let es_e = map (\e -> encode_e e _version) es 
    in FnAppExpr (SymIdent $ SimpleSym v) es_e 
  A v e ->
    let s_v = IdentExpr $ SymIdent $ SimpleSym $ v ++ _version
        enc_e = encode_e e _version
    in FnAppExpr (SymIdent $ SimpleSym "get") [s_v, enc_e]

toOpcode :: OpCode -> String
toOpcode op = case op of
  And -> "and"
  Or -> "or"
  Add -> "+"
  Sub -> "-"
  Mult -> "*"
  Le -> "<"
  Ge -> ">"
  Leq -> "<="
  Geq -> ">="
  Eq -> "="
  _ -> error $ "toOpcode: " ++ show op ++ " not supported" 

replace :: Eq a => a -> a -> [a] -> [a]
replace a x' [] = []
replace a x' (x:xs)
 | x' == x = a:xs
 | otherwise = x:(replace a x' xs)
                
s_subst :: Maybe (Var, Var, Var) -> VarMap -> VarMap
s_subst Nothing varmap = varmap
s_subst (Just (x,x_k,nx_k)) varmap = M.update (\l -> Just $ replace nx_k x_k l) x varmap

encode_stat :: Vars -> Label -> [(Stat, [Label])] -> SMod -> SMod
encode_stat vars n_e [(s_o,n_o), (s_a,n_a), (s_b,n_b), (s_c,n_c)] rest =
  let _vars = toVarMap vars -- [[xa, xb, xc, xd], ... ]
      vars_ = concat $ M.elems _vars
      preQ = encode_Q vars_ n_e
      (s_o_e, v_o) = encode_s s_o "o"
      (s_a_e, v_a) = encode_s s_a "a"
      (s_b_e, v_b) = encode_s s_b "b"
      (s_c_e, v_c) = encode_s s_c "m"
      _vars' = concat $ M.elems $ foldr s_subst _vars [v_o, v_a, v_b, v_c]
      preStat = foldl (\r s_k_e -> maybe [] (:[]) s_k_e ++ r) [preQ] [s_o_e, s_a_e, s_b_e, s_c_e]
      pre = if length preStat == 1
            then head $ preStat
            else FnAppExpr (SymIdent $ SimpleSym "and") preStat 
      vars_for = map (\v -> (SimpleSym v, "Int")) $ nub $ vars_ ++ _vars'
      succ_labels = nub [n_o, n_a, n_b, n_c]
  in if succ_labels == [n_o]
     then
       let postQ' = map (encode_Q _vars') n_o
           ass = map (\post -> SE $ Assert $ ForallExpr vars_for $ mk_e "=>" pre post) postQ'
       in ass ++ rest
     else error $ "encode_stat: label error " ++ show (n_e, succ_labels, n_o) 

--to_var a = FnAppExpr (SymIdent $ SimpleSym a) []
to_var a = IdentExpr $ SymIdent $ SimpleSym a
mk_e op a b = FnAppExpr (SymIdent $ SimpleSym op)
  [a, b]

mk_eq a b = mk_e "=" (to_var a) (to_var b)
mk_or a b = mk_e "or" a b 
mk_ors l = FnAppExpr (SymIdent $ SimpleSym "or") l
mknot l =  FnAppExpr (SymIdent $ SimpleSym "not") [l]

-- condition
exit_condition :: Vars -> SExpr
exit_condition [xo,xa,xb,xc] =
  FnAppExpr (SymIdent $ SimpleSym "and") [mk_or (mk_eq xo xa) (mk_eq xc xa)
                                         ,mk_or (mk_eq xo xb) (mk_eq xc xb)
                                         ,mk_ors [mknot $ mk_eq xo xa
                                                 ,mknot $ mk_eq xo xb
--                                                 ,mknot $ mk_eq xc xo]
                                                 , (mk_eq xc xo)]
                                         ] 
  
final_state :: [Label] -> Vars -> SMod
final_state nx vars =
  foldr (\n_x r -> single_final_state n_x vars ++ r) [] nx

single_final_state :: Label -> Vars -> SMod
single_final_state nx vars =
  let node_x = SimpleSym $ "Q_"++nx
      _vars = toVarMap vars -- [[xa, xb, xc, xd], ...]
      vars_ = concat $ M.elems _vars
      vars_for = map (\v -> (SimpleSym v, "Int")) vars_
      pre = encode_Q vars_ nx
      -- post condition
      and_vars = map (\l -> exit_condition l) $ M.elems _vars
      post = FnAppExpr (SymIdent $ SimpleSym "and") and_vars 
      for_expr = FnAppExpr (SymIdent $ SimpleSym "=>") [pre, post]
  in [SE $ Assert $ ForallExpr vars_for for_expr]
