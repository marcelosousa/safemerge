{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Encoding where

import Data.List hiding (product)
import qualified Data.Map as M
import Data.Maybe
import Data.Map (Map)
import Debug.Trace
import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer
import Prelude hiding (product)
import Printer
import Product
import Types
import Util

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
data EncodeOpt = Whole | Fine
 deriving Eq

encode :: Program -> Edit -> Edit -> Edit -> SMod
encode base a b m = main_encoding Whole $ generate_product base a b m

fine_encode :: Program -> Edit -> Edit -> Edit -> SMod
fine_encode base a b m = main_encoding Fine $ generate_product base a b m

data VarType = SimpleVar | ArrayVar
  deriving Eq

type VarMap = Map Var (VarType, Vars)

toVarMap :: Variables -> VarMap
toVarMap (svars, avars) =
  let svars_ = foldr (\v -> M.insert v (SimpleVar, variants_var v)) M.empty svars 
      avars_ = foldr (\v -> M.insert v (ArrayVar, variants_var v)) svars_ avars
  in avars_

variants_var :: Var -> Vars
variants_var v = [v++"o", v++"a", v++"b", v++"m"]
-- Encoding for the product program P'
-- Start node: Eq of all vars implies the predicate of the start node
-- Similar for exit node: postcondition implies merge criteria.
--  For each edge e \in P', e = (base, a, b, m)
--   Define an encoding for each type of statements
main_encoding :: EncodeOpt -> (ProdProgram, [Label]) -> SMod
main_encoding opt (prodprogram@(ne,prod,nx), checks) = -- trace ("CHECKS AT " ++ show checks) $
  let vars = getVariable prodprogram
      fns = getFunctionSig prodprogram
      -- Relevant Encoding Functions
      h = header prodprogram vars fns
      i = initial_state ne vars
      prog = M.foldWithKey (encode_stat vars) [] prod
      f = if opt == Whole
          then final_state nx vars
          else final_state (nub $ nx ++ checks) vars
        --  else final_state nx vars
      csat = SE $ CheckSat
  in h ++ i ++ prog ++ f ++ [csat]

-- Header of the SMT File:
--  Function declarations.
--  Signatures for each node. 
header :: ProdProgram -> Variables -> [FunctionSig] -> SMod
header (n_e,prodprogram,n_x) vars fns = 
  let nodes = nub $ n_e:(n_x ++ M.keys prodprogram)
      sig = encode_vars vars 
      nodes_enc = map (\n -> node_sig n sig) nodes
      fns_enc = map fn_sig fns
      logic = setlogic HORN
  in logic:(fns_enc ++ nodes_enc)
 where node_sig :: Label -> [SSortExpr] -> SExpression
       node_sig n sig = SE $ DeclFun (SimpleSym $ "Q_"++n) sig (SymSort "Bool")

       fn_sig :: (Var, Int) -> SExpression
       fn_sig (fn_name, fn_arity) =
         let sig = replicate fn_arity $ SymSort "Int"
         in SE $ DeclFun (SimpleSym fn_name) sig (SymSort "Int")
       
       encode_vars :: Variables -> [SSortExpr]
       encode_vars (svars, avars) =
         let svars_e = concatMap (\_ -> replicate 4 (SymSort "Int")) svars
             avars_e = concatMap (\_ -> replicate 4 (SymSort "(Array Int Int)")) avars
         in svars_e ++ avars_e

-- VC for the initial state.
initial_state :: Label -> Variables -> SMod
initial_state ne vars = 
  let _vars = toVarMap vars
      vars_ = concatMap (\(v,l) -> map (\k -> (v,k)) l) $ M.elems _vars
      --and_vars = map (\[a,b,c,d] -> [mk_eq a b, mk_eq b c, mk_eq c d]) $ M.elems _vars 
      and_vars = map enc_begin $ M.elems _vars 
      vars_for = map enc_var vars_
      pre = FnAppExpr (SymIdent $ SimpleSym "and") $ concat and_vars 
      post = encode_Q (snd $ unzip $ vars_) ne
      for_expr = mk_e "=>" pre post
  in [SE $ Assert $ ForallExpr vars_for for_expr]
 where enc_begin :: (VarType, [Var]) -> [SExpr]
       enc_begin (SimpleVar, [a,b,c,d]) = [mk_eq a b, mk_eq b c, mk_eq c d]
       enc_begin (ArrayVar, [a,b,c,d]) = [mk_eq_arr a b, mk_eq_arr b c, mk_eq_arr c d] 

-- Given the four statements return the list of boolean conditions in
-- assumes and also the number of assignments. We have a condition that states
-- that we cannot have assumes with assigments.
partitionStat :: [(Stat,String)] -> ([(Expr,String)],Int)
partitionStat = foldr (\(s,str) (es,n) ->
   case s of
      Assume e -> ((e,str):es,n)
      Assign _ _ -> (es,n+1)
      _ -> (es,n)) ([],0)

-- Encoding of a 4-way statement
encode_stat :: Variables -> Label -> [(Stat, [Label])] -> SMod -> SMod
encode_stat vars n_e [(s_o,n_o), (s_a,n_a), (s_b,n_b), (s_c,n_c)] rest =
  let (g_es, a_nr) = partitionStat [(s_o,"o"),(s_a,"a"),(s_b,"b"),(s_c,"m")]
      guard_cond =
        if g_es == []
        then mkTrue
        else if a_nr > 0
             then error "Syntactic criteria of assume with assignment in product is not satisfied"
             else if length g_es == 1
                  then uncurry encode_e $ head g_es
                  else mkEqs $ map (\(a,b) -> encode_e a b) g_es 
      _vars = toVarMap vars -- [[xa, xb, xc, xd], ... ]
      vars_ = concatMap (\(v,l) -> map (\k -> (v,k)) l) $ M.elems _vars
      preQ = encode_Q (snd $ unzip $ vars_) n_e
      (s_o_e, v_o) = encode_s s_o "o"
      (s_a_e, v_a) = encode_s s_a "a"
      (s_b_e, v_b) = encode_s s_b "b"
      (s_c_e, v_c) = encode_s s_c "m"
      _vars' = concatMap (\(v,l) -> map (\k -> (v,k)) l) $ M.elems $ foldr s_subst _vars [v_o, v_a, v_b, v_c]
      preStat = foldl (\r s_k_e -> maybe [] (:[]) s_k_e ++ r) [preQ] [s_o_e, s_a_e, s_b_e, s_c_e]
      pre = if length preStat == 1
            then head $ preStat
            else FnAppExpr (SymIdent $ SimpleSym "and") preStat 
      vars_for = map enc_var $ nub $ vars_ ++ _vars'
      succ_labels = nub [n_o, n_a, n_b, n_c]
      guard = SE $ Assert $ ForallExpr vars_for $ mkImplies preQ guard_cond 
  in if succ_labels == [n_o]
     then let postQ' = map (encode_Q (snd $ unzip $ _vars')) n_o
              ass = map (\post -> SE $ Assert $ ForallExpr vars_for $ mk_e "=>" pre post) postQ'
          in guard:ass ++ rest
          --in ass ++ rest
     else error $ "encode_stat: label error " ++ show (n_e, succ_labels, n_o) 
 where s_subst :: Maybe (VarType, Var, Var, Var) -> VarMap -> VarMap
       s_subst Nothing varmap = varmap
       s_subst (Just (x_ty, x, x_k, nx_k)) varmap = M.update (\(_,l) -> Just $ (x_ty, replace nx_k x_k l)) x varmap
       
       replace :: Eq a => a -> a -> [a] -> [a]
       replace a x' [] = []
       replace a x' (x:xs)
        | x' == x = a:xs
        | otherwise = x:(replace a x' xs)
                
-- Generate the final state  
final_state :: [Label] -> Variables -> SMod
final_state nx vars =
  foldr (\n_x r -> single_final_state n_x vars ++ r) [] nx
  where single_final_state :: Label -> Variables -> SMod
        single_final_state nx vars =
          let node_x = SimpleSym $ "Q_"++nx
              _vars = toVarMap vars 
              vars_ = concatMap (\(v,l) -> map (\k -> (v,k)) l) $ M.elems _vars
              vars_for = map enc_var vars_
              pre = encode_Q (snd $ unzip $ vars_) nx
              -- post condition
              and_vars = map (\l -> exit_condition l) $ M.elems _vars
              post = FnAppExpr (SymIdent $ SimpleSym "and") and_vars 
              for_expr = FnAppExpr (SymIdent $ SimpleSym "=>") [pre, post]
          in [SE $ Assert $ ForallExpr vars_for for_expr]
        -- condition
        exit_condition :: (VarType, Vars) -> SExpr
        exit_condition (SimpleVar, [xo,xa,xb,xc]) =
          FnAppExpr (SymIdent $ SimpleSym "and") [mk_or (mk_eq xo xa) (mk_eq xc xa)
                                                 ,mk_or (mk_eq xo xb) (mk_eq xc xb)
                                                 ,mk_ors [mknot $ mk_eq xo xa
                                                         ,mknot $ mk_eq xo xb
                                                         ,mk_eq xc xo]
                                                 ] 
        exit_condition (ArrayVar, [xo,xa,xb,xc]) =
          FnAppExpr (SymIdent $ SimpleSym "and") [mk_or (mk_eq_arr xo xa) (mk_eq_arr xc xa)
                                                 ,mk_or (mk_eq_arr xo xb) (mk_eq_arr xc xb)
                                                 ,mk_ors [mknot $ mk_eq_arr xo xa
                                                         ,mknot $ mk_eq_arr xo xb
                                                         ,mk_eq_arr xc xo]
                                                 ] 

mk_eq_arr :: Var -> Var -> SExpr
mk_eq_arr l r = 
  let i = to_var "i"
      l_e = to_var l
      r_e = to_var r
      lhs = FnAppExpr (SymIdent $ SimpleSym "select") [l_e, i]
      rhs = FnAppExpr (SymIdent $ SimpleSym "select") [r_e, i]
      for_expr = mk_e "=" lhs rhs 
  in ForallExpr [(SimpleSym "i", "Int")] for_expr
 
-- Encode the Node
encode_Q :: Vars -> Label -> SExpr
encode_Q vars label =
  let q_label = SymIdent $ SimpleSym $ "Q_" ++ label
      enc_vars = map to_var vars
  in FnAppExpr q_label enc_vars

-- Encoding of a statement
encode_s :: Stat -> String -> (Maybe SExpr, Maybe (VarType, Var, Var, Var))
encode_s Skip _version = (Nothing, Nothing)
encode_s (Assume e) _version = (Just $ encode_e e _version, Nothing) 
encode_s (Assign (LhsVar v) e) _version =
 let s_e = encode_e e _version
     var = v ++ _version ++ "1"
     a_enc = mk_e "=" (to_var var) s_e 
 in (Just a_enc, Just (SimpleVar, v, v++_version, var))
encode_s (Assign (LhsArray v ev) e) _version = 
 let s_e = encode_e e _version
     ev_e = encode_e ev _version
     var = v ++ _version ++ "1"
     array_var = to_var $ v ++ _version
     array_var_mod = to_var var
     store = FnAppExpr (SymIdent $ SimpleSym "store") [array_var, ev_e, s_e]
     a_enc = mk_e "=" store array_var_mod 
 in (Just a_enc, Just (ArrayVar, v, v++_version, var))

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
    in case es_e of
      [] -> IdentExpr $ SymIdent $ SimpleSym v 
      _ -> FnAppExpr (SymIdent $ SimpleSym v) es_e 
  A v e ->
    let s_v = IdentExpr $ SymIdent $ SimpleSym $ v ++ _version
        enc_e = encode_e e _version
    in FnAppExpr (SymIdent $ SimpleSym "select") [s_v, enc_e]

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
  Mod -> "mod"
  Div -> "div"
  _ -> error $ "toOpcode: " ++ show op ++ " not supported" 

to_var a = IdentExpr $ SymIdent $ SimpleSym a
mk_e op a b = FnAppExpr (SymIdent $ SimpleSym op)
  [a, b]

mkTrue = IdentExpr $ SymIdent $ SimpleSym "true"
mkImplies a b = mk_e "=>" a b
mkEquiv a b = mk_e "=" a b
mkAnd l = FnAppExpr (SymIdent $ SimpleSym "and") l
mkEqs l = FnAppExpr (SymIdent $ SimpleSym "=") l

mk_eq a b = mk_e "=" (to_var a) (to_var b)
mk_or a b = mk_e "or" a b 
mk_ors l = FnAppExpr (SymIdent $ SimpleSym "or") l
mknot l =  FnAppExpr (SymIdent $ SimpleSym "not") [l]

enc_var :: (VarType, Var) -> (SSymbol, String)
enc_var (SimpleVar, v) = (SimpleSym v, "Int")
enc_var (ArrayVar, v) = (SimpleSym v, "(Array Int Int)")

