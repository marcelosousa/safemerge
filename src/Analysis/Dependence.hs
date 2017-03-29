{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Dependence
-- Copyright :  (c) 2017 Marcelo Sousa
-- Computes the output-input dependence analysis per method:
--   It produces per symbolic outputs, the set of inputs that might affect
--   that value.
-------------------------------------------------------------------------------
module Analysis.Dependence where

import Control.Monad.State.Lazy
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Edit.Types
import Graph
import Flow 
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace as T

-- Domain of analysis
-- Symbolic Locations
data SymLoc = SField Ident | SName Name | SArray ArrayIndex
  deriving (Show,Ord,Eq)

data Tag = Input | Output | Both | None
  deriving (Show,Ord,Eq)

join_tag :: Tag -> Tag -> Tag
join_tag None b = b
join_tag a None = a
join_tag Both b = Both
join_tag b Both = Both
join_tag a b =
  if a == b
  then a
  else Both

type AbsVar = SymLoc
type AbsVarAnn = (SymLoc,Tag)

-- This only works for a flow insensitive analysis
-- Need a richer domain for flow sensitivity
-- DepMap is the dependence map
-- If x -> {y,z}, it means that the 
-- the value of x depends on the values 
-- of y and z. 
type DepMap = Map AbsVar (Tag,[AbsVar])

join_depmap :: DepMap -> DepMap -> DepMap
join_depmap = M.unionWith (\(ta,a) (tb,b) -> 
  (ta `join_tag` tb, nub $ a ++ b)) 

type DepGraph = Graph DepMap 
type WItem = (NodeId,EdgeId,NodeId)
type Worklist = [WItem]
type NodeTable = Map NodeId [DepMap]
type ResultList = NodeTable -- Map NodeId DepMap

printResultList :: ResultList -> String
printResultList m = 
  "ResultList\n" ++ M.foldWithKey (\nId d r -> "Node " ++ show nId ++ "\n" ++ concatMap printDepMap d ++ "\n" ++ r) "" m

printDepMap :: DepMap -> String
printDepMap = 
  M.foldWithKey (\k (t,d) r -> show (t,k) ++ " -> " ++ show d ++ "\n" ++ r) "" 

-- State of the fixpoint
data FixState =
  FixState
  {
    fs_cfg  :: DepGraph 
  }

type FixOp val = State FixState val

-- | updates the information in the CFG 
update_node_table :: NodeTable -> FixOp DepGraph 
update_node_table node_table' = do 
  fs@FixState{..} <- get
  let cfg = fs_cfg { node_table = node_table' }
  put fs { fs_cfg = cfg }
  return cfg

type DepInfo = Map MIdent ResultList

printDepInfo :: DepInfo -> String
printDepInfo = 
  M.foldWithKey (\k l r -> "Function " ++ show k ++ "\n" ++ printResultList l ++ "\n\n" ++ r ) ""
 
-- compute dependencies for a module
depAnalysis :: FlowInfo DepMap -> DepInfo
depAnalysis = M.map dependencies 

dependencies :: (MemberDecl,DepGraph) -> ResultList
dependencies (mDecl,cfg) = 
  let initMap = initDepMap mDecl
  in fixpt cfg initMap

initDepMap :: MemberDecl -> DepMap
initDepMap mDecl = case mDecl of
  MethodDecl    _ _ _ _ params _ _ -> paramsToDep params 
  ConstructorDecl _ _ _ params _ _ -> paramsToDep params
  _ -> M.empty 

paramsToDep :: [FormalParam] -> DepMap
paramsToDep params = 
  M.fromList $ map paramToDep params 

paramToDep :: FormalParam -> (AbsVar,(Tag,[AbsVar]))
paramToDep (FormalParam a b c varId) = 
  case varId of
    VarId i -> (SName (Name [i]),(Input,[]))
    VarDeclArray vId -> paramToDep $ FormalParam a b c vId

-- The main fixpoint for the dependence analysis
-- It requires the initial DepMap that specifies the inputs
fixpt :: DepGraph -> DepMap -> ResultList 
fixpt cfg@Graph{..} initMap =
  -- reset the node table information with the information passed
  let node_table' = M.insert entry_node [initMap] $ M.map (const []) node_table
      cfg' = cfg { node_table = node_table' }
      wlist = map (\(a,b) -> (entry_node,a,b)) $ succs cfg' entry_node
      i_fix_st = FixState cfg' 
  in evalState (worklist wlist) i_fix_st

-- standard worklist algorithm
--  we have reached a fixpoint when the worklist is empty
worklist :: Worklist -> FixOp ResultList 
worklist _wlist = do
  fs@FixState{..} <- get
  case _wlist of
    [] -> return $ node_table fs_cfg 
    (it@(pre,eId,post):wlist) -> do
      -- get the current state in the pre
      let depMap = case get_node_info fs_cfg pre of
            [s] -> s
            l   -> error $ "worklist invalid pre states: " ++ show l
          -- get the edge info
          e@EdgeInfo{..} = get_edge_info fs_cfg eId
          _depMap = transformer edge_code depMap
          rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
      (is_fix,node_table') <-
           case edge_tags of
             -- loop head point 
             [LoopHead] -> return $ weak_update (node_table fs_cfg) post _depMap 
             -- join point: join the info in the cfg 
             [IfJoin] -> return $ weak_update (node_table fs_cfg) post _depMap 
             -- considering everything else the standard one: just replace 
             -- the information in the cfg and add the succs of post to the worklist
             _ -> return $ weak_update (node_table fs_cfg) post _depMap 
      cfg' <- update_node_table node_table'
      -- depending on the tags of the edge; the behaviour is different
      let nwlist = if is_fix || (Exit `elem` edge_tags) 
                   then wlist 
                   else (wlist ++ rwlst)
      worklist nwlist

weak_update :: NodeTable -> NodeId -> DepMap -> (Bool, NodeTable)
weak_update node_table node depMap =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [depMap] node_table)
    Just lst -> case lst of
      [] -> (False, M.insert node [depMap] node_table)
      [depMap'] ->
        let _depMap = depMap `join_depmap` depMap'
        in if _depMap == depMap'
           then (True, node_table)
           else (False, M.insert node [_depMap] node_table)
      _ -> error "join_update: more than one state in the list"
 
strong_update :: NodeTable -> NodeId -> DepMap -> (Bool, NodeTable)
strong_update node_table node depMap = 
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [depMap] node_table) 
    Just lst -> case lst of
      [] -> (False, M.insert node [depMap] node_table)
      [depMap'] ->
        if depMap == depMap'
        then (True, node_table)
        else (False, M.insert node [depMap] node_table)
      _ -> error "strong_update: more than one state in the list" 

transformer :: Stmt -> DepMap -> DepMap
transformer stmt map = case stmt of
  Skip -> map
  Return _e -> case _e of 
    Nothing -> map
    Just e -> 
      let (r,w) = transformer_expr e
      in if null w
         then foldr set_output map r 
         else error $ "transformer: Write set of return should be empty, " ++ show w 
  ExpStmt e -> 
    let (r,w) = transformer_expr e
    in foldr (set_dep r) map w 
  Assume e -> 
    let (r,w) = transformer_expr e
    in foldr (set_dep r) map w 
  _ -> error $ "transformer: " ++ show stmt

set_output :: AbsVar -> DepMap -> DepMap 
set_output v m = T.trace ("set_output: " ++ show v) $  
  case M.lookup v m of
    Nothing -> M.insert v (Output,[]) m
    Just (t,l) -> M.insert v (t `join_tag` Output,l) m  

set_dep :: [AbsVar] -> AbsVarAnn -> DepMap -> DepMap
set_dep r (w,t) m = M.insert w (t,r) m
 
-- transformer_expr: generates the read and write set of an expression
transformer_expr :: Exp -> ([AbsVar], [AbsVarAnn])
transformer_expr e = case e of
  Assign lhs op rhs -> 
    let writeSet = getWrite lhs
        readSet = getReadSet rhs
    in case op of
      EqualA -> (readSet, [writeSet])
      _ -> (fst writeSet:readSet, [writeSet])
  _ -> (getReadSet e,[]) 

getWrite :: Lhs -> AbsVarAnn
getWrite lhs = case lhs of
  NameLhs  n  -> (SName n,None) 
  FieldLhs fa -> case fa of
    PrimaryFieldAccess e i -> (SField i,Both)
    SuperFieldAccess i     -> (SField i,Both)
    ClassFieldAccess n i   -> (SField i,Both) 
  ArrayLhs ai -> (SArray ai,None) 

getReadSet :: Exp -> [AbsVar]
getReadSet e = case e of
  Lit l  -> [] 
  Nondet -> []
  ClassLit m     -> error $ "getReadSet: " ++ show e 
  This           -> error $ "getReadSet: " ++ show e 
  ThisClass name -> error $ "getReadSet: " ++ show e  
  -- [TypeArgument] ClassType [Argument] (Maybe ClassBody)
  InstanceCreation tyargs classty args mclass -> concatMap getReadSet args 
  -- Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
  QualInstanceCreation e tyargs classty args mclass -> error $ "getReadSet: " ++ show e
  -- Type [Exp] Int
  ArrayCreate ty   exps  n   -> error $ "getReadSet: " ++ show e
  -- Type Int ArrayInit
  ArrayCreateInit ty n ainit -> error $ "getReadSet: " ++ show e
  FieldAccess fa -> case fa of
    PrimaryFieldAccess e i -> [SField i]
    SuperFieldAccess i     -> [SField i]
    ClassFieldAccess n i   -> [SField i]
  MethodInv mi -> getReadSetInv mi 
  ArrayAccess ai -> [SArray ai]
  ExpName n -> [SName n]
  PostIncrement exp -> getReadSet exp
  PostDecrement exp -> getReadSet exp
  PreIncrement  exp -> getReadSet exp
  PreDecrement  exp -> getReadSet exp
  PrePlus       exp -> getReadSet exp
  PreMinus      exp -> getReadSet exp
  PreBitCompl   exp -> getReadSet exp
  PreNot        exp -> getReadSet exp
  Cast     ty   exp -> getReadSet exp
  BinOp  lhs op rhs -> getReadSet lhs ++ getReadSet rhs
  InstanceOf    exp refType -> getReadSet exp
  Cond c t e -> getReadSet c ++ getReadSet t ++ getReadSet e
  Assign lhs assignOp rhs -> error $ "getReadSet of " ++ show e 

-- @TODO: Method invocation can affect the write set
getReadSetInv :: MethodInvocation -> [AbsVar]
getReadSetInv mi = case mi of
  MethodCall n args -> concatMap getReadSet args
  PrimaryMethodCall e _ _ args -> getReadSet e ++ concatMap getReadSet args
  _ -> error $ "getReadSetInv: " ++ show mi 
