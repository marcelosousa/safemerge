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

import Analysis.Java.ClassInfo
import Analysis.Java.Flow 
import Analysis.Java.Graph
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Debug.Trace as T
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe
import qualified Data.Map as M
import qualified Data.Set as S

-- Domain of analysis
-- Symbolic Locations
data SymLoc = SField Ident | SName Name | SArray ArrayIndex
  deriving (Show,Ord,Eq)

writable :: SymLoc -> Bool
writable sym = case sym of
  SName  n ->
   let Ident i = toIdent n 
   in not $ isUpper $ head i
  _ -> True

symLocToLhs :: SymLoc -> Lhs
symLocToLhs sym = case sym of
  SField i -> FieldLhs $ PrimaryFieldAccess This i
  SName  n -> NameLhs n
  SArray (ArrayIndex a _) -> case a of
    FieldAccess fa -> FieldLhs $ fa
    ExpName n -> NameLhs n
    _ -> error "symLocToLhs"

symLocToExp :: SymLoc -> Exp
symLocToExp sym = case sym of
  SField i -> FieldAccess $ PrimaryFieldAccess This i
  SName  n -> ExpName  n
  SArray a -> ArrayAccess  a 

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

is_output :: Tag -> Bool
is_output tg = 
  case tg of 
    Output -> True
    Both -> True
    _ -> False

is_input :: Tag -> Bool
is_input tg = 
  case tg of 
    Input -> True
    Both -> True
    _ -> False

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
join_depmap m1 m2 = M.unionWith (\(ta,a) (tb,b) -> (ta `join_tag` tb, nub $ a ++ b)) m1 m2 

-- | Given a dependence map, returns the set of
--   read global and write globals.
readWriteSet :: DepMap -> ([AbsVar], [AbsVarAnn])
readWriteSet dep =
  M.foldWithKey (\w (t,rs) (a,b) -> (a ++ rs, (w,t):b)) ([],[]) dep 

-- The context represents a stack of the variables involved in
-- the path condition
type Context = [[AbsVar]]
-- An abstract element is a pair of the context that represents
-- the path condition and the dependence map of the assignments
type AbsElem = (Context,DepMap)
-- The node table is an element of the DepGraph
type NodeTable = Map NodeId [AbsElem]
-- A dependence graph is a CFG annotated with the dependence info
type DepGraph = Graph AbsElem 

-- The result list is simply a NodeTable
type ResultList = NodeTable 
-- The FullDepInfo represents the full information 
--  of the dependence analysis of a set of methods
type FullDepInfo = Map MIdent ResultList 
-- The DepInfo represents the important information
--  of the dependence analysis of a set of methods
--  that is for each method, the dependence map
--  at the return node labelled with the (-1) identifier
type DepInfo = Map MIdent DepMap 

-- These are types related to the fixpoint computation
type WItem = (NodeId,EdgeId,NodeId)
type Worklist = [WItem]

-- pretty printing
printResultList :: ResultList -> String
printResultList m = 
  "ResultList\n" ++ M.foldWithKey (\nId d r -> "Node " ++ show nId ++ "\n" ++ concatMap printAbsElem d ++ "\n" ++ r) "" m

printAbsElem :: AbsElem -> String
printAbsElem (c, dep) = 
  let cString = "Context: " ++ show c ++ "\n"
  in cString ++ printDepMap dep

printDepMap :: DepMap -> String
printDepMap = 
  M.foldWithKey (\k (t,d) r -> show (t,k) ++ " -> " ++ show d ++ "\n" ++ r) "" 

printDepInfo :: DepInfo -> String
printDepInfo = 
  M.foldWithKey (\k l r -> "Function " ++ show k ++ "\n" ++ printDepMap l ++ "\n\n" ++ r ) ""
 
-- State of the fixpoint
data FixState =
  FixState
  {
    fs_cfg     :: DepGraph 
  , fs_cl_sum  :: ClassSum
  }

type FixOp val = State FixState val

-- | updates the information in the CFG 
update_node_table :: NodeTable -> FixOp DepGraph 
update_node_table node_table' = do 
  fs@FixState{..} <- get
  let cfg = fs_cfg { node_table = node_table' }
  put fs { fs_cfg = cfg }
  return cfg

-- compute dependencies for a module
depAnalysis :: ClassInfo -> FlowInfo AbsElem -> DepInfo
depAnalysis class_info = M.mapWithKey (memberDep class_info)

-- compute dependences for a member declaration (method or constructor)
--  returns the annotated CFG with the dependence map
dependencies :: ClassInfo -> MIdent -> (MemberDecl,DepGraph) -> ResultList
dependencies class_info mIdent (mDecl,cfg) = 
  let class_sum = findClass mIdent class_info 
      initMap = initDepMap mDecl
      initVal = ([],initMap)
  in fixpt class_sum cfg initVal 
 where
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

-- compute dependences for a member declaration (method or constructor)
--  returns only the dependence map associated with the exit node
memberDep :: ClassInfo -> MIdent ->  (MemberDecl,DepGraph) -> DepMap 
memberDep class_info mIdent (mDecl,cfg) = 
  let res = dependencies class_info mIdent (mDecl,cfg) 
  in case M.lookup (-1) res of
       Nothing -> M.empty 
       Just r  -> case r of
         [(_,x)] -> x
         _ -> error $ "memberDep: invalid result " ++ show r

-- computes dependences for a part of the CFG 
blockDep :: ClassSum -> DepGraph -> DepMap 
blockDep class_sum cfg = 
  let initVal = ([], M.empty)
      res = fixpt class_sum cfg initVal 
  in case M.lookup (-1) res of
       Nothing ->  M.empty 
       Just r  ->  case r of
         [(_,x)] ->  x
         _ -> error $ "memberDep: invalid result " ++ show r

-- The main fixpoint for the dependence analysis
-- It requires the initial DepMap that specifies the inputs
fixpt :: ClassSum -> DepGraph -> AbsElem -> ResultList 
fixpt class_sum cfg@Graph{..} initVal = 
  -- reset the node table information with the information passed
  let node_table' = M.insert entry_node [initVal] $ M.map (const []) node_table
      cfg' = cfg { node_table = node_table' }
      wlist = map (\(a,b) -> (entry_node,a,b)) $ succs cfg' entry_node
      i_fix_st = FixState cfg' class_sum 
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
      let absElem = case get_node_info fs_cfg pre of
            [s] -> s
            l   -> error $ "worklist invalid pre states: " ++ show l
          -- get the edge info
          e@EdgeInfo{..} = get_edge_info fs_cfg eId
          rwlst = map (\(a,b) -> (post,a,b)) $ succs fs_cfg post
          _absElem@(_cont,_depMap) = transformer edge_code fs_cl_sum absElem 
      (is_fix,node_table') <-
           case edge_tags of
             -- loop head point 
             [LoopHead] -> return $ weak_update (node_table fs_cfg) post _absElem 
             -- join point: join the info in the cfg 
             [IfJoin] -> return $ weak_update (node_table fs_cfg) post (tail _cont,_depMap)
             -- considering everything else the standard one: just replace 
             -- the information in the cfg and add the succs of post to the worklist
             _ -> return $ weak_update (node_table fs_cfg) post _absElem 
      cfg' <- update_node_table node_table'
      -- depending on the tags of the edge; the behaviour is different
      let nwlist = if is_fix || (Exit `elem` edge_tags) 
                   then wlist 
                   else (wlist ++ rwlst)
      worklist nwlist

-- | What if the context is different?
weak_update :: NodeTable -> NodeId -> AbsElem -> (Bool, NodeTable)
weak_update node_table node el@(cont,depMap) =
  case M.lookup node node_table of
    Nothing -> (False, M.insert node [el] node_table)
    Just lst -> case lst of
      [] -> (False, M.insert node [el] node_table)
      [(cont',depMap')] ->
        let _depMap = depMap `join_depmap` depMap'
        in if _depMap == depMap' 
           then (True, node_table)
           else (False, M.insert node [(cont,_depMap)] node_table)
      _ -> error "join_update: more than one state in the list"

transformer :: Stmt -> ClassSum -> AbsElem -> AbsElem 
transformer stmt class_sum el@(k,dmap) =  -- T.trace ("transformer: " ++ show stmt) $ 
  let kvars = nub $ concat k 
  in case stmt of
    Skip -> el 
    Return _e -> case _e of 
      Nothing -> el 
      Just e -> 
        let (r,w) = transformer_expr class_sum e
        in if null w
           then (k, foldr set_output dmap r) 
           else error $ "transformer: Write set of return should be empty, " ++ show w 
    ExpStmt e -> 
      let (r,w) = transformer_expr class_sum e
      in  (k, foldr (set_dep $ r ++ kvars) dmap w) 
    Assume e -> 
      let (r,w) = transformer_expr class_sum e
      in (r:k, foldr (set_dep $ r ++ kvars) dmap w) 
    Empty -> el
    Throw e -> 
      let (r,w) = transformer_expr class_sum e
      in  (k, foldr (set_dep $ r ++ kvars) dmap w) 
    _ -> error $ "transformer: " ++ show stmt

set_output :: AbsVar -> DepMap -> DepMap 
set_output v m = -- trace ("set_output: " ++ show v) $  
  case M.lookup v m of
    Nothing -> M.insert v (Output,[]) m
    Just (t,l) -> M.insert v (t `join_tag` Output,l) m  

set_dep :: [AbsVar] -> AbsVarAnn -> DepMap -> DepMap
set_dep r (w,t) m = -- T.trace ("set_dep: w = " ++ show w ++ ", r = " ++ show r) $ 
  m `join_depmap` (M.singleton w (t,r))
 
-- transformer_expr: generates the read and write set of an expression
transformer_expr :: ClassSum -> Exp -> ([AbsVar], [AbsVarAnn])
transformer_expr class_sum e = case e of
  Assign lhs op rhs -> 
    let (rSet,writeSet) = getWrite class_sum lhs
        (readSet,wSet) = transformer_expr class_sum rhs
    in case op of
      EqualA -> (readSet++rSet, writeSet++wSet)
      _ -> (map fst writeSet++readSet++rSet, writeSet)
  MethodInv m -> transformer_methInv class_sum m
  _ -> (getReadSet class_sum e,[]) 

-- | transformer for a method invocation
--   need to identify if it is an object method being invoked
transformer_methInv :: ClassSum -> MethodInvocation -> ([AbsVar], [AbsVarAnn])
transformer_methInv class_sum m =
  let rSet = getReadSetInv class_sum m 
      (mths,args) = case m of
        PrimaryMethodCall This [] i args -> (findMethodGen i (length args) class_sum, args) 
        _ -> ([],[])
      cfgs = map computeGraphMember mths
      deps = map (readWriteSet . blockDep class_sum) cfgs
      (r,w) = foldr (\(a,b) (c,d) -> (a++c, b++d)) ([],[]) deps
      -- the objects that are read can also be written
      wR = map (\a -> (a,None)) $ filter writable (rSet++r) 
  in (rSet++r, wR++w)

getWrite :: ClassSum -> Lhs -> ([AbsVar],[AbsVarAnn])
getWrite class_sum lhs = case lhs of
  NameLhs  n  -> 
    let i = toIdent n
    in case M.lookup i (_cl_fields class_sum) of
         Nothing -> ([],[(SName n,None)]) 
         Just m  -> ([],(SName n,None):(map (\(i,_) -> (SField i,Both)) $ toMemberSig m)) 
  FieldLhs fa -> case fa of
    PrimaryFieldAccess e i -> ([],[(SField i,Both)])
    SuperFieldAccess i     -> ([],[(SField i,Both)])
    ClassFieldAccess n i   -> ([],[(SField i,Both)])
  ArrayLhs ai@(ArrayIndex _ args) -> (concatMap (getReadSet class_sum) args,[(SArray ai,None)]) 

getReadSet :: ClassSum -> Exp -> [AbsVar]
getReadSet class_sum e = case e of
  Lit l  -> [] 
  Nondet -> []
  ClassLit m     -> [] -- error $ "getReadSet: " ++ show e 
  This           -> [] -- error $ "getReadSet: " ++ show e 
  ThisClass name -> error $ "getReadSet: " ++ show e  
  -- [TypeArgument] ClassType [Argument] (Maybe ClassBody)
  InstanceCreation tyargs classty args mclass -> concatMap (getReadSet class_sum) args 
  -- Exp [TypeArgument] Ident [Argument] (Maybe ClassBody)
  QualInstanceCreation e tyargs classty args mclass -> error $ "getReadSet: " ++ show e
  -- Type [Exp] Int
  ArrayCreate ty   exps  n   -> [] -- error $ "getReadSet: " ++ show e
  -- Type Int ArrayInit
  ArrayCreateInit ty n ainit -> [] -- error $ "getReadSet: " ++ show e
  FieldAccess fa -> case fa of
    PrimaryFieldAccess e i -> [SField i]
    SuperFieldAccess i     -> [SField i]
    ClassFieldAccess n i   -> [SField i]
  MethodInv mi -> getReadSetInv class_sum mi 
  ArrayAccess ai -> [SArray ai]
  ExpName n -> 
    let i = toIdent n
    in case M.lookup i (_cl_fields class_sum) of
        Nothing -> [SName n] 
        Just m  -> (SName n):(map (\(i,_) -> SField i) $ toMemberSig m) 
  PostIncrement exp -> getReadSet class_sum exp
  PostDecrement exp -> getReadSet class_sum exp
  PreIncrement  exp -> getReadSet class_sum exp
  PreDecrement  exp -> getReadSet class_sum exp
  PrePlus       exp -> getReadSet class_sum exp
  PreMinus      exp -> getReadSet class_sum exp
  PreBitCompl   exp -> getReadSet class_sum exp
  PreNot        exp -> getReadSet class_sum exp
  Cast     ty   exp -> getReadSet class_sum exp
  BinOp  lhs op rhs -> getReadSet class_sum lhs ++ getReadSet class_sum rhs
  InstanceOf    exp refType -> getReadSet class_sum exp
  Cond c t e -> getReadSet class_sum c ++ getReadSet class_sum t ++ getReadSet class_sum e
  Assign lhs assignOp rhs -> [] -- error $ "getReadSet of " ++ show e 

getReadSetInv :: ClassSum -> MethodInvocation -> [AbsVar]
getReadSetInv class_sum mi = case mi of
  MethodCall (Name n) args -> (getReadSetName n):(concatMap (getReadSet class_sum) args)
  PrimaryMethodCall e _ _ args -> (getReadSet class_sum) e ++ concatMap (getReadSet class_sum) args
  SuperMethodCall _ i args -> (getReadSetName [i]):(concatMap (getReadSet class_sum) args)
  _ -> error $ "getReadSetInv: " ++ show mi 

getReadSetName :: [Ident] -> AbsVar
getReadSetName [] = error "getReadSetInv"
getReadSetName (Ident i:_) = SName (Name [Ident i])
