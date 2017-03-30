{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DoAndIfThenElse #-}

{-
 Converts a Java CompilationUnit into a set of CFGs of methods 
-}
module Analysis.Java.Flow where

import Analysis.Java.ClassInfo
import Analysis.Java.Graph
import Control.Monad.State.Lazy
import Data.List 
import Data.Map (Map)
import Language.Java.Pretty
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Debug.Trace as T

type FlowInfo st = Map MIdent (MemberDecl,Graph st)

data FlowState st
  = FlowState {
    graphs  :: Graphs st
  , this    :: Graph  st
  , defs    :: Map MIdent MemberDecl 
  , entries :: Map MIdent NodeId
  , prev :: [NodeId] 
  , current :: NodeId 
  , next  :: [NodeId] 
  , exit :: Maybe NodeId 
  , pc_counter :: NodeId
  , edge_counter :: EdgeId 
  } deriving Show

init_st :: FlowState st
init_st = FlowState M.empty undefined M.empty M.empty [] 0 [] Nothing 0 0

type FlowOp val st = State (FlowState st) val

-- | API
addEdgeInfo :: EdgeId -> EdgeInfo -> FlowOp () st
addEdgeInfo eId eInfo = do
  p@FlowState{..} <- get
  let table = edge_table this
  case M.lookup eId table of
    Nothing -> do
      let new_table = M.insert eId eInfo table 
          this' = this {edge_table = new_table}
      put p {this = this'}
    Just info ->
      if eInfo == info
      then return ()
      else error "different info's: not sure what to do"
 
addEdge :: NodeId -> EdgeId -> NodeId -> FlowOp () st
addEdge nA eI nB = do 
  p@FlowState{..} <- get
  let gr = graph this
  case M.lookup nA gr of
    Nothing -> do
      let new_gr = M.insert nA [(eI,nB)] gr 
          this' = this {graph = new_gr}
      put p {this = this'}
    Just nodes -> do
      let n = nub $ (eI,nB):nodes
          new_gr = M.insert nA n gr
          this' = this {graph = new_gr}
      put p {this = this'}

addThis :: MIdent -> FlowOp () st
addThis sym = do 
  p@FlowState{..} <- get
  let graphs' = M.insert sym this graphs
  put p {graphs = graphs'}

replaceGraph :: Graph st -> FlowOp () st
replaceGraph graph = do
  p@FlowState{..} <- get
  put p {this = graph}
 
incCounter :: FlowOp Int st
incCounter = do
  p@FlowState{..} <- get
  let c' = pc_counter + 1
  put p {pc_counter = c'}
  return c'

incEdCounter :: FlowOp Int st
incEdCounter = do
  p@FlowState{..} <- get
  let c' = edge_counter + 1
  put p {edge_counter = c'}
  return c'

addEntry :: MIdent -> NodeId -> FlowOp () st
addEntry sym n = do
  p@FlowState{..} <- get
  let e = M.insert sym n entries
  put p {entries = e}

addDef :: MIdent -> MemberDecl -> FlowOp () st
addDef sym m = do
  p@FlowState{..} <- get
  let d = M.insertWithKey (\k nv ov -> error $ "addDef: value for key " ++ show k ++ " already in definitions") sym m defs 
  put p {defs = d}
 
getCurrent :: FlowOp NodeId st
getCurrent = do
  p@FlowState{..} <- get
  return current

getExit :: FlowOp (Maybe NodeId) st
getExit = do
  p@FlowState{..} <- get
  return exit 

replaceExit :: Maybe NodeId -> FlowOp () st
replaceExit n = do
  p@FlowState{..} <- get
  put p {exit = n}

replaceCurrent :: NodeId -> FlowOp () st
replaceCurrent n = do
  p@FlowState{..} <- get
  put p {current = n}

pushPrev :: NodeId -> FlowOp () st
pushPrev n = do
  p@FlowState{..} <- get
  let e = n:prev
  put p {prev = e}

popPrev :: FlowOp NodeId st
popPrev = do
  p@FlowState{..} <- get
  case prev of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {prev = xs}
      return x 

getPrev :: FlowOp NodeId st
getPrev = do
  p@FlowState{..} <- get
  case prev of
    [] -> error "cant get current: empty"
    (x:xs) -> return x
 
pushNext :: NodeId -> FlowOp () st
pushNext n = do
  p@FlowState{..} <- get
  let e = n:next 
  put p {next = e}

popNext :: FlowOp NodeId st
popNext = do
  p@FlowState{..} <- get
  case next of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {next = xs}
      return x 

getPCLabel :: MIdent -> FlowOp NodeId st
getPCLabel sym = do
  p@FlowState{..} <- get
  case M.lookup sym entries of
    Nothing -> do
      lab <- incCounter
      addEntry sym lab
      return lab 
 
getNext :: FlowOp NodeId st
getNext = do
  p@FlowState{..} <- get
  case next of
    [] -> error "cant get next: empty"
    (x:xs) -> return x
 
getEntryId :: MIdent -> FlowOp NodeId st
getEntryId sym = do
  p@FlowState{..} <- get
  case M.lookup sym entries of
    Nothing -> error "getEntryId"
    Just n  -> return n

-- | Main Functions
computeGraphs :: Program -> FlowInfo st
computeGraphs prog = 
  let ((),st) = runState (toFlow prog) init_st
  in M.intersectionWith (,) (defs st) (graphs st) 

-- | Main ToFlow class 
class Flow a v  where
  toFlow :: a -> FlowOp v st

-- | Flow Program 
instance Flow Program () where
  toFlow (CompilationUnit _ _ cls) = do 
    mapM_ computeEntry cls 
    p@FlowState{..} <- get
    --T.trace ("toFlow: defs = " ++ show defs) $     
    mapM_ computeGraph $ M.toList defs 

computeEntry :: TypeDecl -> FlowOp () st 
computeEntry decl = case decl of
  ClassTypeDecl     cl -> computeClDefs cl
  InterfaceTypeDecl it -> computeItDefs it 

computeClDefs :: ClassDecl -> FlowOp () st 
computeClDefs (ClassDecl _ cId _ _ _ (ClassBody bdy)) =
  mapM_ (computeDeclsDefs cId) bdy

computeDeclsDefs :: Ident -> Decl -> FlowOp () st 
computeDeclsDefs id decl = case decl of
  MemberDecl mdecl -> computeMDeclDefs id mdecl
  InitDecl _ _ -> error $ "computeDeclDefs: InitDecl unsupported"

computeItDefs :: InterfaceDecl -> FlowOp () st 
computeItDefs (InterfaceDecl _ iId _ _ (InterfaceBody bdy)) = 
  mapM_ (computeMDeclDefs iId) bdy

computeMDeclDefs :: Ident -> MemberDecl -> FlowOp () st 
computeMDeclDefs uId mDecl = case mDecl of
  FieldDecl _ _ _ -> return () 
  MethodDecl _ _ _ mId ps _ _ -> newEntry (uId,mId,map getParamType ps) mDecl 
  ConstructorDecl _ _ cId ps _ _ -> newEntry (uId,cId,map getParamType ps) mDecl
  MemberClassDecl c -> computeClDefs c
  MemberInterfaceDecl i -> computeItDefs i 

newEntry :: MIdent -> MemberDecl -> FlowOp () st
newEntry sym mDecl = do
  n <- incCounter
  addEntry sym n
  addDef sym mDecl  

computeGraph :: (MIdent,MemberDecl) -> FlowOp () st
computeGraph (sym,mdecl) = do 
  entry <- getEntryId sym
  let this = init_graph entry
  replaceGraph this
  replaceCurrent entry
  computeGraphBody mdecl 
  addThis sym 
  return ()

-- | computes the body of a method or constructor
computeGraphBody :: MemberDecl -> FlowOp () st
computeGraphBody mDecl = case mDecl of
  MethodDecl _ _ _ _ _ _ (MethodBody mBlock) -> case mBlock of
    Nothing -> return () 
    Just (Block block) -> do
      mapM_ computeGraphBStmt block
  ConstructorDecl _ _ sym _ _ (ConstructorBody mInv block) -> do
    let inv = invToStmt sym mInv
    mapM_ computeGraphStmt inv
    mapM_ computeGraphBStmt block 
  _ -> error $ "computeGraphBody: Not supported " ++ show mDecl

computeGraphBStmt :: BlockStmt -> FlowOp Bool st
computeGraphBStmt bstmt = case bstmt of
  BlockStmt stmt -> computeGraphStmt stmt 
  LocalClass _ -> error "computeGraphBStmt: LocalClass is not supported here"
  LocalVars _ _ vardecls -> do
   let varAssigns = map localToStmt vardecls
   mapM_ computeGraphStmt varAssigns 
   return False 

-- | Conversion to Stmt 
invToStmt :: Ident -> Maybe ExplConstrInv -> [Stmt]
invToStmt _ Nothing = []
invToStmt cId (Just inv) = 
  let name = Name [cId] 
  in case inv of
    ThisInvoke ty args -> [ExpStmt (MethodInv (MethodCall name args))]
    SuperInvoke ty args -> [ExpStmt (MethodInv (SuperMethodCall ty cId args))]
    PrimarySuperInvoke e ty args -> error $ "invToStmt: PrimarySuperInvoke not supported"

localToStmt :: VarDecl -> Stmt 
localToStmt (VarDecl varId varInit) =
  let lhs = NameLhs $ varDeclIdToName varId 
      rhs = case varInit of
        Nothing -> Lit $ Int 0
        Just i  -> case i of
          InitExp e -> e
          InitArray a -> Lit $ Int 0
  in ExpStmt $ Assign lhs EqualA rhs

varDeclIdToName :: VarDeclId -> Name
varDeclIdToName v = case v of
  VarId i -> Name [i]
  VarDeclArray v' -> varDeclIdToName v' 

computeGraphStmt :: Stmt -> FlowOp Bool st
computeGraphStmt stmt = do
  next <- incCounter
  pushNext next
  case stmt of 
    StmtBlock (Block block) -> do 
      bvals <- mapM computeGraphBStmt block
      popNext
      return $ or bvals
    Return mExpr -> do
      eId <- incEdCounter
      curr <- getCurrent
      let next = (-1) 
          eInfo = EdgeInfo [Exit] stmt
      addEdgeInfo eId eInfo
      addEdge curr eId next
      popNext
      return True
    ExpStmt e  -> computeGraphSimpleStmt stmt 
    Empty      -> computeGraphSimpleStmt stmt
    Assert a b -> computeGraphSimpleStmt stmt
    Assume a   -> computeGraphSimpleStmt stmt
    Skip       -> computeGraphSimpleStmt stmt
    IfThen cond tExp -> computeGraphStmt $ IfThenElse cond tExp Skip 
    IfThenElse cond tStmt eStmt -> do
      curr <- getCurrent
      next <- getNext -- Join point
      thenEdge <- incEdCounter
      elseEdge <- incEdCounter
      joinTEdge <- incEdCounter
      joinEEdge <- incEdCounter
      thenPc <- incCounter
      elsePc <- incCounter
      let eThen = EdgeInfo [CondTag] (Assume cond)
          eElse = EdgeInfo [CondTag] (Assume (PreNot cond))
          eJoin = EdgeInfo [IfJoin] Skip
      -- Add the edges from the branches
      addEdgeInfo thenEdge eThen
      addEdgeInfo elseEdge eElse
      addEdge curr thenEdge thenPc
      addEdge curr elseEdge elsePc
      -- Add the edges info for the joins
      addEdgeInfo joinTEdge eJoin 
      addEdgeInfo joinEEdge eJoin
      -- Execute then branch 
      replaceCurrent thenPc 
      bT <- computeGraphStmt tStmt
      -- Add the join edge from the then
      _ <- if not bT 
      then do
        curr <- getCurrent
        addEdge curr joinTEdge next
        return False
      else return False
      replaceCurrent elsePc
      bE <- computeGraphStmt eStmt
      if not bE 
      then do 
        curr <- getCurrent
        addEdge curr joinEEdge next
        replaceCurrent next
        popNext
        return False
      else do
        replaceCurrent next 
        popNext
        return False
    Break mIdent -> case mIdent of
      Nothing -> do
        eId <- incEdCounter
        curr <- getCurrent
        exit <- getExit
        next <- do
          case exit of
            Nothing -> getNext
            Just l  -> return l
        let eInfo = EdgeInfo [] Skip 
        addEdgeInfo eId eInfo
        addEdge curr eId next
        popNext
        return True 
      Just x  -> error $ "computeGraphStmt: Not supported " ++ show stmt
    Continue mIdent -> case mIdent of
      Nothing -> do
        edgeId <- incEdCounter
        curr <- getCurrent
        prev <- getPrev
        let eInfo = EdgeInfo [] Skip 
        addEdgeInfo edgeId eInfo
        addEdge curr edgeId prev
        popNext
        return True 
      Just x  -> error $ "computeGraphStmt: Not supported " ++ show stmt
    Do    stmt cond -> computeWhile cond stmt True 
    While cond stmt -> computeWhile cond stmt False 
    BasicFor init exp exps stmt -> computeFor init exp exps stmt
    -- Be careful with the case statements
    Switch cond body -> do
      eId <- incEdCounter
      curr <- getCurrent
      next <- incCounter
      prev_exit <- getExit
      prev_next <- getNext
      replaceExit (Just prev_next)
      let eInfo = EdgeInfo [] (Assume cond)
      addEdgeInfo eId eInfo
      addEdge curr eId next
      replaceCurrent next
      mapM_ computeSwitchBody body
      replaceExit prev_exit
      popNext
      return False
    _ -> error $ "computeGraphStmt: Not supported " ++ show stmt

computeGraphSimpleStmt :: Stmt -> FlowOp Bool st
computeGraphSimpleStmt stmt = do 
  edgeId <- incEdCounter
  curr <- getCurrent
  next <- getNext
  let eInfo = EdgeInfo [] stmt 
  addEdgeInfo edgeId eInfo
  addEdge curr edgeId next 
  replaceCurrent next
  popNext
  return False 

computeSwitchBody :: SwitchBlock -> FlowOp Bool st
computeSwitchBody (SwitchBlock lab bstmt) = 
  case lab of 
    Default -> do
      mapM_ computeGraphBStmt bstmt
      return False
    SwitchCase expr -> do
      eId <- incEdCounter
      curr <- getCurrent
      new <- incCounter
      replaceCurrent new
      let eInfo = EdgeInfo [] (ExpStmt expr)
      addEdgeInfo eId eInfo
      addEdge curr eId new
      mapM_ computeGraphBStmt bstmt 
      return False
    
computeFor :: Maybe ForInit -> Maybe Exp -> Maybe [Exp] -> Stmt -> FlowOp Bool st 
computeFor begin cond end body = do
  -- Take care of the initialization part
  _ <- case begin of
    Nothing -> return ()
    Just i  -> case i of
      ForLocalVars _ _ vars ->
        mapM_ (computeGraphStmt . localToStmt) vars
      ForInitExps exps ->
        mapM_ (computeGraphStmt . ExpStmt) exps
  -- After initialization we have an usual while loop
  -- The current should be the condition and the scope
  condPc <- getCurrent
  condEId <- incEdCounter
  trueEId <- incEdCounter
  falseEId <- incEdCounter
  let eInfo = EdgeInfo [LoopHead] Skip
      _cond = case cond of
         Nothing -> Lit $ Boolean True 
         Just e  -> e
      eTrue = EdgeInfo [CondTag] (ExpStmt _cond)
      eFalse = EdgeInfo [CondTag] (ExpStmt (PreNot _cond))
  addEdgeInfo condEId eInfo
  addEdgeInfo trueEId eTrue
  addEdgeInfo falseEId eFalse
  truePc <- incCounter
  falsePc <- getNext
  -- Add the edges from the loop head
  addEdge condPc falseEId falsePc
  -- Going to do the loop body now
  replaceCurrent truePc
  endPc <- incCounter
  pushPrev endPc
  computeGraphStmt body
  _ <- case end of
     Nothing -> return ()
     Just e -> mapM_ (computeGraphStmt . ExpStmt) e
  -- New current should be at the end
  curr <- getCurrent
  tranE <- incEdCounter
  endE  <- incEdCounter
  let eTran = EdgeInfo [] Skip
      eEnd = EdgeInfo [LoopHead] Skip 
  addEdgeInfo endE eEnd
  addEdgeInfo tranE eTran
  addEdge curr tranE endPc
  addEdge endPc endE condPc 
  popPrev
  replaceCurrent falsePc 
  return False

computeWhile :: Exp -> Stmt -> Bool -> FlowOp Bool st
computeWhile cond body doWhile = 
  if doWhile
  then error "no support for do while loops"
  else do
    curr <- getCurrent
    pushPrev curr
    trueE <- incEdCounter
    falseE <- incEdCounter
    let eTrue = EdgeInfo [CondTag] (Assume cond)
        eFalse = EdgeInfo [CondTag] (Assume (PreNot cond))
    truePc <- incCounter
    falsePc <- incCounter
    addEdgeInfo trueE eTrue
    addEdgeInfo falseE eFalse
    addEdge curr trueE truePc
    addEdge curr falseE falsePc
    replaceCurrent truePc 
    prev_exit <- getExit
    replaceExit (Just falsePc)
    computeGraphStmt body 
    popPrev
    let eEnd = EdgeInfo [LoopHead] Skip
    endE <- incEdCounter
    addEdgeInfo endE eEnd
    _curr <- getCurrent
    addEdge _curr endE curr
    replaceCurrent falsePc
    replaceExit prev_exit
    return False

-- | Pretty Print the Graphs into Dot
pp_dot_graphs :: Graphs st -> String
pp_dot_graphs graphs =
  let n_e_s = "digraph program {"
      n_x_s = "}"
      tab = "" -- show_symt_dot symt
      prog_s = M.fold pp_dot_graph "" graphs
  in n_e_s ++ "\n" ++ tab ++ prog_s ++ n_x_s
  where
pp_dot_graph gr@Graph{..} rest =
  let g = M.foldWithKey (pp_dot_edges edge_table) "" graph
  in g ++ "\n" ++ rest
pp_dot_edges table pre succs rest =
  let succs' = foldr  (pp_dot_edge table pre) "" succs
  in succs' ++ "\n" ++ rest
pp_dot_edge table pre (eId,succ) rest =
  let e_label = case M.lookup eId table of
        Nothing -> ""
        Just info -> show (edge_tags info) ++ " " ++ prettyPrint (edge_code info)
      e = show pre ++ " -> " ++ show succ ++ " [label=< " ++ e_label ++ " >]"
  in e ++ "\n" ++ rest

