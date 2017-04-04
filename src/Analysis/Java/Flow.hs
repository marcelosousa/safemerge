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
    graphs       :: Graphs st
  , this         :: Graph  st
  , defs         :: Map MIdent MemberDecl 
  , entries      :: Map MIdent NodeId
  -- previous nodes? are these just for loop heads?
  , prev         :: [NodeId] 
  -- current node
  , current      :: NodeId 
  -- next nodes?
  , next         :: [NodeId] 
  , exit         :: Maybe NodeId 
  , pc_counter   :: NodeId
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

computeGraphMember :: MemberDecl -> Graph st
computeGraphMember mem = 
  let ((),st) = runState (toFlow mem) init_st
  in this st 

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

instance Flow MemberDecl () where
  toFlow mem = do
    let mid = (Ident "", Ident "", [])
    newEntry mid mem 
    computeGraph (mid,mem)

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

-- This is the main function that computes the CFG of a MemberDecl (Method or Constructor) 
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
      if isInfixOf "Return" $ show block
      then mapM_ computeGraphBStmt block
      else mapM_ computeGraphBStmt (block ++ [BlockStmt $ Return Nothing])
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
  -- the next is the node id of the successor
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
    -- Convert a BasicFor into a While statement (disregarding scoping)
    BasicFor init exp exps stmt -> do 
      _ <- case init of
        Nothing -> return ()
        Just fi -> case fi of
          ForLocalVars _ _ vars -> mapM_ (computeGraphStmt . localToStmt) vars
          ForInitExps es  -> mapM_ (computeGraphStmt . ExpStmt) es 
      let cond = case exp of 
            Nothing -> Lit $ Boolean True 
            Just c  -> c 
          body = case exps of
            Nothing -> stmt
            Just [] -> stmt
            Just st -> StmtBlock $ Block $ (BlockStmt stmt):map (BlockStmt . ExpStmt) st
      computeWhile cond body False 
    -- Enhanced For: converts to a while loop *PART OF THE COLLECTIONS*
    EnhancedFor mods ty ident exp body -> do
      -- create an internal counter that will traverse the collection
      --  and the variable ident
      let idI = Ident "wiz_i" 
          varI = VarDecl (VarId idI) $ Just $ InitExp $ Lit $ Int 0 
          nameI = ExpName $ Name [idI]
          varK = VarDecl (VarId ident) $ Just $ InitExp $ ArrayAccess $ ArrayIndex exp [nameI] 
      mapM_ (computeGraphStmt . localToStmt) [varI, varK]
      -- create the condition of the loop 
      let cond = BinOp nameI LThan $ MethodInv $ PrimaryMethodCall exp [] (Ident "length") [] 
      -- create the increment
          inc = ExpStmt $ BinOp nameI Add $ Lit $ Int 1 
          body' = StmtBlock $ Block [BlockStmt body, BlockStmt inc] 
      computeWhile cond body' False
    -- Switch statement
    --  i.   generate all the condition expressions 
    --  ii.  generate for each switch block the condi
    --  iii. generate the exit point 
    Switch cond body -> do
      -- current point in the CFG
      curr <- getCurrent
      -- save exit point 
      prev_exit <- getExit
      -- the next point (exit of the switch)
      next <- getNext
      -- replace the current exit with the next
      replaceExit (Just next)
      -- get all the conditions for the default case
      let conds = foldr (\(SwitchBlock l _) r -> 
           case l of
             SwitchCase e -> e:r
             Default -> r) [] body  
      foldM_ (computeSwitchBody curr cond conds) True body
      -- revert back the original exit
      replaceExit prev_exit
      -- standard
      new <- getCurrent
      eId <- incEdCounter
      let eInfo = EdgeInfo [] Skip 
      addEdgeInfo eId eInfo
      addEdge new eId next
      replaceCurrent next
      popNext
      return False
    _ -> error $ "computeGraphStmt: Not supported " ++ show stmt

computeGraphSimpleStmt :: Stmt -> FlowOp Bool st
computeGraphSimpleStmt stmt = do 
  edgeId <- incEdCounter
  curr <- getCurrent
  next <- getNext
  let eInfo = EdgeInfo [] $ normalizeStmt stmt 
  addEdgeInfo edgeId eInfo
  addEdge curr edgeId next 
  replaceCurrent next
  popNext
  return False 

normalizeStmt :: Stmt -> Stmt 
normalizeStmt stmt = case stmt of
  ExpStmt exp -> ExpStmt $ normalizeExp exp
  _ -> stmt

normalizeExp :: Exp -> Exp
normalizeExp exp = 
  let one = Lit $ Int 1
  in case exp of
    PostIncrement e ->
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Add one
    PreIncrement  e -> 
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Add one
    PostDecrement e ->
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Sub one
    PreDecrement  e -> 
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Sub one
    Assign lhs op rhs -> 
      case op of
        EqualA -> exp
        _ ->
          let e = lhsToExp lhs 
              op' = toOp op
          in Assign lhs EqualA $ BinOp e op' rhs
    _ -> exp

toOp :: AssignOp -> Op
toOp op = case op of
  MultA -> Mult
  DivA  -> Div
  RemA  -> Rem
  AddA  -> Add
  SubA  -> Sub
  LShiftA -> LShift
  RShiftA -> RShift
  AndA -> And
  XorA -> Xor
  OrA  -> Or

lhsToExp :: Lhs -> Exp
lhsToExp lhs = case lhs of
  NameLhs n -> ExpName n
  FieldLhs f -> FieldAccess f
  ArrayLhs ai -> ArrayAccess ai

expToLhs :: Exp -> Lhs
expToLhs e = case e of
  ExpName n -> NameLhs n
  _ -> error $ "expToLhs: Unsupported " ++ show e

-- computeSwitchBody: 
--  Receives the initial node id and the expression to generate the condition
computeSwitchBody :: NodeId -> Exp -> [Exp] -> Bool -> SwitchBlock -> FlowOp Bool st
computeSwitchBody orig cond conds hasBreak (SwitchBlock lab bstmt) = do
  -- there is already an edge from the last node in the previous block to id_bstmt 
  id_bstmt <- getCurrent
  let fcond = case lab of 
        Default -> foldr (\c r -> BinOp (BinOp cond NotEq c) And r) (Lit $ Boolean True) conds 
        SwitchCase expr -> BinOp cond Equal expr 
  new <- if orig == id_bstmt || hasBreak
         then incCounter
         else return id_bstmt
  -- build the condition edge
  eId <- incEdCounter
  let eInfo = EdgeInfo [] (Assume fcond)
  addEdgeInfo eId eInfo
  addEdge orig eId new
  replaceCurrent new
  res <- mapM computeGraphBStmt bstmt
  return $ last res

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

