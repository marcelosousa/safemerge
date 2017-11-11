{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Verifier
-- Copyright :  (c) 2016/17 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Verifier (wiz) where

import Analysis.API
import Analysis.Dependence
import Analysis.Debug
import Analysis.Engine
import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Analysis.Java.Simplifier
import Analysis.Java.Flow
import Analysis.Java.Liff hiding (trace)
import Analysis.Optimiser
import Analysis.Pretty
import Analysis.Types
import Analysis.Util
import Calculus
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State.Strict hiding (join)
import Data.Map (Map)
import Data.Maybe
import Data.List
import Edit
import Edit.Apply
import Edit.Types
import Language.Java.Pretty
import Language.Java.Syntax
import Util
import Z3.Monad
import qualified Data.Map as M

wiz :: WMode -> DiffInst -> IO () 
wiz mode diff@MInst{..} = mapM_ (wiz_meth mode diff) _merges 

-- Pre-condition: same set of parameters names in all 4 versions of the method 
wiz_meth :: WMode -> DiffInst -> MethInst -> IO ()
wiz_meth mode diff@MInst{..} (mth_id,mth,e_o,e_a,e_b,e_m) = do 
  -- putStrLn $ "wiz_meth: " ++ show mth_id
  let _mth = simplifyMDecl mth
      _es  = map simplifyEdit [e_o,e_a,e_b,e_m]
      m_mth = fst $ apply_edit_member _mth (_es!!3)
      (f_mth,f_edits) = case mode of
        Prod -> wholeProduct _mth _es 
        _ -> let f_mth = toAnn [1,2,3,4] _mth
                 f_es  = M.fromList $ map (\(vId,_e) -> (vId, map (toAnn [vId]) _e)) $ zip [1,2,3,4] _es 
             in (f_mth,f_es)
      classes = map (findClass mth_id) [_o_info,_a_info,_b_info,_m_info]
  res <- evalZ3 $ verify mode m_mth (mth_id,f_mth) classes f_edits 
  putStrLn $ "wiz: |HOLE|   = " ++ show (length e_m)
  putStrLn $ "wiz: LOC HOLE = " ++ show (loc_of e_m)
  case res of
    Nothing  -> putStrLn "No semantic conflict found"
    Just str -> do
      putStrLn "Semantic conflict found:"
      putStrLn str    

-- The main verification function
verify :: WMode -> MemberDecl -> (MIdent,AnnMemberDecl) -> [ClassSum]-> AnnEdits -> Z3 (Maybe String)
verify mode m (mid,mth) classes edits = do 
 -- compute the set of inputs 
 -- i. union the fields of all classes
 let class_fields    = nub $ concatMap M.elems $ map _cl_fields classes 
 -- ii. get the member signatures for the method (parameters) and the fields
     (params,fields) = (toMemberSig mth, concatMap toMemberSig class_fields) 
 -- iii. get the return type
     rty = getReturnType mth
 -- compute the pre and the post condition
 -- the pre-condition states that the parameters for each version are equal
 -- the post-condition states the soundness condition for the return variable
 --  which is a special dummy variable res_version
 (_ssa,pre) <- encodePre $ params ++ fields  
 -- Compute the post-condition 
 liftIO $ putStrLn $ "verify: " ++ show mid ++ "\n" ++ show fields
 (ssa,post) <- case rty of 
                 Nothing -> encodePost _ssa fields  
                 Just ty -> encodePost _ssa $ (Ident "",[ty]):fields 
 postStr    <- astToString post
 iFuncMap   <- initial_FuncMap
 let iEnv = Env ssa iFuncMap pre classes edits True 0 [1..4] 0 mode rty M.empty (class_fields++[m]) 
     body = case ann_mth_body mth of
              AnnMethodBody Nothing -> []
              AnnMethodBody (Just (AnnBlock b)) -> b 
 -- Generate the relational post & check for semantic conflict freedom
 ((),fEnv)   <- runStateT (analyser body) iEnv
 (res,model) <- local $ helper (_e_pre fEnv) post 
 log_pre     <- astToString (_e_pre fEnv)
 let log_header = "verify: relational VC" 
     log_check  = "verify: semantic conflict VC" 
     log_post   = postStr
     log_out    = unlines [log_header,log_pre,log_check,log_post]
 let sFile = case snd3 mid of
               Ident s -> s
 liftIO $ putStrLn $ "verify: result = " ++ show res 
 liftIO $ writeFile (sFile ++ ".java") (unlines $ map prettyPrint (_e_loc fEnv))
 liftIO $ writeFile (sFile ++ ".log") log_out 
 case res of 
  Unsat -> return Nothing
  Sat -> do
   str <- showModel $ fromJust model
   return $ Just str

analyser :: ProdProgram -> EnvOp ()
analyser prog = do
 -- printProg False prog
 -- printEdits 
  analyse prog

-- | Analyser main function
analyse :: ProdProgram -> EnvOp () 
analyse prog = do
 wizPrint "analyzer: press any key to continue..."
 --wizBreak 
 --debugger prog 
 env@Env{..} <- get
 case prog of
  [] -> wizPrint "analyse: end of program" 
  (bstmt:cont) -> do
    if every (getAnn bstmt) && _e_mode == Dep 
    then do
      wizPrint "analyse: dependence analysis"
      case next_block prog of
          (Left [b],cont) -> 
            if isComplex b 
            then analyseBlock prog cont [fromAnn b] 
            else analyseBStmt b cont
          (Left bck,cont) -> analyseBlock prog cont $ map fromAnn bck 
          (Right b ,cont) -> analyseBStmt b cont 
    else analyseBStmt bstmt cont 

-- | Analyse a block statement:
--     Statement or 
--     Initialization of local variables
analyseBStmt :: AnnBlockStmt -> ProdProgram -> EnvOp () 
analyseBStmt bstmt cont = do 
 --printStat bstmt
 case bstmt of
  AnnBlockStmt stmt           -> analyseStmt stmt cont 
  AnnLocalVars vIds _ ty vars -> do
   mapM_ (encodeVarDecl vIds ty) vars 
   analyse cont 

isComplex :: AnnBlockStmt -> Bool
isComplex bstmt = case bstmt of
  AnnBlockStmt stmt           -> case stmt of
    AnnStmtBlock vId (AnnBlock b) -> True 
    AnnIfThen vId cond s1         -> True
    AnnIfThenElse vId cond s1 s2  -> True 
    AnnWhile _cond _body          -> True 
    _                             -> False 
  AnnLocalVars vIds _ ty vars -> False 
  
-- | Analyse a statement
analyseStmt :: AnnStmt -> ProdProgram -> EnvOp () 
analyseStmt stmt cont = 
 case stmt of
  AnnStmtBlock vId (AnnBlock b) -> analyse $ b ++ cont
  AnnReturn vId mexpr           -> analyseRet vId mexpr cont 
  AnnIfThen vId cond s1         -> do
   let s2 = AnnStmtBlock vId $ AnnBlock []
   analyseIf vId cond s1 s2 cont
  AnnIfThenElse vId cond s1 s2  -> analyseIf vId cond s1 s2 cont 
  AnnExpStmt vId expr           -> analyseExp vId expr cont
  AnnWhile _cond _body          -> analyseLoop _cond _body cont
  AnnHole  vId                  -> analyseHole vId cont 
  AnnSkip  vId                  -> analyse cont 
  AnnEmpty vId                  -> analyse cont
  AnnThrow vId expr             -> analyse cont -- ignoring throw statements 
  _                             -> analyse cont -- error $ "analyseStmt: " ++ show stmt

-- | Analyse Expressions
--   This function only takes care of assigments and method invocations
analyseExp :: [VId] -> Exp -> ProdProgram -> EnvOp () 
analyseExp vIds _exp rest =
 case _exp of
  MethodInv minv -> do
   mapM_ (encodeCall minv Nothing) vIds 
   analyse rest
  Assign lhs aOp rhs -> do
   wizPrint $ "analyseExp: " ++ show _exp
   assign vIds _exp lhs aOp rhs
   analyse rest 
  PostIncrement lhs -> do
   post_op vIds _exp lhs Add "PostIncrement"
   analyse rest
  PostDecrement lhs -> do
   post_op vIds _exp lhs Sub "PostDecrement"
   analyse rest
  InstanceOf e _ -> 
   let call = MethodInv $ MethodCall (Name [Ident "instanceOf"]) [e]
       one  = Lit $ Int 1
   in analyseExp vIds (BinOp call Equal one) rest
  _ -> error $ "analyseExp: " ++ show _exp
 

-- | The analysis of a hole
--   Assume that there are no nested holes
analyseHole :: [VId] -> ProdProgram -> EnvOp () 
analyseHole vId rest = do
  -- Get the edit statements for this hole.
  edits <- foldM popEdits [] vId
  let prod_prog = miniproduct edits
  analyse $ prod_prog ++ rest

-- Analyse If Then Else
-- Call the analyse over both branches to obtain the VCs 
analyseIf :: [VId] -> Exp -> AnnStmt -> AnnStmt -> ProdProgram -> EnvOp () 
analyseIf vId cond s1 s2 cont = do
 wizPrint $ "analyseIf: versions " ++ show vId 
 wizPrint $ "analyseIf: condition " ++ prettyPrint cond 
 --wizBreak 
 --debugger cont 
 -- bSort <- lift $ mkBoolSort >>= return . Just
 let bSort = Nothing
 if cond == Nondet
 then do
  i_env    <- get
  _        <- analyse [AnnBlockStmt s1]
  env_then <- get 
  put i_env
  _        <- analyse [AnnBlockStmt s2]
  env_else <- get
  new_env  <- joinEnv i_env env_then env_else
  put new_env
  analyse cont
 else do
  condSmt  <- mapM (\v -> encodeExp bSort v cond) vId 
  env      <- get
  -- check that all variants take the same paths 
  condAst <- lift $ mapM (uncurry mkIff) (pair condSmt) >>= mkAnd
  ncondSmt <- lift $ mapM mkNot condSmt
  ncondAst <- lift $ mkAnd ncondSmt
  (vCond,_)   <- lift $ local $ helper (_e_pre env) condAst 
  if vCond == Unsat 
  then do 
  -- then branch
    preThen  <- lift $ mkAnd ((_e_pre env):condSmt)
    updatePre preThen
    _        <- analyse [AnnBlockStmt s1] 
    env_then <- get
    -- else branch
    put env
    updateEdits (_e_edits env_then)
    updateConsts (_e_consts env_then)
    ncondSmt <- lift $ mapM mkNot condSmt
    preElse  <- lift $ mkAnd ((_e_pre env):ncondSmt)
    updatePre preElse
    _        <- analyse [AnnBlockStmt s2]
    env_else <- get
    new_env  <- joinEnv env env_then env_else
    put new_env
    analyse cont
  else do
    wizPrint "analyseIf: not all variants are taking the same path"
    let ifs = map (\i -> AnnBlockStmt $ AnnIfThenElse [i] cond (toAnn [i] ((fromAnn s1)::Stmt)) (toAnn [i] ((fromAnn s2)::Stmt))) vId
    analyse $ ifs ++ cont  

-- Analyse Loops
--  Houdini style loop invariant generation
analyseLoop :: [(VId,Exp)] -> AnnStmt -> ProdProgram -> EnvOp () 
analyseLoop conds body rest = do
 wizPrint "analyseLoop: press any key to continue..."
 --wizBreak 
 --debugger rest 
 bSort <- lift $ mkBoolSort >>= return . Just
 env@Env{..} <- get
 -- use equality predicates between variables in the assignment map
 all_preds   <- getPredicates _e_ssamap 
 -- only consider filters consistent with the pre-condition 
 init_preds  <- lift $ filterM (\(i,m,n,p) -> _e_pre `implies` p) all_preds
 -- encode the condition of the loop
 cond_ast    <- mapM (uncurry (encodeExp bSort)) conds >>= lift . mkAnd
 -- going to call houdini
 cond_str    <- lift $ astToString cond_ast
 preds_str   <- lift $ mapM (\(i,m,n,e) -> astToString e >>= \estr -> return $ show (i,m,n,estr)) init_preds 
 -- wizPrint $ "analyse_loop: calling houdini with following inputs\npredicate set: " 
 --          ++ show preds_str ++ "\nloop condition:\n" ++ cond_str 
 -- wizBreak
 houdini init_preds cond_ast body 
 analyse rest

-- houdini: fixpoint over set of predicates to compute inductive invariant
houdini :: [(Ident,VId,VId,AST)] -> AST -> AnnStmt -> EnvOp ()
houdini ann_preds cond body = do 
 i_env  <- get
 let pre   = _e_pre i_env 
     preds = map (\(a,b,c,d) -> d) ann_preds 
 -- candidate invariant is simply the conjunction of the current set of predicates
 inv    <- lift $ if null preds then mkTrue else mkAnd preds
 invStr <- lift $ astToString inv
 --wizPrint $ "houdini: candidate invariant:\n" ++ invStr 
 --wizBreak
 npre   <- lift $ mkAnd [inv,cond] 
 updatePre npre 
 -- compute the relational post condition 
 analyseStmt body []
 nenv   <- get
 let post = _e_pre nenv
 post_str <- lift $ astToString post 
 -- get the preds that are implied by the post
 -- 1. gather the updated predicates of the assignments
 new_preds <- mapM updatePredicate ann_preds 
 sat_preds <- lift $ filterM (\(i,m,n,p) -> post `implies` p) new_preds
 -- the "original" predicates not implied by the post
 let old_sat_preds = map (toOldPredicate ann_preds) sat_preds 
     not_preds = ann_preds \\ old_sat_preds
 unsat_str <- lift $ mapM (\(i,m,n,e) -> astToString e >>= \estr -> return $ show (i,m,n,estr)) not_preds 
 --wizPrint $ "houdini: candidate invariant:\n" ++ invStr ++ "\nrelational post:\n" ++ post_str
 --         ++ "\npredicates not satisfied by relational post:\n" ++ show unsat_str 
 --wizBreak
 -- revert to the original environment
 put i_env
 -- fixpoint check 
 if null not_preds
 -- we are done
 then do
  neg_cond <- lift $ mkNot cond
 -- invStr <- lift $ astToString inv
 -- liftIO $ putStrLn "houdini: loop invariant"
 -- liftIO $ putStrLn invStr
  new_pre  <- lift $ mkAnd [inv,neg_cond]
  updatePre new_pre 
  updateEdits (_e_edits nenv)
  updateLoc (_e_loc nenv)
 -- we are not done unless preds == [] where we just default to True
 else if null preds 
      then error $ "houdini: unable to compute inductive fixpoint" 
      else houdini old_sat_preds cond body

toOldPredicate :: [(Ident,VId,VId,AST)] -> (Ident,VId,VId,AST) -> (Ident,VId,VId,AST)
toOldPredicate preds inp@(i,m,n,a) =
 case find (\(i',m',n',a') -> (i,m,n) == (i',m',n')) preds of
   Nothing -> error $ "to_old_predicate: cant find element " ++ show (i,m,n)
   Just (i',m',n',a') -> (i',m',n',a')

updatePredicate :: (Ident,Int,Int,AST) -> EnvOp (Ident,Int,Int,AST)
updatePredicate (ident,m,n,_) = do
 env@Env{..} <- get
 let a_m = getASTSSAMap "update_predicates" m ident _e_ssamap
     a_n = getASTSSAMap "update_predicates" n ident _e_ssamap
 eqs <- lift $ mapM (uncurry mkEq) $ zip a_m a_n 
 eq  <- lift $ mkAnd eqs
 return (ident,m,n,eq)
 
getPredicates :: SSAMap -> EnvOp [(Ident,VId,VId,AST)]
getPredicates m = do
 let pairs = concat $ M.mapWithKey (\k@(Ident name) m' -> 
       if take 3 name == "ret" || name == "null" 
       then []
       else comb $ map (\(n,v) -> (k,n,v)) $ M.toList m') m  
 lift $ mapM (\(i,m,n,a,b) -> equalVariable a b >>= \eq -> return (i,m,n,eq)) pairs

-- Analyse Return
analyseRet :: [VId] -> Maybe Exp -> ProdProgram -> EnvOp () 
analyseRet vIds _exp cont = do 
  mapM_ (\p -> ret_inner p _exp) vIds
  env@Env{..} <- get
  if _e_numret == 4
  then do
    --preast <- lift $ astToString _e_pre
    --wizPrint $ "return:\npre-condition:\n" ++ preast 
    --wizBreak
    --debugger cont 
    return ()
  else analyse cont 
 where
   ret_inner :: VId -> Maybe Exp -> EnvOp ()
   ret_inner vId mexpr = do
    env@Env{..} <- get
    ret <- 
     case mexpr of
       Nothing   -> return [] 
       Just expr -> do
        exp_ast <- getASTExp vId expr
        -- encode the return value
        let res_str = Ident "ret"
            res_ast = getASTSSAMap "ret_inner ret" vId res_str _e_ssamap
        lift $ mapM (uncurry mkEq) $ zip res_ast exp_ast 
    -- encode the fields which are part of the global state
    -- let class_vId@ClassSum{..} = _e_classes !! (vId-1)
    let fls = nub $ concatMap M.keys $ map _cl_fields _e_classes 
        -- get the names of the fields
        -- fls      = M.keys _cl_fields
        -- get the ASTs per field
        fls_last = concatMap (\i -> getASTSSAMap "ret_inner" vId i _e_ssamap) fls
        -- computes the return names of the fields
        ret_fls  = map (\(Ident str) -> Ident $ "ret_"++str) fls 
        -- get those ASTs 
        ret_fls_last = concatMap (\i -> getASTSSAMap "ret_inner" vId i _e_ssamap) ret_fls
    ret_fields <- lift $ mapM (uncurry mkEq) $ zip ret_fls_last fls_last
    pre <- lift $ mkAnd $ _e_pre:(ret ++ ret_fields)
    updatePre pre
    updateNumRet

-- CODE RELATED TO THE DEPENDENCE ANALYSIS
-- optimised a block that is shared by all variants
--  i. generate the CFG for the block b
--  ii. call the dependence analysis that will return
--      for each variable in the WriteSet the list 
--      of dependences (ReadSet)
--  iii. with the result, update the SSAMap and 
--       use uninterpreted functions to model 
--       the changes using assignments 
analyseBlock :: ProdProgram -> ProdProgram -> [BlockStmt] -> EnvOp ()
analyseBlock (bstmt:cont) kont b = do 
  wizPrint $ "analyseBlock:\n" ++ (unlines $ map prettyPrint b)
  env@Env{..} <- get
  let mid = (Ident "", Ident "", [])
      mth_bdy = MethodBody $ Just $ Block (b ++ [BlockStmt $ Return Nothing])
      mth = MethodDecl [] [] Nothing (Ident "") [] [] mth_bdy 
      cfg = computeGraphMember mth
      -- blockDep returns a list of DepMap [O,A,B,M]
      -- assume for now that they are all the same
      deps = blockDep (head _e_classes) cfg 
      listDeps = M.toList deps
      rhs = concat $ snd $ unzip $ snd $ unzip listDeps
      -- need to get the all the inputs first 
  wizPrint $ "analyseBlock: \n" ++ show mth_bdy
  wizPrint $ "analyseBlock: CFG\n" ++ show (cfg :: DepGraph)
  wizPrint $ "analyseBlock:\n" ++ printDepMap deps
  isSound <- checkDep rhs 
  if isSound
  then do
    wizPrint $ "analyseBlock: Sound to use dependence info"
    mapM_ analyse_block_dep listDeps 
    analyse kont
  else do
    wizPrint $ "analyseBlock: Unsound to use dependence info"
    analyseBStmt bstmt cont 
 where
   -- | analyse_block_dep: analyses for each dependence graph
   --   the assignments:
   --    output = _anonymous (dep1, ..., depn)
   --    include the older versions of the variables in the dependencies
   analyse_block_dep :: (AbsVar,(Tag,[AbsVar])) -> EnvOp ()
   analyse_block_dep (out,(_,inp)) = do
     num <- incAnonym 
     let id  = Ident $ "Anonymous"++ show num
     mapM_ (\vId -> assign_special vId id out inp) [1..4]

-- Checks if a set of variables is semantic conflict free
checkDep :: [AbsVar] -> EnvOp Bool
checkDep inputs = do
  asts <- foldM checkDepInp [] inputs 
  env@Env{..} <- get
  if null asts
  then return True
  else do
   tmp_post    <- lift $ mkAnd asts 
   (r,_)       <- lift $ local $ helper _e_pre tmp_post
   return (r == Unsat) 
 where
  -- check if a variable v is semantic conflict free
  checkDepInp :: [AST] -> AbsVar -> EnvOp [AST]
  checkDepInp r v = do
   env@Env{..} <- get
   case v of
    SField i  -> do
     case M.lookup i _e_ssamap of
       Nothing -> error $ "checkDepInp: Field " ++ show i ++ " is not in the SSAMap" 
       Just ve -> do
        -- @ OLD: checks for equality between variables
        --let e  = M.elems ve
        --    vs = lin e 
        --asts <- lift $ mapM (uncurry equalVariable) vs 
        ast <- lift $ conflict_freedom ve
        return (ast:r) 
    _ -> do
     let i = absVarToIdent v 
     case M.lookup i _e_ssamap of
       Nothing -> return r
       Just ve -> do
        ast <- lift $ conflict_freedom ve
        return (ast:r) 

conflict_freedom :: SSAVer -> Z3 AST
conflict_freedom v = do
  let [r_o,r_a,r_b,r_m] = map getASTSSAVar $ M.elems v 
  -- Comparison between original and A
  _oa <- mapM (uncurry mkEq) $ zip r_o r_a
  oa  <- mkAnd _oa
  noa <- mkNot oa
  -- Comparison between original and B
  _ob <- mapM (uncurry mkEq) $ zip r_o r_b
  ob  <- mkAnd _ob
  nob <- mkNot ob
  -- Comparison between original and M
  _om <- mapM (uncurry mkEq) $ zip r_o r_m
  om  <- mkAnd _om
  -- Comparison between M and A
  _ma <- mapM (uncurry mkEq) $ zip r_m r_a
  ma  <- mkAnd _ma
  -- Comparison between M and B
  _mb <- mapM (uncurry mkEq) $ zip r_m r_b
  mb  <- mkAnd _mb
  -- Remains the same
  c1 <- mkImplies noa ma
  c2 <- mkImplies nob mb
  c3 <- mkAnd [ma,mb,om]
  c4 <- mkAnd [c1,c2]
  mkOr [c3,c4]    
  
absVarToIdent :: AbsVar -> Ident
absVarToIdent (SField i) = i
absVarToIdent (SName n) = toIdent n
absVarToIdent (SArray (ArrayIndex e _)) = expToIdent e

-- | This will definitely fail for containers
enc_meth_special :: Int -> Ident -> Sort -> [AST] -> EnvOp AST
enc_meth_special pid id@(Ident ident) sort args = do
  env@Env{..} <- get
  let arity = length args
  case M.lookup (id,arity) _e_fnmap of
    Nothing -> do 
     sorts <- lift $ mapM getSort args 
     fn <- lift $ mkFreshFuncDecl ident sorts sort 
     ast <- lift $ mkApp fn args
     let fnmap = M.insert (id,arity) (fn,M.empty) _e_fnmap 
     updateFunctMap fnmap 
     return ast 
    Just (ast,_) -> lift $ mkApp ast args

-- This is special because the variable might not be defined
assign_special :: VId -> Ident -> AbsVar -> [AbsVar] -> EnvOp ()
assign_special vId fnId out inp = do
-- wizPrint $ "assign_special: " ++ show lhs ++ ", vid = " ++ show vId
 e <- get
 let ident = absVarToIdent out 
 case M.lookup ident (_e_ssamap e) of
   Nothing -> do
     -- new variable definition
     -- adapted from encodeVarDecl @ Engine.hs
     let sig = (ident,[PrimType IntT]) -- Might want to change this!
     var  <- lift $ encodeVariable vId sig
     let ssa = insertSSAVar vId ident var $ _e_ssamap e
         lhs = NameLhs $ Name [ident]
     updateSSAMap ssa 
     assign_special vId fnId out (delete out inp)
   Just ve  -> do 
    case M.lookup vId ve of
     Nothing -> do
      -- new variable definition
      -- adapted from encodeVarDecl @ Engine.hs
      let sig = (ident,[PrimType IntT])
      var  <- lift $ encodeVariable vId sig
      let ssa = insertSSAVar vId ident var $ _e_ssamap e
          lhs = NameLhs $ Name [ident]
      updateSSAMap ssa 
      assign_special vId fnId out (delete out inp)
     Just _  -> do 
      -- variable is already defined
      -- adapted from assign @ Engine.hs
      let args = map symLocToExp inp
      argsAST <- mapM (\e -> encodeExp Nothing vId e) args 
      let lhsVar = getVarSSAMap "assign" vId ident (_e_ssamap e) 
          aOp    = EqualA
          lhs = NameLhs $ Name [ident]
          -- lhs = symLocToLhs out 
      rhsAst  <- enc_meth_special vId fnId (_v_typ lhsVar) argsAST
      nLhsVar <- lift $ updateVariable vId ident lhsVar
      iSort   <- lift $ mkIntSort
      env@Env{..} <- get
      case lhs of
       NameLhs (Name [ident]) -> do
        let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
        ass <- lift $ processAssign lhsVar nLhsVar aOp rhsAst 
        pre <- lift $ mkAnd [_e_pre,ass]
        updatePre pre
        updateSSAMap ssamap
       FieldLhs (PrimaryFieldAccess This ident) -> do
        let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
        ass <- lift $ processAssign lhsVar nLhsVar aOp rhsAst 
        pre <- lift $ mkAnd [_e_pre,ass]
        updatePre pre
        updateSSAMap ssamap
       ArrayLhs (ArrayIndex e args) -> error "not supported because of types"
       _ -> error $ "assign_special not supported"
  
