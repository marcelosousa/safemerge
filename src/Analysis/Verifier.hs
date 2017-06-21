{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Verifier
-- Copyright :  (c) 2016/17 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Verifier (wiz) where

-- import Analysis.Invariant
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

-- Assume the parameters names are the same in all 4 versions of the method 
wiz_meth :: WMode -> DiffInst -> MethInst -> IO ()
wiz_meth mode diff@MInst{..} (mth_id,mth,e_o,e_a,e_b,e_m) = do 
  putStrLn $ "wiz_meth: " ++ show mth_id
--  putStrLn $ "original wiz_meth:\n" ++ prettyPrint mth 
--  putStrLn $ "original edit o:\n" ++ (unlines $ map prettyPrint e_o) 
--  putStrLn $ "original edit a:\n" ++ (unlines $ map prettyPrint e_a) 
--  putStrLn $ "original edit b:\n" ++ (unlines $ map prettyPrint e_b) 
--  putStrLn $ "original edit m:\n" ++ (unlines $ map prettyPrint e_m) 
  let _mth = simplifyMDecl mth
      _e_o = simplifyEdit e_o
      _e_a = simplifyEdit e_a
      _e_b = simplifyEdit e_b
      _e_m = simplifyEdit e_m
      (f_mth,f_edits) = case mode of
        Prod -> 
         let _o = toAnn [1] $ fst $ apply_edit_member _mth _e_o
             _a = toAnn [2] $ fst $ apply_edit_member _mth _e_a
             _b = toAnn [3] $ fst $ apply_edit_member _mth _e_b
             _m = toAnn [4] $ fst $ apply_edit_member _mth _e_m
             AnnMethodDecl _1 _2 _3 _4 _5 _6 b = _o
             body = case b of
              AnnMethodBody Nothing -> error "" 
              AnnMethodBody (Just (AnnBlock o)) -> 
                case ann_mth_body _a of
                  AnnMethodBody Nothing -> error "" 
                  AnnMethodBody (Just (AnnBlock a)) -> 
                    case ann_mth_body _b of
                      AnnMethodBody Nothing -> error "" 
                      AnnMethodBody (Just (AnnBlock b)) -> 
                       case ann_mth_body _m of
                         AnnMethodBody Nothing -> error "" 
                         -- This is LIKELY TO BE WRONG!! TEST 
                         AnnMethodBody (Just (AnnBlock m)) -> miniproduct (o++a++b++m) 
             n_o = AnnMethodDecl _1 _2 _3 _4 _5 _6 (AnnMethodBody (Just (AnnBlock body)))
         in (n_o,[])
        _ -> 
         let f_mth = toAnn [1,2,3,4] _mth
             f_e_o = map (toAnn [1]) _e_o
             f_e_a = map (toAnn [2]) _e_a
             f_e_b = map (toAnn [3]) _e_b
             f_e_m = map (toAnn [4]) _e_m
         in (f_mth,[f_e_o,f_e_a,f_e_b,f_e_m])
  -- putStrLn $ "wiz_meth:\n" ++ prettyPrint _mth 
  -- putStrLn $ "edit o:\n" ++ (unlines $ map (prettyPrint . fst) _e_o) 
  -- putStrLn $ "edit a:\n" ++ (unlines $ map (prettyPrint . fst) _e_a) 
  -- putStrLn $ "edit b:\n" ++ (unlines $ map (prettyPrint . fst) _e_b) 
  -- putStrLn $ "edit m:\n" ++ (unlines $ map (prettyPrint . fst) _e_m) 
  let o_class = findClass mth_id _o_info 
      a_class = findClass mth_id _a_info 
      b_class = findClass mth_id _b_info 
      m_class = findClass mth_id _m_info 
      classes = [o_class, a_class, b_class, m_class]
  putStrLn $ "wiz_meth: Fields" 
  putStrLn $ show $ concatMap (\l -> map fst $ toMemberSig l) $ M.elems $ M.unions $ map _cl_fields classes 
  res <- evalZ3 $ verify mode (mth_id,f_mth) classes f_edits 
  case res of
    Nothing  -> putStrLn "No semantic conflict found"
    Just str -> do
      putStrLn "Semantic conflict found:"
      putStrLn str    

-- The main verification function
verify :: WMode -> (MIdent,AnnMemberDecl) -> [ClassSum]-> [AnnEdit] -> Z3 (Maybe String)
verify mode (mid,mth) classes edits = do 
 -- compute the set of inputs 
 -- i. union the fields of all classes
 let class_fields    = nub $ M.elems $ M.unions $ map _cl_fields classes 
 -- ii. get the member signatures for the method (parameters) and the fields
     (params,fields) = (toMemberSig mth,concatMap toMemberSig class_fields) 
 -- iii. get the return type
     rty = getReturnType mth
 -- compute the pre and the post condition
 -- the pre-condition states that the parameters for each version are equal
 -- the post-condition states the soundness condition for the return variable
 --  which is a special dummy variable res_version
 (_ssa,pre) <- encodePre $ params ++ fields  
 -- Compute the post-condition 
 liftIO $ print $ "verify: " ++ show mid 
 (ssa,post) <- case rty of 
                 Nothing -> encodePost _ssa fields  
                 Just ty -> encodePost _ssa $ (Ident "",[ty]):fields 
 postStr    <- astToString post
 iFuncMap   <- initial_FuncMap
 let iEnv = Env ssa iFuncMap pre classes edits True 0 [1..4] 0 mode rty M.empty
     body = case ann_mth_body mth of
              AnnMethodBody Nothing -> []
              AnnMethodBody (Just (AnnBlock b)) -> b 
 -- This should simply produce the relational post
 -- that should be checked against the post-condition
 ((),fEnv)   <- runStateT (analyse body) iEnv
 (res,model) <- local $ helper (_e_pre fEnv) post 
 liftIO $ putStrLn postStr
 liftIO $ putStrLn $ "result: " ++ show res 
 case res of 
  Unsat -> return Nothing
  Sat -> do
   str <- showModel $ fromJust model
   return $ Just str

-- @ Analyser main function
-- main verification heavyweight function 
-- checks if the vID = (ALL) and calls 
-- the optimiser to obtain the block and 
-- call the dependence analysis to deal 
-- with it.
-- otherwise, calls the standard analysis 
analyse :: ProdProgram -> EnvOp () 
analyse prog = do
 --wizPrint "analyzer: press any key to continue..."
 --wizBreak 
 --debugger prog 
 env@Env{..} <- get
 case prog of
  [] -> wizPrint "analyse: end of program" 
  (bstmt:cont) -> do
    if (every $ getAnn bstmt) && _e_mode == Dep 
    then case next_block prog of
          (Left [b],cont) -> analyseBStmt b cont
          (Left bck,cont) -> analyseBlock prog cont $ map fromAnn bck 
          (Right b ,cont) -> analyseBStmt b cont 
    else analyseBStmt bstmt cont 

-- | Analyse a block statement:
--     Statement or 
--     Initialization of local variables
analyseBStmt :: AnnBlockStmt -> ProdProgram -> EnvOp () 
analyseBStmt bstmt cont = do 
 printStat bstmt
 case bstmt of
  AnnBlockStmt stmt           -> analyseStmt stmt cont 
  AnnLocalVars vIds _ ty vars -> do
   mapM_ (encodeVarDecl vIds ty) vars 
   analyse cont 

-- | analyse a statement
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
  _                             -> error $ "analyseStmt: " ++ show stmt

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

-- | The analysis of a hole
--   Assume that there are no nested holes
--   @NOTE: April'17: it should support nested holes
analyseHole :: [VId] -> ProdProgram -> EnvOp () 
analyseHole vId rest =
 if every vId 
 then do
  -- Get the edit statements for this hole.
  edits <- popEdits
  let prod_prog = miniproduct edits
  analyse $ prod_prog ++ rest
 else error $ "analyse_hole: vIds = " ++ show vId

-- Analyse If Then Else
-- Call the analyse over both branches to obtain the VCs 
-- Need to create additional assignments to uniformize the SSA construction
-- Test cases
--  1. Conditional with multiple assignments to make sure the SSA Map is correct
--  2. Conditional with return statements in one of the branches
--  3. Conditional within a loop
--  4. Conditional within a loop where one of the branches breaks
--  5. Conditional within a loop where one of the branches returns 
analyseIf :: [VId] -> Exp -> AnnStmt -> AnnStmt -> ProdProgram -> EnvOp () 
analyseIf vId cond s1 s2 cont = do
 wizPrint $ "analyseIf: versions " ++ show vId 
 wizPrint $ "analyseIf: condition " ++ prettyPrint cond 
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
  -- then branch
  preThen  <- lift $ mkAnd ((_e_pre env):condSmt)
  updatePre preThen
  _        <- analyse [AnnBlockStmt s1] 
  env_then <- get
  --env_then <- do
  --  rThen <- lift $ checkSAT preThen
  --  case rThen of 
  --    Unsat -> return env
  --    Sat   -> do
  --     updatePre preThen
  --     _        <- analyse [AnnBlockStmt s1] 
  --     get
  -- else branch
  put env
  updateEdits (_e_edits env_then)
  updateConsts (_e_consts env_then)
  ncondSmt <- lift $ mapM mkNot condSmt
  preElse  <- lift $ mkAnd ((_e_pre env):ncondSmt)
  updatePre preElse
  _        <- analyse [AnnBlockStmt s2]
  env_else <- get
  --env_else <- do
  --  rElse <- lift $ checkSAT preElse 
  --  case rElse of 
  --    Unsat -> get 
  --    Sat   -> do
  --     updatePre preElse
  --     _        <- analyse [AnnBlockStmt s2]
  --     get
  new_env  <- joinEnv env env_then env_else
  put new_env
  analyse cont

-- Analyse Loops
--  Houdini style loop invariant generation
analyseLoop :: [(VId,Exp)] -> AnnStmt -> ProdProgram -> EnvOp () 
analyseLoop conds body rest = do
 wizPrint "analyseLoop: press any key to continue..."
 wizBreak 
 debugger rest 
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
 wizPrint $ "analyse_loop: calling houdini with following inputs\npredicate set: " 
          ++ show preds_str ++ "\nloop condition:\n" ++ cond_str 
 wizBreak
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
 wizPrint $ "houdini: candidate invariant:\n" ++ invStr 
 wizBreak
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
 wizPrint $ "houdini: candidate invariant:\n" ++ invStr ++ "\nrelational post:\n" ++ post_str
          ++ "\npredicates not satisfied by relational post:\n" ++ show unsat_str 
 wizBreak
 -- revert to the original environment
 put i_env
 -- fixpoint check 
 if null not_preds
 -- we are done
 then do
  neg_cond <- lift $ mkNot cond
  new_pre  <- lift $ mkAnd [inv,neg_cond]
  updatePre new_pre 
  updateEdits (_e_edits nenv)
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
-- @TODO: Review what happens when there is more than one return statement
analyseRet :: [VId] -> Maybe Exp -> ProdProgram -> EnvOp () 
analyseRet vIds _exp cont = do 
  mapM_ (\p -> ret_inner p _exp) vIds
  env@Env{..} <- get
  if _e_numret == 4
  then do
    preast <- lift $ astToString _e_pre
    wizPrint $ "return:\npre-condition:\n" ++ preast 
    wizBreak
    debugger cont 
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
    let class_vId@ClassSum{..} = _e_classes !! (vId-1)
        -- get the names of the fields
        fls      = M.keys _cl_fields
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
  wizPrint $ "analyseBlock:\n" ++ show b
  env@Env{..} <- get
  let mid = (Ident "", Ident "", [])
      mth_bdy = MethodBody $ Just $ Block (b ++ [BlockStmt $ Return Nothing])
      mth = MethodDecl [] [] Nothing (Ident "") [] [] mth_bdy 
      cfg = computeGraphMember mth
      -- blockDep returns a list of DepMap [O,A,B,M]
      -- assume for now that they are all the same
      deps = M.toList $ blockDep (head _e_classes) cfg 
      -- need to get the all the inputs first 
  wizPrint $ "analyseBlock: " ++ show deps
  isSound <- checkDep $ concat $ snd $ unzip $ snd $ unzip deps
  if isSound
  then do
   list <- mapM get_inputs deps
   mapM_ analyse_block_dep list 
   analyse kont
  else analyseBStmt bstmt cont 
 where
   -- | For each out in the dependence analysis, get the input ASTs
   get_inputs :: (AbsVar, (Tag, [AbsVar])) -> EnvOp (AbsVar, [[AST]])
   get_inputs (out, (_,inp)) = do
     let args = map symLocToExp inp
     argsAST <- mapM (\e -> mapM (\vId -> encodeExp Nothing vId e) [1,2,3,4]) args 
     return (out, transpose argsAST)

   -- | analyse_block_dep: analyses for each dependence graph
   --   the assignments:
   --    output = _anonymous (dep1, ..., depn)
   --    include the older versions of the variables in the dependencies
   analyse_block_dep :: (AbsVar, [[AST]]) -> EnvOp ()
   analyse_block_dep (out,inp) = do
     num <- incAnonym 
     let lhs = symLocToLhs out 
         id = Ident $ "Anonymous"++ show num
     sortLhs <- getSortAbsVar out 
     rhs <- if null inp 
            then mapM (\vId -> enc_meth_special vId id sortLhs []) [1..4]
            else mapM (\(vId,args) -> enc_meth_special vId id sortLhs args) $ zip [1..4] inp 
     mapM_ (\(vId,ast) -> assign_special vId lhs ast) $ zip [1..4] rhs

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
  checkDepInp :: [AST] -> AbsVar -> EnvOp [AST]
  checkDepInp r v = do
   env@Env{..} <- get
   case v of
    SField i  -> do
     case M.lookup i _e_ssamap of
       Nothing -> error $ "checkDepInp: Field " ++ show i ++ " is not in the SSAMap" 
       Just ve -> do
        let e  = M.elems ve
            vs = lin e 
        asts <- lift $ mapM (uncurry equalVariable) vs 
        return $ asts ++ r 
    SName  n  -> do
     let i = toIdent n
     case M.lookup i _e_ssamap of
       Nothing -> return r
       Just ve -> do
        let e  = M.elems ve
            vs = lin e 
        asts <- lift $ mapM (uncurry equalVariable) vs 
        return $ asts ++ r 
    SArray ai -> error "checkDepInp: SArray" 

absVarToIdent :: AbsVar -> Ident
absVarToIdent (SField i) = i
absVarToIdent (SName n) = toIdent n
absVarToIdent (SArray (ArrayIndex e _)) = expToIdent e

getSortAbsVar :: AbsVar -> EnvOp Sort
getSortAbsVar v = do
 env@Env{..} <- get
 let i = absVarToIdent v 
 case M.lookup i _e_ssamap of
   Nothing -> lift $ mkIntSort
   Just ve -> do 
    let var = head $ M.elems ve
    return $ _v_typ var

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
assign_special :: VId -> Lhs -> AST -> EnvOp ()
assign_special vId lhs rhsAst = do
 wizPrint $ "assign_special: " ++ show lhs ++ ", vid = " ++ show vId
 e <- get
 let ident = case lhs of
      NameLhs  name                            -> toIdent name 
      FieldLhs (PrimaryFieldAccess This ident) -> ident 
      ArrayLhs (ArrayIndex exp args)           -> expToIdent exp
 case M.lookup ident (_e_ssamap e) of
   Nothing -> do
     -- new variable definition
     -- adapted from encodeVarDecl @ Engine.hs
     let sig = (ident,[PrimType IntT])
     var  <- lift $ encodeVariable vId sig
     let ssa = insertSSAVar vId ident var $ _e_ssamap e
         lhs = NameLhs $ Name [ident]
     updateSSAMap ssa 
     assign_special vId lhs rhsAst 
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
      assign_special vId lhs rhsAst 
     Just _  -> do 
      -- variable is already defined
      -- adapted from assign @ Engine.hs
      let lhsVar = getVarSSAMap "assign" vId ident (_e_ssamap e) 
          aOp    = EqualA
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
       ArrayLhs (ArrayIndex e args) -> do
        nLhsVar <- lift $ updateVariable vId ident lhsVar
        let ssamap = insertSSAVar vId ident nLhsVar _e_ssamap
        a <- encodeExp (Just $ _v_typ lhsVar) vId e
        i <- case args of
               [x] -> encodeExp (Just iSort) vId x
               _   -> error $ "assign: ArrayLhs " ++ show lhs 
        _rhsAst <- lift $ mkStore a i rhsAst
        ass <- lift $ mkEq (_v_ast nLhsVar) rhsAst 
        pre <- lift $ mkAnd [_e_pre,ass]
        updatePre pre
        updateSSAMap ssamap
       _ -> error $ "assign_special not supported"
  
