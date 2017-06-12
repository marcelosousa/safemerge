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
import Edit.Types
import Language.Java.Pretty
import Language.Java.Syntax
import Util
import Z3.Monad
import qualified Data.Map as M

wiz :: DiffInst -> IO () 
wiz diff@MInst{..} = mapM_ (wiz_meth diff) _merges 

-- Assume the parameters names are the same in all 4 versions of the method 
wiz_meth :: DiffInst -> MethInst -> IO ()
wiz_meth diff@MInst{..} (mth_id, mth, e_o, e_a, e_b, e_m) = do 
--  putStrLn $ "wiz_meth: " ++ show mth_id
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
      f_mth = toAnn [1,2,3,4] _mth
      f_e_o = map (toAnn [1]) _e_o
      f_e_a = map (toAnn [2]) _e_a
      f_e_b = map (toAnn [3]) _e_b
      f_e_m = map (toAnn [4]) _e_m
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
  res <- evalZ3 $ verify (mth_id, f_mth) classes [f_e_o,f_e_a,f_e_b,f_e_m] 
  case res of
    Nothing  -> putStrLn "No semantic conflict found"
    Just str -> do
      putStrLn "Semantic conflict found:"
      putStrLn str    

-- The main verification function
verify :: (MIdent,AnnMemberDecl) -> [ClassSum]-> [AnnEdit] -> Z3 (Maybe String)
verify (mid@(_,_,(rty:_)),mth) classes edits = do 
 -- compute the set of inputs 
 -- i. union the fields of all classes
 let class_fields    = nub $ M.elems $ M.unions $ map _cl_fields classes 
 -- ii. get the member signatures for the method (parameters) and the fields
     (params,fields) = (toMemberSig mth,concatMap toMemberSig class_fields) 
 -- compute the pre and the post condition
 -- the pre-condition states that the parameters for each version are equal
 -- the post-condition states the soundness condition for the return variable
 --  which is a special dummy variable res_version
 (_ssa,pre) <- encodePre $ params ++ fields  
 -- Compute the post-condition 
 (ssa,post) <- encodePost _ssa $ (Ident "",[rty]):fields 
 postStr <- astToString post
 iFuncMap   <- initial_FuncMap
 let iEnv = Env ssa iFuncMap pre classes edits True 0 [1..4] 0
     body = case ann_mth_body mth of
              AnnMethodBody Nothing -> []
              AnnMethodBody (Just (AnnBlock b)) -> b 
 -- This should simply produce the relational post
 -- that should be checked against the post-condition
 ((), fEnv)  <- runStateT (analyse body) iEnv
 (res,model) <- local $ helper (_e_pre fEnv) post 
 liftIO $ putStrLn postStr
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
-- wizPrint "analyzer: press any key to continue..."
-- wizBreak 
-- debugger prog 
 env@Env{..} <- get
 case prog of
  [] -> wizPrint "analyse: end of program" 
  (bstmt:cont) -> do
    -- only apply dependence analysis if all variables are equal in all versions
    applyDepCheck <- checkDep 
    if (every $ getAnn bstmt) && applyDepCheck 
    then case next_block prog of
          (Left [b],cont) -> analyseBStmt b cont
          (Left bck,cont) -> do
             wizPrint "analyse: abstracting common block" 
             analyseBlock $ map fromAnn bck 
             analyse cont
          (Right b,cont)  ->
            -- we know that bstmt is a high level hole
            analyseBStmt b cont 
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
  _                             -> error $ "analyseStmt: " ++ show stmt

-- | Analyse Expressions
--   This function only takes care of assigments and method invocations
analyseExp :: [VId] -> Exp -> ProdProgram -> EnvOp () 
analyseExp vIds _exp rest =
 case _exp of
  MethodInv minv -> do
   mapM_ (encodeCall minv) vIds 
   analyse rest
  Assign lhs aOp rhs -> do
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
  condSmt  <- mapM (\v -> encodeExp v cond) vId 
  env      <- get
  -- then branch
  preThen  <- lift $ mkAnd ((_e_pre env):condSmt)
  updatePre preThen
  _        <- analyse [AnnBlockStmt s1] 
  env_then <- get
  -- else branch
  put env
  updateEdits (_e_edits env_then)
  ncondSmt <- lift $ mapM mkNot condSmt
  preElse  <- lift $ mkAnd ((_e_pre env):ncondSmt)
  updatePre preElse
  _        <- analyse [AnnBlockStmt s2]
  env_else <- get
  new_env  <- joinEnv env env_then env_else
  put new_env
  analyse cont

-- Analyse Loops
--  Houdini style loop invariant generation
analyseLoop :: [(VId,Exp)] -> AnnStmt -> ProdProgram -> EnvOp () 
analyseLoop conds body rest = do
 wizPrint "analyseLoop"
 env@Env{..} <- get
 -- use equality predicates between variables in the assignment map
 all_preds   <- getPredicates _e_ssamap 
 -- only consider filters consistent with the pre-condition 
 init_preds  <- lift $ filterM (\(i,m,n,p) -> _e_pre `implies` p) all_preds
 -- encode the condition of the loop
 cond_ast    <- mapM (uncurry encodeExp) conds >>= lift . mkAnd
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
    exp_ast <- 
      case mexpr of
        Nothing   -> error "ret: return Nothing"
        Just expr -> getASTExp vId expr
    env@Env{..} <- get
    -- encode the return value
    let res_str = Ident "ret"
        res_ast = getASTSSAMap "ret_inner ret" vId res_str _e_ssamap
    ret <- lift $ mapM (uncurry mkEq) $ zip res_ast exp_ast 
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
checkDep :: EnvOp Bool
checkDep = return False 
-- do
--  env@Env{..} <- get
--  preds       <- get_predicates _ssamap 
--  tmp_post    <- lift $ if null preds then mkTrue else mkAnd preds 
--  (r,_)       <- lift $ local $ helper _pre tmp_post
--  return (r == Unsat) 

-- optimised a block that is shared by all variants
--  i. generate the CFG for the block b
--  ii. call the dependence analysis that will return
--      for each variable in the WriteSet the list 
--      of dependences (ReadSet)
--  iii. with the result, update the SSAMap and 
--       use uninterpreted functions to model 
--       the changes using assignments 
analyseBlock :: [BlockStmt] -> EnvOp ()
analyseBlock b = do 
  undefined
{-
  env@Env{..} <- get
  let mid = (Ident "", Ident "", [])
      mth_bdy = MethodBody $ Just $ Block (b ++ [BlockStmt $ Return Nothing])
      mth = MethodDecl [] [] Nothing (Ident "") [] [] mth_bdy 
      cfg = computeGraphMember mth
      -- blockDep returns a list of DepMap [O,A,B,M]
      -- assume for now that they are all the same
      deps = M.toList $ blockDep (head _e_classes) cfg 
      -- need to get the all the inputs first 
  list <- mapM get_inputs deps
  mapM_ analyse_block_dep list 
 where
   get_inputs :: (AbsVar, (Tag, [AbsVar])) -> EnvOp (AbsVar, [[AST]])
   get_inputs (out, (_,inp)) = do
     let args = map symLocToExp inp
     argsAST <- mapM (enc_exp [1,2,3,4]) args 
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
     (ident, (_,sort,_)) <- enc_lhs 1 lhs
     rhs <- if null inp 
            then mapM (\vId -> enc_meth_special vId id sort []) [1..4]
            else mapM (\(vId,args) -> enc_meth_special vId id sort args) $ zip [1..4] inp 
     mapM_ (\(vId,ast) -> assign_special vId lhs ast) $ zip [1..4] rhs

enc_meth_special :: Int -> Ident -> Sort -> [AST] -> EnvOp AST
enc_meth_special pid id@(Ident ident) sort args = do
  env@Env{..} <- get
  let arity = length args
  sorts <- lift $ mapM getSort args 
  fn <- lift $ mkFreshFuncDecl ident sorts sort 
  ast <- lift $ mkApp fn args
  let fnmap = M.insert (id,arity) (fn,M.empty) _e_fnmap 
  updateFunctMap fnmap 
  return ast 

enc_lhs :: VId -> Lhs -> EnvOp (Ident,(AST,Sort,Int))
enc_lhs vId lhs = do 
 env@Env{..} <- get
 case lhs of
  NameLhs (Name [ident@(Ident str)]) -> do
   case M.lookup ident _ssamap of
       -- new variable
       Nothing -> enc_lhs_inner vId ident
       Just l -> case M.lookup vId l of
         Nothing -> enc_lhs_inner vId ident
         Just r  -> return (ident,r) 
  FieldLhs (PrimaryFieldAccess This (ident@(Ident str))) -> do
   case M.lookup ident _ssamap of
       -- new variable
       Nothing -> trace ("new field variable?") $ enc_lhs_inner vId ident
       Just l -> case M.lookup vId l of
         Nothing -> enc_lhs_inner vId ident
         Just r  -> return (ident,r) 

enc_lhs_inner vId ident@(Ident str) = do
  env@Env{..} <- get
  iSort <- lift $ mkIntSort
  let i = 0
  idAsts <- lift $ enc_ident [vId] str i iSort
  let idAst = snd $ head idAsts
      res = (idAst,iSort,i)
      nssamap = updateSSAMap vId ident res _ssamap
  updateSSAMap nssamap
  return (ident,res)

enc_ident :: [Int] -> String -> Int -> Sort -> Z3 [(Int, AST)]
enc_ident pids str i sort = 
  mapM (\j -> do
    let nstr = str ++ "_" ++ show j ++ "_" ++ show i
    sym <- mkStringSymbol nstr
    ast <- mkVar sym sort
    return (j,ast)) pids
assign_special :: VId -> Lhs -> AST -> EnvOp ()
assign_special vId lhs rhsAst = trace ("assign_special: " ++ show vId ++ " " ++ show lhs) $ do
 env@Env{..} <- get
 case lhs of
  ArrayLhs (ArrayIndex e args) -> do
   let ident@(Ident str) = expToIdent e
   (plhsAST,sort, i) <- trace ("the ident is " ++ show ident) $  
     case M.lookup ident _ssamap of
       -- new variable
       Nothing -> enc_lhs_inner vId ident >>= return . snd
       Just l -> case M.lookup vId l of
         Nothing -> enc_lhs_inner vId ident >>= return . snd
         Just r  -> return r 
   let cstr = str ++ "_" ++ show vId ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show vId ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   astVar <- lift $ mkVar sym sort
   let ssamap = updateSSAMap vId ident (astVar, sort, ni) _ssamap
   a <- enc_exp_inner vId e
   i <- case args of
          [x] -> enc_exp_inner vId x
          _ -> error $ "assign: ArrayLhs " ++ show lhs 
   _rhsAst <- lift $ mkStore a i rhsAst
   ass <- lift $ mkEq _rhsAst astVar
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
  _ -> do 
   (ident@(Ident str),(plhsAST,sort,i)) <- enc_lhs vId lhs 
   let cstr = str ++ "_" ++ show vId ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show vId ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = updateSSAMap vId ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar EqualA rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
-}
