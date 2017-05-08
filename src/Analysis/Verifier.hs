{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Verifier
-- Copyright :  (c) 2016/17 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Verifier (wiz) where

-- import Analysis.Invariant
import Analysis.Dependence
import Analysis.Engine
import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Analysis.Java.Simplifier
import Analysis.Java.Flow
import Analysis.Java.Liff hiding (trace)
import Analysis.Optimiser
import Analysis.Types
import Analysis.Util
import Calculus
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe
import Data.List
import Edit
import Edit.Types
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe
import Z3.Monad
import qualified Data.Map as M
import qualified Debug.Trace as T

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
  putStrLn $ "wiz_meth:\n" ++ prettyPrint _mth 
  putStrLn $ "edit o:\n" ++ (unlines $ map prettyPrint _e_o) 
  putStrLn $ "edit a:\n" ++ (unlines $ map prettyPrint _e_a) 
  putStrLn $ "edit b:\n" ++ (unlines $ map prettyPrint _e_b) 
  putStrLn $ "edit m:\n" ++ (unlines $ map prettyPrint _e_m) 
  let o_class = findClass mth_id _o_info 
      a_class = findClass mth_id _a_info 
      b_class = findClass mth_id _b_info 
      m_class = findClass mth_id _m_info 
      classes = [o_class, a_class, b_class, m_class]
--  putStrLn $ "wiz_meth: Fields" 
--  putStrLn $ show $ concatMap (\l -> map fst $ toMemberSig l) $ M.elems $ M.unions $ map _cl_fields classes 
  (res,mstr) <- evalZ3 $ verify (mth_id, f_mth) classes [f_e_o,f_e_a,f_e_b,f_e_m] 
  if res == Unsat
  then putStrLn "No semantic conflict found"
  else do
    putStrLn "Semantic conflict found:"
    case mstr of
      Nothing -> error "wiz_meth: counterexample missing"
      Just str -> putStrLn str    

-- The main verification function
verify :: (MIdent, AnnMemberDecl) -> [ClassSum]-> [AnnEdit] -> Z3 (Result, Maybe String) 
verify (mid, mth) classes edits = do 
  -- compute the set of inputs 
  -- i. union the fields of all classes
  let fields = M.elems $ M.unions $ map _cl_fields classes 
  -- ii. get the member signatures for the method (parameters) and the fields
      inputs = (toMemberSig mth, concatMap toMemberSig fields) 
  (params, inp, out) <- z3_gen_inout inputs 
  -- compute the pre and the post condition
  -- for now, the pre-condition states that the parameters for each version are equal
  --  and the post-condition states the soundness condition for the return variable
  --  which is a special dummy variable res_version
  pre  <- initial_precond inp  
  preStr <- astToString pre
  post <- trace ("verify: " ++ preStr) $ postcond out
  iSSAMap <- initial_SSAMap params 
  iFuncMap <- initial_FuncMap
  let iEnv = Env iSSAMap M.empty iFuncMap pre post post classes edits True 0 [1,2,3,4] 0
      body = case ann_mth_body mth of
               AnnMethodBody Nothing -> []
               AnnMethodBody (Just (AnnBlock b)) -> b 
  ((res, model), _) <- runStateT (analyser body) iEnv
  case res of 
   Unsat -> return (Unsat, Nothing)
   Sat -> do
    str <- showModel $ fromJust model
    return (Sat, Just str)

-- strongest post condition
_triple :: String -> String -> String -> String
_triple pre stm post =
 unlines
  ["-----------------"
  ,"Analyser State"
  ,pre
  ,stm
  ,post
  ,"-----------------"]

-- @ Analyser main function
analyser :: ProdProgram -> EnvOp (Result, Maybe Model)
analyser stmts = do
 env@Env{..} <- get
 if _debug
 then analyser_debug stmts
 else analyse stmts

analyser_debug :: ProdProgram -> EnvOp (Result, Maybe Model)
analyser_debug stmts = do
 env@Env{..} <- get
 preStr  <- lift $ astToString _pre
 postStr <- lift $ astToString _post
 case stmts of
  [] -> do 
    let k = T.trace (_triple preStr "end" postStr ++ "\n" ++ printSSAMap _ssamap) $ 
             unsafePerformIO $ getChar
    analyse stmts
    -- k `seq` analyse stmts
  (bstmt:_) -> do 
    --let k = T.trace (_triple preStr (show bstmt ++ "\n" ++ prettyPrint (fromAnn bstmt :: BlockStmt)) postStr ++ "\n" ++ printSSAMap _ssamap ++ 
    --        "\nKeys in Function Map: " ++ show (M.keys _fnmap)) $ unsafePerformIO $ getChar
    let k = T.trace (_triple preStr (show bstmt ++ "\n" ++ prettyPrint (fromAnn bstmt :: BlockStmt)) "") $ unsafePerformIO $ getChar
    -- k `seq` analyse stmts
    analyse stmts

-- main verification heavyweight function 
-- checks if the PID = (ALL) and calls 
-- the optimiser to obtain the block and 
-- call the dependence analysis to deal 
-- with it.
-- otherwise, calls the standard analysis 
analyse :: ProdProgram -> EnvOp (Result,Maybe Model)   
analyse stmts = do
 env@Env{..} <- get
 case stmts of
  [] -> lift $ local $ helper _pre _post
  (bstmt:rest) -> do
    applyDepCheck <- checkDep  
    if (every $ getAnn bstmt) && applyDepCheck 
    then -- only apply dependence analysis if all variables are equal is all versions
      case next_block stmts of
      (Left b, r) -> T.trace ("analyser: block encoding") $ do
        case b of
          [b'] -> 
            analyser_bstmt bstmt r
          _ -> do
            analyser_block $ map fromAnn b 
            analyser r
      (Right bstmt, r) ->
        -- we know that bstmt is a high level hole
        analyser_bstmt bstmt r 
    else analyser_bstmt bstmt rest 

checkDep :: EnvOp Bool
checkDep = do
  env@Env{..} <- get
  preds <- get_predicates _ssamap 
  tmp_post <- lift $ mkAnd preds 
  (r,_) <- lift $ helper _pre tmp_post
  return (r == Unsat) 

-- optimised a block that is shared by all variants
--  i. generate the CFG for the block b
--  ii. call the dependence analysis that will return
--      for each variable in the WriteSet the list 
--      of dependences (ReadSet)
--  iii. with the result, update the SSAMap and 
--       use uninterpreted functions to model 
--       the changes using assignments 
analyser_block :: [BlockStmt] -> EnvOp ()
analyser_block b = T.trace ("analyser_block:\n" ++ (unlines $ map prettyPrint b)) $ do
  env@Env{..} <- get
  let mid = (Ident "", Ident "", [])
      mth_bdy = MethodBody $ Just $ Block (b ++ [BlockStmt $ Return Nothing])
      mth = MethodDecl [] [] Nothing (Ident "") [] [] mth_bdy 
      cfg = computeGraphMember mth
      -- blockDep returns a list of DepMap [O,A,B,M]
      -- assume for now that they are all the same
      deps = M.toList $ blockDep (head _classes) cfg 
      -- need to get the all the inputs first 
  list <- mapM get_inputs deps
  mapM_ analyser_block_dep list 

get_inputs :: (AbsVar, (Tag, [AbsVar])) -> EnvOp (AbsVar, [[AST]])
get_inputs (out, (_,inp)) = do
  let args = map symLocToExp inp
  argsAST <- mapM (enc_exp [1,2,3,4]) args 
  return (out, transpose argsAST)

-- | analyser_block_dep: analyses for each dependence graph
--   the assignments:
--    output = _anonymous (dep1, ..., depn)
--    include the older versions of the variables in the dependencies
analyser_block_dep :: (AbsVar, [[AST]]) -> EnvOp ()
analyser_block_dep (out,inp) = T.trace ("analyser_block_dep: out = " ++ show out ++ "\nlength = " ++ show (length inp)) $  do
  num <- incAnonym 
  let lhs = symLocToLhs out 
      id = Ident $ "Anonymous"++ show num
  (ident, (_,sort,_)) <- enc_lhs 1 lhs
  rhs <- if null inp 
         then mapM (\pid -> enc_meth_special pid id sort []) [1..4]
         else mapM (\(pid,args) -> enc_meth_special pid id sort args) $ zip [1..4] inp 
  mapM_ (\(pid,ast) -> assign_special pid lhs ast) $ zip [1..4] rhs

-- pre-condition: pid == 0 && bstmt has a hole 
--             or pid != 0 
analyser_bstmt :: AnnBlockStmt -> ProdProgram -> EnvOp (Result, Maybe Model)
analyser_bstmt bstmt rest = case bstmt of
  AnnBlockStmt stmt -> analyser_stmt stmt rest
  AnnLocalVars pids mods ty vars -> do
    env@Env{..} <- get
    sort <- lift $ processType ty    
    mapM_ (enc_new_var pids sort 0) vars 
    analyser rest

analyser_stmt :: AnnStmt -> ProdProgram -> EnvOp (Result, Maybe Model)
analyser_stmt stmt rest = 
 case stmt of
  AnnStmtBlock pid (AnnBlock b) -> analyser $ b ++ rest
  AnnReturn pid mexpr           -> analyse_ret pid mexpr rest
  AnnIfThen pid cond s1         -> do
   let ifthenelse = AnnIfThenElse pid cond s1 $ AnnStmtBlock pid $ AnnBlock []
   analyser_stmt ifthenelse rest
  AnnIfThenElse pid cond s1 s2  -> analyse_conditional pid cond s1 s2 rest
  AnnExpStmt pid expr           -> analyse_exp pid expr rest
  AnnWhile _cond _body          -> analyse_loop _cond _body rest
  AnnHole  pid                  -> analyse_hole pid rest
  AnnSkip  pid                  -> analyser rest 
  AnnEmpty pid                  -> analyser rest
  _                             -> error $ "analyser_stmt: not supported " ++ show stmt

-- | The analysis of a hole
--   Assume that there are no nested holes
--   @NOTE: April'17: it should support nested holes
analyse_hole :: [Pid] -> ProdProgram -> EnvOp (Result, Maybe Model)
analyse_hole pid rest =
  if every pid 
  then do
    -- Get the edit statements for this hole.
    edits <- popEdits
    let prod_prog = miniproduct edits
    analyser $ prod_prog ++ rest
  else error $ "analyse_hole: pids = " ++ show pid

-- Analyse Expressions
analyse_exp :: [Pid] -> Exp -> ProdProgram -> EnvOp (Result, Maybe Model)
analyse_exp pids _exp rest =
 case _exp of
  MethodInv minv -> do
   mapM_ (\pid -> enc_meth_inv pid minv) pids 
   analyser rest
  Assign lhs aOp rhs -> do
   assign pids _exp lhs aOp rhs
   analyser rest 
  PostIncrement lhs -> do
   post_op pids _exp lhs Add "PostIncrement"
   analyser rest
  PostDecrement lhs -> do
   post_op pids _exp lhs Sub "PostDecrement"
   analyser rest

-- Analyse If Then Else
analyse_conditional :: [Pid] -> Exp -> AnnStmt -> AnnStmt -> ProdProgram -> EnvOp (Result,Maybe Model)
analyse_conditional pid cond s1 s2 rest = 
 if cond == Nondet
 then do
  env@Env{..} <- get
  resThen <- analyser $ (AnnBlockStmt s1):rest
  put env
  resElse <- analyser $ (AnnBlockStmt s2):rest
  combine resThen resElse                
 else do
  condSmt <- enc_exp pid cond
  env@Env{..} <- get
  -- then branch
  preThen <- lift $ mkAnd (_pre:condSmt)
  resThen <- T.trace ("analyse:" ++ show pid ++ "  " ++ prettyPrint cond) $ analyse_branch preThen s1
  -- else branch
  put env
  ncondSmt <- lift $ mapM mkNot condSmt
  preElse <- lift $ mkAnd (_pre:ncondSmt)
  resElse <- T.trace ("analyse:" ++ show pid ++ " Not " ++ prettyPrint cond) $ analyse_branch preElse s2
  combine resThen resElse
 where
   analyse_branch phi branch = do
    env@Env{..} <- get
    updatePre phi
    let r = (AnnBlockStmt branch):rest
    cPhi <- lift $ checkSAT phi
    cPhiStr <- lift $ astToString phi 
    if cPhi == Unsat
    then T.trace ("analyse: " ++ cPhiStr ++ "\nanalyse: Unreachable") $ return _default
    else T.trace ("analyse: Reachable") $ analyser r
   combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> EnvOp (Result, Maybe Model)
   combine (Unsat,_) (Unsat,_) = return _default
   combine (Unsat,_) res = return res
   combine res _ = return res

-- Analyse Loops
--  Houdini style loop invariant generation
analyse_loop :: [(Pid,Exp)] -> AnnStmt -> ProdProgram -> EnvOp (Result,Maybe Model) 
analyse_loop conds body rest = do
 env@Env{..} <- get
 -- use equality predicates between variables in the assignment map
 all_preds <- get_predicates _ssamap 
 -- filter out predicates not implied by Pre
 not_preds <- lift $ filterM (\p -> _pre `not_implies` p) all_preds
 let init_preds = all_preds \\ not_preds
 cond_ast <- mapM (\(pid,cond) -> enc_exp [pid] cond >>= lift . mkAnd) conds >>= lift . mkAnd
 houdini init_preds cond_ast body 
 analyser rest

-- houdini: 
houdini :: [AST] -> AST -> AnnStmt -> EnvOp ()
houdini preds cond body = do 
  env <- get
  let pre = _pre env 
  inv <- lift $ if null preds then mkTrue else mkAnd preds
  invStr <- lift $ astToString inv
  let k = T.trace ("loop invariant: \n" ++ invStr ++ "\n" ) $ 
             unsafePerformIO $ getChar
  -- npre <- k `seq` lift $ mkAnd [inv, cond] 
  npre <- lift $ mkAnd [inv, cond] 
  updatePre npre 
  analyser_stmt body []
  nenv <- get
  let post = _pre nenv
  -- get the preds that are not implied by the post
  not_preds <- lift $ filterM (\p -> post `not_implies` p) preds
  -- revert to the original environment
  put env
  -- fixpoint check 
  if null not_preds
  -- we are done
  then do
    neg_cond <- lift $ mkNot cond
    new_pre <- lift $ mkAnd [inv, neg_cond]
    updatePre new_pre 
  -- we are not done unless preds == [] where we just default to True
  else if null preds
       then error $ "houdini: unable to compute inductive fixpoint" 
       else do
         put env
         houdini (preds \\ not_preds) cond body

get_predicates :: SSAMap -> EnvOp [AST]
get_predicates m = do
  let pairs = concat $ M.map (\m' -> 
        let k = map (\(a,b,c) -> a) $ M.elems m'
        in lin k) m  
  lift $ mapM (\(a,b) -> mkEq a b) pairs
  
-- Analyse Return
analyse_ret :: [Pid] -> Maybe Exp -> ProdProgram -> EnvOp (Result, Maybe Model)
analyse_ret pids _exp pro = do 
  mapM_ (\p -> ret_inner p _exp) pids
  env@Env{..} <- get
  if _numret == 4
  then lift $ local $ helper _pre _post
  else analyser pro 

ret_inner :: Pid -> Maybe Exp -> EnvOp ()
ret_inner pid mexpr = do
 exprPsi <- 
   case mexpr of
     Nothing -> error "ret: return Nothing"
     Just (Lit (Boolean True)) -> lift $ mkIntNum 1
     Just (Lit (Boolean False)) -> lift $ mkIntNum 0
     Just expr -> enc_exp_inner pid expr
 env@Env{..} <- get
 let class_pid@ClassSum{..} = _classes !! (pid-1)
     res_str = Ident "ret"
     (res_ast,sort, i) = safeLookup "ret_inner" pid $ safeLookup "ret_inner 1" res_str _ssamap
     fls = M.keys _cl_fields
     fls_last = map (\i -> get_ast "ret_inner field" pid i _ssamap) fls
     ret_fls = map (\(Ident str) -> Ident $ "ret_"++str) fls 
     ret_fls_last = map (\i -> get_ast "ret_inner last field" pid i _ssamap) ret_fls
 r' <- lift $ mapM (\(a,b) -> mkEq a b) $ zip ret_fls_last fls_last
 r <- lift $ mkEq res_ast exprPsi
 pre <- lift $ mkAnd $ [_pre,r] ++ r'
 updatePre pre
 updateNumRet

-- Analyse Assign
assign :: [Pid] -> Exp -> Lhs -> AssignOp -> Exp -> EnvOp ()
assign pids _exp lhs aOp rhs = -- trace ("assign: " ++ show pids ++ ", lhs = " ++ prettyPrint lhs ++ ", rhs = " ++ prettyPrint rhs) $ do
  mapM_ (\p -> assign_inner p _exp lhs aOp rhs) pids

enc_lhs :: Pid -> Lhs -> EnvOp (Ident,(AST,Sort,Int))
enc_lhs pid lhs = do 
 env@Env{..} <- get
 case lhs of
  NameLhs (Name [ident@(Ident str)]) -> do
   case M.lookup ident _ssamap of
       -- new variable
       Nothing -> enc_lhs_inner pid ident
       Just l -> case M.lookup pid l of
         Nothing -> enc_lhs_inner pid ident
         Just r  -> return (ident,r) 
  FieldLhs (PrimaryFieldAccess This (ident@(Ident str))) -> do
   case M.lookup ident _ssamap of
       -- new variable
       Nothing -> T.trace ("new field variable?") $ enc_lhs_inner pid ident
       Just l -> case M.lookup pid l of
         Nothing -> enc_lhs_inner pid ident
         Just r  -> return (ident,r) 

enc_lhs_inner pid ident@(Ident str) = do
  env@Env{..} <- get
  iSort <- lift $ mkIntSort
  let i = 0
  idAsts <- lift $ enc_ident [pid] str i iSort
  let idAst = snd $ head idAsts
      res = (idAst,iSort,i)
      nssamap = update_ssamap pid ident res _ssamap
  updateSSAMap nssamap
  return (ident,res)

assign_special :: Pid -> Lhs -> AST -> EnvOp ()
assign_special pid lhs rhsAst = T.trace ("assign_special: " ++ show pid ++ " " ++ show lhs) $ do
 env@Env{..} <- get
 case lhs of
  ArrayLhs (ArrayIndex e args) -> do
   let ident@(Ident str) = expToIdent e
   (plhsAST,sort, i) <- trace ("the ident is " ++ show ident) $  
     case M.lookup ident _ssamap of
       -- new variable
       Nothing -> enc_lhs_inner pid ident >>= return . snd
       Just l -> case M.lookup pid l of
         Nothing -> enc_lhs_inner pid ident >>= return . snd
         Just r  -> return r 
   let cstr = str ++ "_" ++ show pid ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show pid ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   astVar <- lift $ mkVar sym sort
   let ssamap = update_ssamap pid ident (astVar, sort, ni) _ssamap
   a <- enc_exp_inner pid e
   i <- case args of
          [x] -> enc_exp_inner pid x
          _ -> error $ "assign: ArrayLhs " ++ show lhs 
   _rhsAst <- lift $ mkStore a i rhsAst
   ass <- lift $ mkEq _rhsAst astVar
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
  _ -> do 
   (ident@(Ident str),(plhsAST,sort,i)) <- enc_lhs pid lhs 
   let cstr = str ++ "_" ++ show pid ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show pid ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = update_ssamap pid ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar EqualA rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap

assign_inner :: Pid -> Exp -> Lhs -> AssignOp -> Exp -> EnvOp ()
assign_inner pid _exp lhs aOp rhs = do
 rhsAst <- enc_exp_inner pid rhs
 env@Env{..} <- get
 case lhs of
  NameLhs (Name [ident@(Ident str)]) -> do
   (plhsAST,sort, i) <- 
     case M.lookup ident _ssamap of
       -- new variable
       Nothing -> assign_inner' pid ident
       Just l -> case M.lookup pid l of
         Nothing -> assign_inner' pid ident
         Just r  -> return r 
   let cstr = str ++ "_" ++ show pid ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show pid ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = update_ssamap pid ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar aOp rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
   incrementAssignMap ident rhs
  FieldLhs (PrimaryFieldAccess This (ident@(Ident str))) -> do
   (plhsAST,sort, i) <- 
     case M.lookup ident _ssamap of
       -- new variable
       Nothing -> assign_inner' pid ident
       Just l -> case M.lookup pid l of
         Nothing -> assign_inner' pid ident
         Just r  -> return r 
   let cstr = str ++ "_" ++ show pid ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show pid ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = update_ssamap pid ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar aOp rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
   incrementAssignMap ident rhs
  ArrayLhs (ArrayIndex e args) -> do
   let ident@(Ident str) = expToIdent e
   (plhsAST,sort, i) <- trace ("the ident is " ++ show ident) $  
     case M.lookup ident _ssamap of
       -- new variable
       Nothing -> assign_inner' pid ident
       Just l -> case M.lookup pid l of
         Nothing -> assign_inner' pid ident
         Just r  -> return r 
   let cstr = str ++ "_" ++ show pid ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show pid ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   astVar <- lift $ mkVar sym sort
   let ssamap = update_ssamap pid ident (astVar, sort, ni) _ssamap
   a <- enc_exp_inner pid e
   i <- case args of
          [x] -> enc_exp_inner pid x
          _ -> error $ "assign: ArrayLhs " ++ show lhs 
   aSortStr <- lift $ getSort a >>= sortToString
   iSortStr <- lift $ getSort i >>= sortToString
   rhsSortStr <- lift $ getSort rhsAst >>= sortToString
   astSortStr <- lift $ getSort astVar >>= sortToString
   sortStr <- lift $ sortToString sort
   _rhsAst <- trace ("sorts = " ++ show (aSortStr,iSortStr,rhsSortStr,astSortStr,sortStr)) $ lift $ mkStore a i rhsAst
   ass <- lift $ mkEq _rhsAst astVar
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
   incrementAssignMap ident rhs
  _ -> error $ show _exp ++ " not supported"
 where
  assign_inner' pid ident@(Ident str) = do
    env@Env{..} <- get
    iSort <- lift $ mkIntSort
    let i = 0
    idAsts <- lift $ enc_ident [pid] str i iSort
    let idAst = snd $ head idAsts
        res = (idAst,iSort,i)
        nssamap = update_ssamap pid ident res _ssamap
    updateSSAMap nssamap
    return res 

-- 
expToIdent :: Exp -> Ident
expToIdent exp = case exp of
  FieldAccess (PrimaryFieldAccess _ i) -> i
  ArrayAccess (ArrayIndex e _) -> expToIdent e
  ExpName n -> toIdent n

-- Analyse Post De/Increment
-- @NOTE: April'17: These expressions were optimised?
post_op :: [Pid] -> Exp -> Exp -> Op -> String -> EnvOp ()
post_op pids _exp lhs op str = mapM_ (\p -> post_op_inner p _exp lhs op str) pids

post_op_inner :: Pid -> Exp -> Exp -> Op -> String -> EnvOp ()
post_op_inner pid _exp lhs op str = do
 rhsAst <- enc_exp_inner pid (BinOp lhs op (Lit $ Int 1))
 env@Env{..} <- get
 case lhs of
  ExpName (Name [ident@(Ident str)]) -> do
   let (plhsAST,sort, i) = safeLookup "p_inner" pid $ safeLookup "post_op_inner" ident _ssamap
       cstr = str ++ "_" ++ show pid ++ "_" ++ show i
       ni = i+1
       nstr = str ++ "_" ++ show pid ++ "_" ++ show ni
   sym <- lift $ mkStringSymbol nstr
   var <- lift $ mkFreshFuncDecl nstr [] sort
   astVar <- lift $ mkApp var []
   let ssamap = update_ssamap pid ident (astVar, sort, ni) _ssamap
   ass <- lift $ processAssign astVar EqualA rhsAst plhsAST
   pre <- lift $ mkAnd [_pre, ass]
   updatePre pre
   updateSSAMap ssamap
  _ -> error $ str ++ show _exp ++ " not supported"
