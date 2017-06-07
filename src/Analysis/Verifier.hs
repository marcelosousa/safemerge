{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Verifier
-- Copyright :  (c) 2016/17 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Verifier (wiz) where

-- import Analysis.Invariant
import Analysis.Dependence
import Analysis.Debug
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
import Control.Monad.State.Strict hiding (join)
import Data.Map (Map)
import Data.Maybe
import Data.List
import Edit
import Edit.Types
import Language.Java.Pretty
import Language.Java.Syntax
import System.Console.ANSI
import System.IO.Unsafe
import Util
import Z3.Monad
import qualified Data.Map as M

debugger :: ProdProgram -> EnvOp ()
debugger prog = do
  -- liftIO $ clearScreen
  liftIO $ putStrLn menuText
  liftIO $ putStrLn prompt 
  debug
 where
  debug :: EnvOp ()
  debug = do
    env@Env{..} <- get
    c <- liftIO $ getLine
    liftIO $ clearLine
    if null c 
    then debugger prog
    else case head c of
      '1' -> printProg True  prog >> debug 
      '2' -> printEdits           >> debug 
      '3' -> printProg False prog >> debug 
      '4' -> printPre             >> debug
      '5' -> printSSA _ssamap     >> debug
      '6' -> printEnv             >> debug
      'c' -> return ()
      'q' -> error "Exiting..."
      _   -> debugger prog 

printStat :: AnnBlockStmt -> EnvOp ()
printStat bstmt = do
  let str = prettyPrint (fromAnn bstmt :: BlockStmt)
      scope = getAnn bstmt  
  liftIO $ putStrLn $ "Statement of versions " ++ show scope
  liftIO $ putStrLn str

printProg :: Bool -> ProdProgram -> EnvOp ()
printProg b stmts = do
 env@Env{..} <- get
 case stmts of
  [] -> liftIO $ putStrLn "End of Program" 
  (bstmt:_) ->
    if b 
    then printStat bstmt 
    else do 
      let str = unlines $ map (\s -> prettyPrint (fromAnn s :: BlockStmt)) stmts
      liftIO $ putStrLn "Program:"
      liftIO $ putStrLn str

printEdits :: EnvOp ()
printEdits = do
  env@Env{..} <- get
  let holes       = transpose _edits
      h_holes     = zip holes [1..]
      edits     h = zip h     [1..]
      h_edit    1 = "Edit Base:      "
      h_edit    2 = "Edit Variant A: "
      h_edit    3 = "Edit Variant B: "
      h_edit    4 = "Edit Merge:     "
      h_hole    h = "Hole " ++ show h ++ ":"
      printEdit (s,i) = h_edit i ++  prettyPrint (fromAnn s :: BlockStmt)
      printCode h = map printEdit $ edits h 
      printHole (h,i) = unlines ((h_hole i):(printCode h))
      str = unlines $ map printHole h_holes
  liftIO $ putStrLn $ "Edits:"
  liftIO $ putStrLn str 

printPre :: EnvOp ()
printPre = do
  env@Env{..} <- get
  cPhi    <- lift $ checkSAT _pre
  cPhiStr <- lift $ astToString _pre 
  solverStr <- lift $ solverToString
  liftIO $ putStrLn $ "Pre-condition (" ++ show cPhi ++ ")"
  liftIO $ putStrLn cPhiStr
  liftIO $ putStrLn $ show _pre 
  liftIO $ putStrLn solverStr 

printEnv :: EnvOp ()
printEnv = do
  env@Env{..} <- get
  error "printEnv: not implemented" 

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
--  putStrLn $ "wiz_meth: Fields" 
--  putStrLn $ show $ concatMap (\l -> map fst $ toMemberSig l) $ M.elems $ M.unions $ map _cl_fields classes 
  (res,mstr) <- evalZ3 $ verify (mth_id, f_mth) classes [f_e_o,f_e_a,f_e_b,f_e_m] 
  if res == Unsat
  then putStrLn "No semantic conflict found"
  else do
    putStrLn "Semantic conflict found:"
    case mstr of
      Nothing  -> error "wiz_meth: counterexample missing"
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
  -- the pre-condition states that the parameters for each version are equal
  -- the post-condition states the soundness condition for the return variable
  --  which is a special dummy variable res_version
  pre      <- initial_precond inp  
  post     <- postcond out
  iSSAMap  <- initial_SSAMap params 
  iFuncMap <- initial_FuncMap
  let iEnv = Env iSSAMap iFuncMap pre post post classes edits True 0 [1,2,3,4] 0
      body = case ann_mth_body mth of
               AnnMethodBody Nothing -> []
               AnnMethodBody (Just (AnnBlock b)) -> b 
  ((res, model), _) <- runStateT (analyser body) iEnv
  case res of 
   Unsat -> return (Unsat, Nothing)
   Sat -> do
    str <- showModel $ fromJust model
    return (Sat, Just str)

-- @ Analyser main function
-- main verification heavyweight function 
-- checks if the PID = (ALL) and calls 
-- the optimiser to obtain the block and 
-- call the dependence analysis to deal 
-- with it.
-- otherwise, calls the standard analysis 
analyser :: ProdProgram -> EnvOp (Result, Maybe Model)
analyser stmts = do
 --wiz_print "analyzer: press any key to continue..."
 --_ <- liftIO $ getChar
 --debugger stmts
 env@Env{..} <- get
 case stmts of
  [] -> do
    wiz_print "analyser: end of program" 
    lift $ local $ helper _pre _post
  (bstmt:rest) -> do
    applyDepCheck <- checkDep  
    if False -- (every $ getAnn bstmt) && applyDepCheck 
    then -- only apply dependence analysis if all variables are equal is all versions
      case next_block stmts of
      (Left b, r) -> do
        wiz_print "analyser: computing common block" 
        case b of
          [b'] -> do 
            wiz_print "analyser: block is a single statement"
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
  preds       <- get_predicates _ssamap 
  tmp_post    <- lift $ if null preds then mkTrue else mkAnd preds 
  (r,_)       <- lift $ local $ helper _pre tmp_post
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
analyser_block b = trace ("analyser_block:\n" ++ (unlines $ map prettyPrint b)) $ do
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
analyser_block_dep (out,inp) = trace ("analyser_block_dep: out = " ++ show out ++ "\nlength = " ++ show (length inp)) $  do
  num <- incAnonym 
  let lhs = symLocToLhs out 
      id = Ident $ "Anonymous"++ show num
  (ident, (_,sort,_)) <- enc_lhs 1 lhs
  rhs <- if null inp 
         then mapM (\pid -> enc_meth_special pid id sort []) [1..4]
         else mapM (\(pid,args) -> enc_meth_special pid id sort args) $ zip [1..4] inp 
  mapM_ (\(pid,ast) -> assign_special pid lhs ast) $ zip [1..4] rhs

-- | analyse a block statment which can be a statement or initialization of local variables
analyser_bstmt :: AnnBlockStmt -> ProdProgram -> EnvOp (Result, Maybe Model)
analyser_bstmt bstmt rest = do 
 printStat bstmt
 case bstmt of
  AnnBlockStmt stmt              -> analyser_stmt stmt rest
  AnnLocalVars pids mods ty vars -> do
    env@Env{..} <- get
    sort <- lift $ processType ty    
    mapM_ (enc_new_var pids sort 0) vars 
    analyser rest

-- | analyse a statement
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
-- Call the analyser over both branches to obtain the VCs 
-- Need to create additional assignments to uniformize the SSA construction
-- Test cases
--  1. Conditional with multiple assignments to make sure the SSA Map is correct
--  2. Conditional with return statements in one of the branches
--  3. Conditional within a loop
--  4. Conditional within a loop where one of the branches breaks
--  5. Conditional within a loop where one of the branches returns 
analyse_conditional :: [Pid] -> Exp -> AnnStmt -> AnnStmt -> ProdProgram -> EnvOp (Result,Maybe Model)
analyse_conditional pid cond s1 s2 rest = do
 wiz_print $ "analyse_conditional: versions " ++ show pid 
 wiz_print $ "analyse_conditional: conditional " ++ prettyPrint cond 
 if cond == Nondet
 then do
  i_env    <- get
  _        <- analyser [AnnBlockStmt s1]
  env_then <- get 
  put i_env
  _         <- analyser [AnnBlockStmt s2]
  env_else <- get
  new_env  <- join_env i_env env_then env_else
  put new_env
  analyser rest 
 else do
  condSmt  <- enc_exp pid cond
  env      <- get
  -- then branch
  preThen  <- lift $ mkAnd ((_pre env):condSmt)
  updatePre preThen
  _        <- analyser [AnnBlockStmt s1] 
  env_then <- get
  -- else branch
  put env
  updateEdits (_edits env_then)
  ncondSmt <- lift $ mapM mkNot condSmt
  preElse  <- lift $ mkAnd ((_pre env):ncondSmt)
  updatePre preElse
  _        <- analyser [AnnBlockStmt s2]
  env_else <- get
  new_env  <- join_env env env_then env_else
  put new_env
  analyser rest 

-- Analyse Loops
--  Houdini style loop invariant generation
analyse_loop :: [(Pid,Exp)] -> AnnStmt -> ProdProgram -> EnvOp (Result,Maybe Model) 
analyse_loop conds body rest = do
 env@Env{..} <- get
 -- use equality predicates between variables in the assignment map
 all_preds   <- get_predicates _ssamap 
 -- only consider filters consistent with the pre-condition 
 init_preds  <- lift $ filterM (\p -> _pre `implies` p) all_preds
 -- encode the condition of the loop
 cond_ast    <- mapM (\(pid,cond) -> enc_exp_inner pid cond) conds >>= lift . mkAnd
 -- going to call houdini
 cond_str    <- lift $ astToString cond_ast
 preds_str   <- lift $ mapM astToString init_preds 
 wiz_print $ "analyse_loop: calling houdini with following inputs\npredicate set: " 
          ++ show preds_str ++ "\nloop condition:\n" ++ cond_str 
 wiz_break
 houdini init_preds cond_ast body 
 analyser rest

-- houdini: fixpoint over set of predicates to compute inductive invariant
houdini :: [AST] -> AST -> AnnStmt -> EnvOp ()
houdini preds cond body = do 
  i_env  <- get
  let pre = _pre i_env 
  -- candidate invariant is simply the conjunction of the current set of predicates
  inv    <- lift $ if null preds then mkTrue else mkAnd preds
  invStr <- lift $ astToString inv
  wiz_print $ "houdini: candidate invariant:\n" ++ invStr 
  wiz_break
  npre   <- lift $ mkAnd [inv, cond] 
  updatePre npre 
  -- compute the relational post condition 
  analyser_stmt body []
  nenv   <- get
  let post = _pre nenv
  post_str <- lift $ astToString post 
  -- get the preds that are implied by the post
  sat_preds <- lift $ filterM (\p -> post `implies` p) preds
  -- the predicates not implied by the post is computed with set difference
  let not_preds = preds \\ sat_preds
  unsat_str <- lift $ mapM astToString not_preds
  wiz_print $ "houdini: candidate invariant:\n" ++ invStr ++ "\nrelational post:\n" ++ post_str
           ++ "\npredicates not satisfied by relational post:\n" ++ show unsat_str 
  wiz_break
  -- revert to the original environment
  put i_env
  -- fixpoint check 
  if null not_preds
  -- we are done
  then do
    neg_cond <- lift $ mkNot cond
    new_pre  <- lift $ mkAnd [inv, neg_cond]
    updatePre new_pre 
  -- we are not done unless preds == [] where we just default to True
  else if null preds 
       then error $ "houdini: unable to compute inductive fixpoint" 
       else houdini sat_preds cond body

get_predicates :: SSAMap -> EnvOp [AST]
get_predicates m = do
  let pairs = 
        concat $ M.mapWithKey (\k m' -> 
          if k == Ident "ret" || k == Ident "null" 
          then []
          else comb $ map (\(a,b,c) -> a) $ M.elems m') m  
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
       Nothing -> trace ("new field variable?") $ enc_lhs_inner pid ident
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
assign_special pid lhs rhsAst = trace ("assign_special: " ++ show pid ++ " " ++ show lhs) $ do
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
