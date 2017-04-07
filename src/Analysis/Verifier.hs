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
import Analysis.Java.Liff
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
  putStrLn $ "original wiz_meth:\n" ++ prettyPrint mth 
  putStrLn $ "original edit o:\n" ++ (unlines $ map prettyPrint e_o) 
  putStrLn $ "original edit a:\n" ++ (unlines $ map prettyPrint e_a) 
  putStrLn $ "original edit b:\n" ++ (unlines $ map prettyPrint e_b) 
  putStrLn $ "original edit m:\n" ++ (unlines $ map prettyPrint e_m) 
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
  putStrLn $ "simplified wiz_meth:\n" ++ prettyPrint _mth 
  putStrLn $ "simplified edit o:\n" ++ (unlines $ map prettyPrint _e_o) 
  putStrLn $ "simplified edit a:\n" ++ (unlines $ map prettyPrint _e_a) 
  putStrLn $ "simplified edit b:\n" ++ (unlines $ map prettyPrint _e_b) 
  putStrLn $ "simplified edit m:\n" ++ (unlines $ map prettyPrint _e_m) 
  let o_class = findClass mth_id _o_info 
      a_class = findClass mth_id _a_info 
      b_class = findClass mth_id _b_info 
      m_class = findClass mth_id _m_info 
      classes = [o_class, a_class, b_class, m_class]
  putStrLn $ "wiz_meth: Fields" 
  putStrLn $ show $ concatMap (\l -> map fst $ toMemberSig l) $ M.elems $ M.unions $ map _cl_fields classes 
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
  post <- T.trace ("verify: " ++ preStr) $ postcond out
  iSSAMap <- initial_SSAMap params 
  let iEnv = Env iSSAMap M.empty M.empty pre post post classes edits True 0 [1,2,3,4] 0
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
    k `seq` analyse stmts
  (bstmt:_) -> do 
    let k = T.trace (_triple preStr (show bstmt) postStr ++ "\n" ++ printSSAMap _ssamap ++ 
            "\nKeys in Function Map: " ++ show (M.keys _fnmap)) $ unsafePerformIO $ getChar
    k `seq` analyse stmts

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
  (bstmt:rest) -> 
    if every $ getAnn bstmt 
    then case next_block stmts of
      (Left b, r) -> T.trace ("analyser: block encoding") $ do
        analyser_block $ map fromAnn b 
        analyse r
      (Right bstmt, r) ->
        -- we know that bstmt is a high level hole
        analyser_bstmt bstmt r 
    else analyser_bstmt bstmt rest 

-- optimised a block that is shared by all variants
--  i. generate the CFG for the block b
--  ii. call the dependence analysis that will return
--      for each variable in the WriteSet the list 
--      of dependences (ReadSet)
--  iii. with the result, update the SSAMap and 
--       use uninterpreted functions to model 
--       the changes using assignments 
analyser_block :: [BlockStmt] -> EnvOp ()
analyser_block b = do
  env@Env{..} <- get
  let mid = (Ident "", Ident "", [])
      mth_bdy = MethodBody $ Just $ Block (b ++ [BlockStmt $ Return Nothing])
      mth = MethodDecl [] [] Nothing (Ident "") [] [] mth_bdy 
      cfg = computeGraphMember mth
      -- blockDep returns a list of DepMap [O,A,B,M]
      -- assume for now that they are all the same
      deps = M.toList $ blockDep (head _classes) cfg 
  mapM_ analyser_block_dep deps 

-- | analyser_block_dep: analyses for each dependence graph
--   the assignments:
--    output = _anonymous (dep1, ..., depn)
analyser_block_dep :: (AbsVar, (Tag, [AbsVar])) -> EnvOp ()
analyser_block_dep (out, (_,inp)) = do
  num <- incAnonym 
  let lhs = symLocToLhs out 
      rhs = toMethodInv num inp
      _exp = Assign lhs EqualA rhs
  assign [1,2,3,4] _exp lhs EqualA rhs

toMethodInv :: Int -> [AbsVar] -> Exp
toMethodInv n inp =
  let args = map symLocToExp inp
  in MethodInv $ MethodCall (Name [Ident $ "Anonymous"++ show n]) args 

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
  AnnWhile pid _cond _body      -> analyse_loop pid _cond _body rest
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
analyse_conditional pid cond s1 s2 rest = T.trace ("analyse conditional of pid " ++ show pid) $
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
  resThen <- analyse_branch preThen s1
  -- else branch
  put env
  ncondSmt <- lift $ mapM mkNot condSmt
  preElse <- lift $ mkAnd (_pre:ncondSmt)
  resElse <- analyse_branch preElse s2
  combine resThen resElse
 where
   analyse_branch phi branch = do
    env@Env{..} <- get
    updatePre phi
    let r = (AnnBlockStmt branch):rest
    cPhi <- lift $ checkSAT phi
    if cPhi == Unsat
    then return _default
    else analyser r
   combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> EnvOp (Result, Maybe Model)
   combine (Unsat,_) (Unsat,_) = return _default
   combine (Unsat,_) res = return res
   combine res _ = return res

analyse_loop = undefined
{-
-- Analyse Loops
analyse_loop :: Int -> [BlockStmt] -> [(Int,Block)] -> Exp -> Stmt -> EnvOp (Result,Maybe Model)
analyse_loop pid r1 rest _cond _body = do
 let bstmt = BlockStmt $ While _cond _body
 env@Env{..} <- get
 invs <- guessInvariants (pid+1) _cond _body
 if _fuse
 then if all isLoop rest
      then do 
       (checkFusion,cont) <- applyFusion ((pid,Block (bstmt:r1)):rest)
       if checkFusion
       then analyse cont
       else analyse_loop_w_inv invs       
      else analyse $ rest ++ [(pid,Block (bstmt:r1))] -- apply commutativity
 else analyse_loop_w_inv invs
 where
   isLoop :: (Int, Block) -> Bool
   isLoop (_, Block ((BlockStmt (While _ _)):rest)) = True
   isLoop _ = False
   analyse_loop_w_inv [] = error "none of the invariants was able to prove the property."
   analyse_loop_w_inv (inv:is) = do
    env@Env{..} <- get
    it_res <- _analyse_loop rest pid _cond _body inv
    if it_res
    then do
--     pre <- lift $ mkAnd [inv,_pre]
     put env
     updatePre inv -- pre
     analyser ((pid,Block r1):rest)
    else analyse_loop_w_inv is
   
--
_analyse_loop :: [(Int,Block)] -> Int -> Exp -> Stmt -> AST -> EnvOp Bool
_analyse_loop rest pid _cond _body inv = do
 invStr  <- lift $ astToString inv
 env@Env{..} <- get
 (checkPre,_) <- lift $ local $ helper _axioms _pre inv
 case checkPre of
  Unsat -> do
   condAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
   ncondAst <- lift $ mkNot condAst
   (checkInv,_) <- lift $ mkAnd [inv,ncondAst] >>= \npre -> local $ helper _axioms npre inv
   case checkInv of
    Unsat -> do
     pre <- lift $ mkAnd [inv,condAst]
     let s = [(pid, Block [BlockStmt _body])]
     updatePre pre
     updatePost inv
     (bodyCheck,m) <- analyser s
     case bodyCheck of
      Unsat -> return True
      Sat -> do
       put env
       return  False -- {inv && cond} body {inv} failed
    Sat -> return False -- inv && not_cond =/=> inv
  Sat -> return False -- pre =/=> inv

applyFusion :: [(Int, Block)] -> EnvOp (Bool,[(Int,Block)])
applyFusion list = do
 env@Env{..} <- get
 let (loops,rest) = unzip $ map takeHead list
     (_conds,bodies) = unzip $ map splitLoop loops
     (pids,conds) = unzip _conds
 astApps <- lift $ mapM (makeApp _ssamap) pids
 let (asts,apps) = unzip astApps
 inv' <- lift $ mkExistsConst [] apps _pre
 -- equality constraints between the loop counter iterations: i1 = i2 and i1 = i3 ...
 eqs <- lift $ mapM (\c -> mkEq (head asts) c) (tail asts)
 eqInv <- lift $ mkAnd eqs
 -- the candidate invariant
 inv <- lift $ mkAnd [inv',eqInv]
 (checkInv,_) <- lift $ local $ helper _axioms _pre inv
 --invStr <- astToString inv
 --preStr <- astToString pre
 --let k = T.trace ("\nPrecondition:\n"++ preStr ++ "\nInvariant:\n" ++ invStr) $ unsafePerformIO $ getChar
 case checkInv of
  Unsat -> do
   -- the new precondition inside the loop
   condsAsts <- lift $ mapM (processExp (_objSort,_params,_res,_fields,_ssamap)) conds 
   ncondsAsts <- lift $ mapM mkNot condsAsts
   bodyPre <- lift $ mkAnd $ inv:condsAsts
   updatePre bodyPre
   updatePost inv
   (bodyCheck,_) <- analyser bodies
   case bodyCheck of
    Unsat -> do
     condsNAst <- lift $ mkAnd condsAsts >>= mkNot
     nPre <- lift $ mkAnd [inv,condsNAst]
     ncondAst <- lift $ mkAnd ncondsAsts
     (lastCheck,_) <- lift $ local $ helper _axioms nPre ncondAst
     case lastCheck of
      Unsat -> do
       put env
       updatePre nPre
       return (True,rest)
      Sat -> return (False,[]) -- "lastCheck failed"
    Sat -> return (False,[]) -- "couldnt prove the loop bodies with invariant"
  Sat -> return (False,[]) -- "precondition does not imply the invariant"
 where
   -- Begin Fusion Utility Functions
   takeHead :: (Int, Block) -> ((Int, Stmt), (Int,Block))
   takeHead (pid, Block []) = error "takeHead"
   takeHead (pid, Block ((BlockStmt b):rest)) = ((pid,b), (pid, Block rest))
   splitLoop :: (Int, Stmt) -> ((Int, Exp), (Int, Block))
   splitLoop (pid, While cond body) = 
    case body of
     StmtBlock block -> ((pid, cond), (pid,block))
     _ -> error "splitLoop constructing block out of loop body"
   splitLoop _ = error "splitLoop"
   makeApp :: SSAMap -> Int -> Z3 (AST,App)
   makeApp ssamap pid = do
    let i = Ident $ "i" ++ show (pid+1)
        (iAST,_,_)  = safeLookup "guessInvariant: i" i ssamap
    iApp <- toApp iAST
    return (iAST,iApp)
   -- End Fusion Utility Functions

-}

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
assign pids _exp lhs aOp rhs = T.trace ("assign: " ++ show pids) $ do
  mapM_ (\p -> assign_inner p _exp lhs aOp rhs) pids
 
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
   (plhsAST,sort, i) <- T.trace ("the ident is " ++ show ident) $  
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
   _rhsAst <- T.trace ("sorts = " ++ show (aSortStr,iSortStr,rhsSortStr,astSortStr,sortStr)) $ lift $ mkStore a i rhsAst
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
