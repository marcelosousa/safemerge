{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Verifier
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Verifier (wiz) where

import Edit.Types
-- import Analysis.Invariant
import Analysis.Util
import Analysis.Types
import Analysis.Engine
import Calculus
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Map (Map)
import Data.Maybe
import Language.Java.Pretty
import Language.Java.Syntax
import System.IO.Unsafe
import Control.Monad.IO.Class
import Z3.Monad
import qualified Data.Map as M
import qualified Debug.Trace as T

wiz :: Program -> Edit -> Edit -> Edit -> Edit -> IO () 
wiz prog e_o e_a e_b e_m = do
  let m = get_method prog
  res <- evalZ3 $ verify m e_o e_a e_b e_m
  print m
  print res

verify :: Method -> Edit -> Edit -> Edit -> Edit -> Z3 (Result, Maybe String) 
verify (pars, Block body) e_o e_a e_b e_m = do 
 (params, res) <- prelude pars 
 -- compute the pre and the post condition
 -- for now, the pre-condition states that the parameters for each version are equal
 --  and the post-condition states the soundness condition for the return variable
 --  which is a special dummy variable res_version
 pre <- initial_precond params
 post <- postcond res
 iSSAMap <- initial_SSAMap params res
 let iEnv = Env iSSAMap M.empty pre post post e_o e_a e_b e_m True 0 0 
     p = [(0, body)]
 ((res, model),_) <- runStateT (analyser p) iEnv
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
   let k = T.trace (_triple preStr "end" postStr) $ unsafePerformIO $ getChar
   k `seq` analyse stmts
  (bstmt:_) -> do 
   let k = case bstmt of 
        (_,[]) -> '0'
        _  -> T.trace (_triple preStr (prettyPrint (head $ snd bstmt)) postStr) $ unsafePerformIO $ getChar
   k `seq` analyse stmts

analyse :: ProdProgram -> EnvOp (Result,Maybe Model)   
analyse stmts = do
 env@Env{..} <- get
 case stmts of
  [] -> lift $ local $ helper _pre _post
  (bstmt:rest) -> analyser_pid bstmt rest 

analyser_pid :: (Pid,[BlockStmt]) -> ProdProgram -> EnvOp (Result, Maybe Model)
analyser_pid (pid,stmts) rest = case stmts of
  [] -> analyser rest
  (bstmt:stmts_pid) -> case bstmt of
    BlockStmt stmt -> do 
      updatePid pid
      analyser_stmt (pid,stmt) ((pid,stmts_pid):rest)
    LocalVars mods ty vars -> do
      env@Env{..} <- get
      sort <- lift $ processType ty    
      (nssamap, nassmap, npre) <- 
        lift $ foldM (\(ssamap', assmap', pre') v ->
          -- we assume that different versions cannot define the same variable 
          enc_new_var (ssamap', assmap', pre') sort v 0) 
          (_ssamap, _assmap, _pre) vars
      updatePre npre
      updateSSAMap nssamap
      updateAssignMap nassmap
      analyser ((pid,stmts_pid):rest)

analyser_stmt :: (Pid, Stmt) -> ProdProgram -> EnvOp (Result, Maybe Model)
analyser_stmt (pid, stmt) rest =
 case stmt of
  StmtBlock (Block block) -> analyser ((pid,block):rest)
  -- Need to revert this change
  Return mexpr            -> ret pid mexpr rest
  IfThen cond s1          -> do
   let ifthenelse = IfThenElse cond s1 (StmtBlock (Block []))
   analyser_stmt (pid,ifthenelse) rest
  IfThenElse cond s1 s2   -> analyse_conditional pid cond s1 s2 rest
  ExpStmt expr            -> analyse_exp pid expr rest
  While _cond _body       -> analyse_loop _cond _body rest
  Hole                    -> analyse_hole rest 
  _                       -> error $ "analyser_stmt: not supported " ++ show stmt

-- | The analysis of a hole
--   Assume that there are no nested holes
analyse_hole :: ProdProgram -> EnvOp (Result, Maybe Model)
analyse_hole rest = do
   -- Get the edit statements for this hole.
  (s_o, s_a, s_b, s_m) <- popEdits
  let prod_prog = miniproduct [(1,s_o),(2,s_a),(3,s_b),(4,s_m)] 
  analyser $ prod_prog ++ rest

analyse_loop = undefined

-- Analyse Expressions
analyse_exp :: Pid -> Exp -> ProdProgram -> EnvOp (Result, Maybe Model)
analyse_exp pid _exp rest =
 case _exp of
  MethodInv minv -> do
    error "analyse_exp: currently not supported" 
--   method_call minv
--   analyser rest
  Assign lhs aOp rhs -> do
   assign pid _exp lhs aOp rhs
   analyser rest 
  PostIncrement lhs -> do
   post_op pid _exp lhs Add "PostIncrement"
   analyser rest
  PostDecrement lhs -> do
   post_op pid _exp lhs Sub "PostDecrement"
   analyser rest

-- Analyse If Then Else
analyse_conditional :: Pid -> Exp -> Stmt -> Stmt -> ProdProgram -> EnvOp (Result,Maybe Model)
analyse_conditional pid cond s1 s2 rest =
 if cond == Nondet
 then do
  env@Env{..} <- get
  resThen <- analyser ((pid, [BlockStmt s1]):rest)
  put env
  resElse <- analyser ((pid, [BlockStmt s2]):rest)
  combine resThen resElse                
 else do
  env@Env{..} <- get
  condSmt <- lift $ enc_exp pid _ssamap cond
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
    let r = ((pid, [BlockStmt branch]):rest)
    cPhi <- lift $ checkSAT phi
    if cPhi == Unsat
    then return _default
    else analyser r
   combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> EnvOp (Result, Maybe Model)
   combine (Unsat,_) (Unsat,_) = return _default
   combine (Unsat,_) res = return res
   combine res _ = return res

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

-- Analyse Method Call
method_call :: MethodInvocation -> EnvOp ()
method_call minv =
 case minv of 
  MethodCall (Name [Ident "assume"]) [expr] -> do 
   env@Env{..} <- get
   expAST <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) expr
   pre <- lift $ mkAnd [_pre,expAST]
   updatePre pre
  _ -> error $ show minv ++ " not supported"
-}

-- Analyse Return
ret :: Pid -> Maybe Exp -> ProdProgram -> EnvOp (Result, Maybe Model)
ret pid _exp pro = do 
  let l = if pid == 0 then [1..4] else [pid]
  mapM_ (\p -> ret_inner p _exp) l
  env@Env{..} <- get
  if _numret == 4
  then lift $ local $ helper _pre _post
  else analyser pro 

ret_inner :: Pid -> Maybe Exp -> EnvOp ()
ret_inner pid mexpr = do
 env@Env{..} <- get
 exprPsi <- lift $ 
   case mexpr of
     Nothing -> error "ret: return Nothing"
     Just (Lit (Boolean True)) -> mkIntNum 1
     Just (Lit (Boolean False)) -> mkIntNum 0
     Just expr -> enc_exp_inner (pid,_ssamap) expr
 let res_str = Ident "_res_"
     (res_ast,sort, i) = (safeLookup "ret_inner" res_str _ssamap) !! (pid-1)
 r <- lift $ mkEq res_ast exprPsi
 pre <- lift $ mkAnd [_pre,r]
 updatePre pre
 updateNumRet

-- Analyse Assign
assign :: Pid -> Exp -> Lhs -> AssignOp -> Exp -> EnvOp ()
assign pid _exp lhs aOp rhs = do
  let l = if pid == 0 then [1..4] else [pid]
  mapM_ (\p -> assign_inner p _exp lhs aOp rhs) l
 
assign_inner :: Pid -> Exp -> Lhs -> AssignOp -> Exp -> EnvOp ()
assign_inner pid _exp lhs aOp rhs = do
 env@Env{..} <- get
 rhsAst <- lift $ enc_exp_inner (pid, _ssamap) rhs
 case lhs of
  NameLhs (Name [ident@(Ident str)]) -> do
   let (plhsAST,sort, i) = (safeLookup "Assign" ident _ssamap) !! (pid-1)
       cstr = str ++ "_" ++ show pid ++ "_" ++ show i
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
  _ -> error $ "Assign " ++ show _exp ++ " not supported"

-- Analyse Post De/Increment
post_op :: Pid -> Exp -> Exp -> Op -> String -> EnvOp ()
post_op pid _exp lhs op str = do
  let l = if pid == 0 then [1..4] else [pid]
  mapM_ (\p -> post_op_inner p _exp lhs op str) l

post_op_inner :: Pid -> Exp -> Exp -> Op -> String -> EnvOp ()
post_op_inner pid _exp lhs op str = do
 env@Env{..} <- get
 rhsAst <- lift $ enc_exp_inner (pid,_ssamap) (BinOp lhs op (Lit $ Int 1))
 case lhs of
  ExpName (Name [ident@(Ident str)]) -> do
   let (plhsAST,sort, i) = (safeLookup "post_op_inner" ident _ssamap) !! (pid-1)
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
