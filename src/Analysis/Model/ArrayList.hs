{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Model.ArrayList
-- Copyright :  (c) 2017 Marcelo Sousa
-- Modelling of ArrayList 
--  Defines two functions:
--   1. arrayListInit: that defines the SSAVarModel
--   2. arrayListModel: for the API 
-------------------------------------------------------------------------------
module Analysis.Model.ArrayList (arrayListInit,arrayListModel,arrayListAxioms,arrayListNew) where

import Analysis.API
import Analysis.Debug
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Language.Java.Syntax
import Z3.Monad
import qualified Data.Map as M

arrayListInit :: VId -> String -> Z3 SSAVarModel
arrayListInit vId name = do 
 intSort <- mkIntSort
 let idx_start = name ++ "_idx_start_" ++  show vId ++ "_0"
 ast_start <- mkFreshConst idx_start intSort 
 let idx_end = name ++ "_idx_end_" ++  show vId ++ "_0"
 ast_end <- mkFreshConst idx_end intSort 
 return $ M.fromList [("idx_start",(ast_start,intSort,0))
                     ,("idx_end"  ,(ast_end  ,intSort,0))]

arrayListModel :: Ident -> SSAVar -> [Ident] -> VId -> [AST] -> EnvOp AST
arrayListModel id@(Ident var) objVar [Ident m] vId args =
 let vmod = _v_mod objVar
 in case m of 
     "get"    -> do 
      let (i,_,_) = safeLookup "arrayList.get" "idx_start" vmod 
      lift $ mkSelect (_v_ast objVar) i 
     "isEmpty" -> do
      let (i,_,_) = safeLookup "arrayList.isEmpty start" "idx_start" vmod  
          (j,_,_) = safeLookup "arrayList.isEmpty end"   "idx_end"   vmod  
      lift $ mkEq i j
     "remove"  -> do
       env@Env{..} <- get
       -- retrieve the head of the list
       res <- arrayListModel id objVar [Ident "get"] vId args
       -- increment the idx_start 
       let (idx,idxSort,idxCnt) = safeLookup "arrayList.remove" "idx_start" vmod  
           nidxCnt = idxCnt + 1
           idx_start = var ++ "_idx_start_" ++ show vId ++ "_" ++ show nidxCnt 
       ast_start <- lift $ mkFreshConst idx_start idxSort 
       one       <- lift $ mkIntNum 1 
       add_one   <- lift $ mkAdd [idx,one]
       pre       <- lift $ mkEq ast_start add_one
       -- add to the pre-condition  
       npre      <- lift $ mkAnd [pre,_e_pre]
       updatePre npre
       -- update the variable
       let nVarMod = M.insert "idx_start" (ast_start,idxSort,nidxCnt) vmod
           nVar    = objVar { _v_mod = nVarMod }
           nssa    = insertSSAVar vId id nVar _e_ssamap 
       updateSSAMap nssa
       return res
     "add" -> do
       wizPrint "ArrayList Add"
       env@Env{..} <- get
       -- increment the idx_end 
       let (idx,idxSort,idxCnt) = safeLookup "arrayList.add" "idx_end" vmod  
           nidxCnt = idxCnt + 1
           idx_end = var ++ "_idx_end_" ++ show vId ++ "_" ++ show nidxCnt 
       ast_end <- lift $ mkFreshConst idx_end idxSort 
       one     <- lift $ mkIntNum 1 
       add_one <- lift $ mkAdd [idx,one]
       pre_end <- lift $ mkEq ast_end add_one
       -- update the variable
       nVar <- lift $ updateVariable vId id objVar 
       let nVarMod = M.insert "idx_end" (ast_end,idxSort,nidxCnt) (_v_mod nVar)
           nObj    = nVar { _v_mod = nVarMod }
           nssa    = insertSSAVar vId id nObj _e_ssamap 
       updateSSAMap nssa
       -- equality of the store
       let arg_ast = if null args then error "ArrayList.add: no args" else head args
       s_arg <- lift $ getSort arg_ast 
       arg_str <- lift $ sortToString s_arg
       s_arr <- lift $ getSort (_v_ast objVar) 
       arr_str <- lift $ sortToString s_arr 
       s_arrt <- lift $ getSort (_v_ast nVar) 
       arrt_str <- lift $ sortToString s_arrt
       wizPrint $ "ArrayList.add " ++ show (arrt_str,arr_str,arg_str) 
       store     <- lift $ mkStore (_v_ast objVar) idx arg_ast 
       pre_var   <- lift $ mkEq (_v_ast nVar) store  
       -- add to the pre-condition  
       npre      <- lift $ mkAnd [pre_end,pre_var,_e_pre]
       updatePre npre
       return store 
     _ -> error $ "arrayListModel: not supported " ++ m 

arrayListAxioms :: SSAVar -> Z3 AST
arrayListAxioms v@SSAVar{..} = do 
 let (i,_,_) = safeLookup "arrayList.isEmpty start" "idx_start" _v_mod  
     (j,_,_) = safeLookup "arrayList.isEmpty end"   "idx_end"   _v_mod  
 mkLe i j

-- Sets the initial values of the auxiliary variables and also makes the 
-- AST of the ArrayList equal to an uninterpreted function 
arrayListNew :: [Argument] -> (SSAVar,VId) -> EnvOp AST 
arrayListNew args (v@SSAVar{..},vId) = case args of 
  -- the default constructor; just set the modelling variables to 0
  [] -> do 
   env@Env{..} <- get
   let (i,s,_) = safeLookup "arrayList.isEmpty start" "idx_start" _v_mod  
       (j,_,_) = safeLookup "arrayList.isEmpty end"   "idx_end"   _v_mod
       ident   = Ident "NewArrayList" 
       arity   = 2 -- [PrimType IntT, PrimType IntT] 
   zero <- lift $ mkIntNum 0
   f1 <- lift $ mkEq i zero
   f2 <- lift $ mkEq j zero
   rhs <- 
    case M.lookup (ident,arity) _e_fnmap of
     Nothing -> do
      fn    <- lift $ mkFreshFuncDecl "NewArrayList" [s,s] _v_typ 
      ast   <- lift $ mkApp fn [i,j] 
      let fnmap = M.insert (ident,arity) (fn,M.empty) _e_fnmap 
      updateFunctMap fnmap 
      return ast 
     Just (ast,dep) -> do
      wizPrint $ "arrayListNew: Found " ++ show ident ++ " in function map" 
      lift $ mkApp ast [i,j] 
   f3 <- lift $ mkEq _v_ast rhs  
   lift $ mkAnd [f1,f2,f3] 
  -- this is not the default constructor; simply use an uninterpreter function
  _ -> error "arrayListNew: not supported" 
 
