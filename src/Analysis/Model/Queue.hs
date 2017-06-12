{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Model.Queue
-- Copyright :  (c) 2017 Marcelo Sousa
-- Modelling of Queues
--  Defines two functions:
--   1. queueInit: that defines the SSAVarModel
--   2. queueModel: for the API 
-------------------------------------------------------------------------------
module Analysis.Model.Queue (queueInit,queueModel) where

import Analysis.API
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Language.Java.Syntax
import Z3.Monad
import qualified Data.Map as M

queueInit :: VId -> String -> Z3 SSAVarModel
queueInit vId name = do 
 intSort <- mkIntSort
 let idx_start = name ++ "_idx_start_" ++  show vId ++ "_0"
 ast_start <- mkFreshConst idx_start intSort 
 let idx_end = name ++ "_idx_end_" ++  show vId ++ "_0"
 ast_end <- mkFreshConst idx_end intSort 
 return $ M.fromList [("idx_start",(ast_start,intSort,0))
                     ,("idx_end"  ,(ast_end  ,intSort,0))]

queueModel :: Ident -> SSAVar -> Ident -> VId -> EnvOp AST
queueModel id@(Ident var) objVar@SSAVar{..} (Ident m) vId =
 case m of 
  "peek"    -> do 
   let (i,_,_) = safeLookup "queue.peek" "idx_start" _v_mod  
   lift $ mkSelect _v_ast i 
  "isEmpty" -> do
   let (i,_,_) = safeLookup "queue.isEmpty start" "idx_start" _v_mod  
       (j,_,_) = safeLookup "queue.isEmpty end"   "idx_end"   _v_mod  
   lift $ mkEq i j
  "remove"  -> do
    env@Env{..} <- get
    -- retrieve the head of the list
    res <- queueModel id objVar (Ident "peek") vId
    -- increment the idx_start 
    let (idx,idxSort,idxCnt) = safeLookup "queue.remove" "idx_start" _v_mod  
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
    let nVarMod = M.insert "idx_start" (ast_start,idxSort,nidxCnt) _v_mod
        nVar    = objVar { _v_mod = nVarMod }
        nssa    = insertSSAVar vId id nVar _e_ssamap 
    updateSSAMap nssa
    return res
  _ -> error $ "queueModel: not supported " ++ m 




