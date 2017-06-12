{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Pretty
-- Copyright :  (c) 2017 Marcelo Sousa
-- Pretty Printing
-------------------------------------------------------------------------------
module Analysis.Pretty where

import Analysis.Types
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (join)
import Data.Map (Map)
import Z3.Monad
import qualified Data.Map as M

printSSAVarModel :: SSAVarModel -> Z3 String
printSSAVarModel m = 
  foldM (\str (ast,ty,k) -> do
            ast_str <- astToString ast
            typ_str <- sortToString ty
            return (show k ++ ":(" ++ ast_str ++","++typ_str++"), " ++ str) 
        ) "" $ M.elems m 

printSSAVar :: SSAVar -> Z3 String
printSSAVar (SSAVar ast typ cnt mod mty) = do 
  ast_str <- astToString ast
  typ_str <- sortToString typ
  mod_str <- printSSAVarModel mod
  return $ show cnt ++ " ~> (" ++ ast_str ++ ","++show mty++","++typ_str++", model = " ++ mod_str ++ ")"

printSSAVer :: SSAVer -> Z3 String
printSSAVer m = do 
  strs <- mapM (\(i,var) -> do
            var_str <- printSSAVar var 
            return $ "\t" ++ show i ++ "_" ++ var_str) $ M.toList m
  return $ unlines strs

printSSA :: SSAMap -> EnvOp ()
printSSA ssa = do 
  strs <- mapM (\(k,m) -> do 
    inner <- lift $ printSSAVer m
    let h = show k ++ " ->\n"
    return $ h ++ inner) $ M.toList ssa 
  let str = unlines strs
  liftIO $ putStrLn $ "SSA map:"
  liftIO $ putStrLn str 

showSSAMap :: SSAMap -> String
showSSAMap = 
  M.foldWithKey (\i m r -> show i ++ " -> " ++ show m ++ "\n" ++ r) "" 

