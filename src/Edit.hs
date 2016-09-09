{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit 
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

import qualified Debug.Trace as T

type Edit = [Stmt] 

main_edit_gen :: FilePath -> FilePath -> IO (CompilationUnit, Edit, Edit)
main_edit_gen orig var = do
  orig_ast <- parser compilationUnit `fmap` readFile orig
  var_ast  <- parser compilationUnit `fmap` readFile var 
  case (orig_ast, var_ast) of
    (Right o_ast, Right v_ast) -> return $ edit_gen o_ast v_ast 
    _ -> error "parse error..."

edit_gen :: CompilationUnit -> CompilationUnit -> (CompilationUnit, Edit, Edit)
edit_gen o_ast v_ast =
  case (o_ast, v_ast) of
						(CompilationUnit o_pkg o_imp o_ty, CompilationUnit v_pkg v_imp v_ty) -> undefined  
