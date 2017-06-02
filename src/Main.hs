{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Main where

import Analysis.Dependence
import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Analysis.Java.Flow
import Analysis.Java.Liff
import Analysis.Optimiser
import Analysis.Verifier
import Edit
import Edit.Types
import Edit.Gen
import Edit.Print
-- import Language.SMTLib2.Base
-- import Language.SMTLib2.Printer
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Prelude hiding (product)
import System.Console.CmdArgs
import System.FilePath.Posix
import qualified Data.Map as M

_program, _summary :: String
_summary = unlines ["wiz - v0.1","Semantic program merging."
                   ,"Copyright 2016 @ Marcelo Sousa"]
_program = "wiz"
_help    = "No input files provided."
_helpParse   = "wiz parse -f=file"
_helpProduct = "wiz product -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt"
_helpDiff    = "wiz diff -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt"
_helpVerify  = "wiz verify -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt"
_helpDiff2   = "wiz diff2 -p=prog.txt -a=variant-a.txt"
_helpMerge   = "wiz merge -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt -o=file.smt2"

data InvGen = Houdini | Horn 
  deriving (Show, Data, Typeable, Eq)

instance Default InvGen where
  def = Houdini 
 
data Option = Parse   { prog :: FilePath }
            | Diff2   { prog :: FilePath, a :: FilePath }
            | Diff4   { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath }
            | Verify  { base :: FilePath } 
            | Product { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath }
            | Merge   { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath, output :: FilePath }
  deriving (Show, Data, Typeable, Eq)

parseMode, productMode, mergeMode :: Option
parseMode = Parse { prog = def } &= help _helpParse
productMode = Product { prog = def, a = def
                      , b = def, merge = def } &= help _helpProduct
diffMode = Diff4 { prog = def, a = def, b = def
                , merge = def } &= help _helpDiff
verifyMode = Verify { base = def } &= help _helpVerify 
diff2Mode = Diff2 { prog = def, a = def } &= help _helpDiff2
mergeMode = Merge { prog = def, a = def, b = def
                  , merge = def, output = def } &= help _helpMerge

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [parseMode, productMode, mergeMode
                                ,diffMode, diff2Mode, verifyMode]
         &= help _help
         &= program _program
         &= summary _summary
         
-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption opt = case opt of
  Parse f -> do
    prog <- parse f
    putStrLn $ prettyPrint prog
    let flowInfo = computeGraphs prog
        classInfo = toClassInfo prog
        graphs = M.map snd flowInfo
        depInfo = depAnalysis classInfo flowInfo 
    putStrLn $ pp_dot_graphs graphs
    putStrLn $ printDepInfo depInfo
  Diff2 o a -> diff2 o a
  Diff4 o a b m -> diff4 o a b m
  Verify f -> do
    let o = f ++ "_o.java"
        a = f ++ "_a.java"
        b = f ++ "_b.java"
        m = f ++ "_m.java"
    verify o a b m
  _ -> error $ "wiz: option currently not supported"

-- | Some utility functions
parse :: FilePath -> IO Program
parse f = do
  ast_ <- parser compilationUnit `fmap` readFile f 
  case ast_ of
    Right ast -> return ast 
    Left err -> error $ "parse error..." ++ show err

-- | Parses the 4 files
parse4 :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Program, Program, Program, Program)
parse4 ofl afl bfl mfl = do
  putStrLn "Parsing files..."
  o <- parse ofl 
  a <- parse afl 
  b <- parse bfl 
  m <- parse mfl 
  return (o,a,b,m)

diff2 :: FilePath -> FilePath -> IO () 
diff2 orig var = do
  o_ast <- parse orig 
  v_ast <- parse var 
  let (no_ast, o_edit, v_edit) = edit_gen o_ast v_ast
  putStrLn $ "Program with holes:" 
  putStrLn $ prettyPrint no_ast
  putStrLn $ "Edit Base:" 
  print_edit_simple o_edit
  putStrLn $ "Edit Variant:" 
  print_edit_simple v_edit 
 
-- Main function that gets the edit scripts
diff4 :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
diff4 ofl afl bfl mfl = do
  (o,a,b,m) <- parse4 ofl afl bfl mfl
  let (fo,e_o,e_a,e_b,e_m) = diff4gen o a b m 
      pairs = [(o,fo,e_o),(a,fo,e_a),(b,fo,e_b),(m,fo,e_m)]
      es = [e_o,e_a,e_b,e_m]
  putStrLn "Program with holes:"
  putStrLn $ prettyPrint fo 
  putStrLn ""
  mapM_ print_edit $ zip es [0..]
  let res = map check_edit_soundness pairs
  if all id res 
  then putStrLn "Edits are sound"
  else do
    putStrLn "Some edit when applied does not yield the original program"
    putStrLn $ show res

-- Main function 
-- | Given 4 Java files generate a merge instance
--   with edit scripts and a program with holes.
verify :: FilePath -> FilePath -> FilePath -> FilePath -> IO () 
verify ofl afl bfl mfl = do
  -- parses the files
  (oast,aast,bast,mast) <- parse4 ofl afl bfl mfl
  -- converts to class info and finds which methods contain changes
  let mergeInst = liff oast aast bast mast
  -- computes per method, the edit scripts and the method with holes
      diffInst = diffMethods mergeInst
  putStrLn $ printMethInsts $ _merges diffInst
  wiz diffInst 
