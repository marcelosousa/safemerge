{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Edit
import Edit.Types
import Edit.Gen
import Edit.Print
-- import Encoding
-- import Language.SMTLib2.Base
-- import Language.SMTLib2.Printer
-- import Parser
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Prelude hiding (product)
-- import Printer
-- import Product
import System.Console.CmdArgs
import System.FilePath.Posix

_program, _summary :: String
_summary = unlines ["wiz - v0.1","Semantic program merging."
                   ,"Copyright 2016 @ Marcelo Sousa"]
_program = "wiz"
_help    = "No input files provided."
_helpParse   = "wiz parse -f=file"
_helpProduct = "wiz product -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt"
_helpDiff    = "wiz diff -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt"
_helpDiff2   = "wiz diff2 -p=prog.txt -a=variant-a.txt"
_helpMerge   = "wiz merge -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt -o=file.smt2"

data InvGen = Houdini | Horn 
  deriving (Show, Data, Typeable, Eq)

instance Default InvGen where
  def = Houdini 
 
data Option = Parse   { file :: FilePath }
            | Diff2   { prog :: FilePath, a :: FilePath }
            | Diff4   { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath }
            | Product { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath }
            | Merge   { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath, output :: FilePath }
  deriving (Show, Data, Typeable, Eq)

parseMode, productMode, mergeMode :: Option
parseMode = Parse { file = def } &= help _helpParse
productMode = Product { prog = def, a = def
                      , b = def, merge = def } &= help _helpProduct
diffMode = Diff4 { prog = def, a = def, b = def
                , merge = def } &= help _helpDiff
diff2Mode = Diff2 { prog = def, a = def } &= help _helpDiff2
mergeMode = Merge { prog = def, a = def, b = def
                  , merge = def, output = def } &= help _helpMerge

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [parseMode, productMode, mergeMode, diffMode, diff2Mode]
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
  Diff2 o a -> diff2 o a
  Diff4 o a b m -> diff4 o a b m
--  Product p a b m -> do
--    p_s <- readFile p >>= return . parseProg
--    a_s <- readFile a >>= return . parseEdit
--    b_s <- readFile b >>= return . parseEdit
--    m_s <- readFile m >>= return . parseEdit
--    let pprod = generate_product p_s a_s b_s m_s
--    putStrLn $ pp_dot_prod_prog $ fst pprod  
--    --putStrLn $ pp_prod_prog pprod 
--  Merge p a b m o -> do
--    p_s <- readFile p >>= return . parseProg
--    a_s <- readFile a >>= return . parseEdit
--    b_s <- readFile b >>= return . parseEdit
--    m_s <- readFile m >>= return . parseEdit
--    let enc = encode False p_s a_s b_s m_s
--    -- let enc = fine_encode False p_s a_s b_s m_s
--    -- let enc = encode True p_s a_s b_s m_s
--    -- let enc = fine_encode True p_s a_s b_s m_s
--    writeFile o $ show $ prettyprint enc 

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
