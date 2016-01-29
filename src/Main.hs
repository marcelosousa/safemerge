{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Encoding
import Language.SMTLib2.Base
import Language.SMTLib2.Printer
import Parser
import Prelude hiding (product)
import Printer
import Product
import System.Console.CmdArgs
import System.FilePath.Posix

_program, _summary :: String
_summary = unlines ["wiz - v0.1","Semantic program merging."
                   ,"Copyright 2016 @ Marcelo Sousa"]
_program = "wiz"
_help    = "No input files supported yet."
_helpParse = unlines ["wiz parse -m={program|edit} -f=file"]
_helpProduct = unlines ["wiz product -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt"]
_helpMerge = unlines ["wiz merge -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt -o=file.smt2"]
_helpFMerge = unlines ["wiz finermerge -p=prog.txt -a=variant-a.txt -b=variant-b.txt -m=merge.txt -o=file.smt2"]

data ParseOption = Program | Edit
  deriving (Show, Data, Typeable, Eq)

instance Default ParseOption where
  def = Program
 
data Option = Parse { mode :: ParseOption, file :: FilePath }
            | Product {prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath }
            | Merge { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath, output :: FilePath }
            | FinerMerge { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath, output :: FilePath }
            | WithGuardFinerMerge { prog :: FilePath, a :: FilePath, b :: FilePath, merge :: FilePath, output :: FilePath }
  deriving (Show, Data, Typeable, Eq)

parseMode, productMode, mergeMode, finermergeMode :: Option
parseMode = Parse { mode = def, file = def } &= help _helpParse
productMode = Product { prog = def, a = def
                      , b = def, merge = def } &= help _helpProduct
mergeMode = Merge { prog = def, a = def, b = def
                  , merge = def, output = def } &= help _helpMerge
finermergeMode = FinerMerge { prog = def, a = def, b = def
                            , merge = def, output = def } &= help _helpFMerge
withfinermergeMode = WithGuardFinerMerge { prog = def, a = def, b = def
                            , merge = def, output = def } &= help _helpFMerge

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [parseMode, productMode, mergeMode, finermergeMode, withfinermergeMode]
         &= help _help
         &= program _program
         &= summary _summary
         
-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption (Parse m f) = do
  s <- readFile f
  case m of
    Program -> putStrLn $ pp_program $ parseProg s
    Edit -> putStrLn $ pp_edit $ parseEdit s
runOption (Product p a b m) = do
  p_s <- readFile p >>= return . parseProg
  a_s <- readFile a >>= return . parseEdit
  b_s <- readFile b >>= return . parseEdit
  m_s <- readFile m >>= return . parseEdit
  let pprod = generate_product p_s a_s b_s m_s
  putStrLn $ pp_dot_prod_prog $ fst pprod  
  --putStrLn $ pp_prod_prog pprod 
runOption (Merge p a b m o) = do
  p_s <- readFile p >>= return . parseProg
  a_s <- readFile a >>= return . parseEdit
  b_s <- readFile b >>= return . parseEdit
  m_s <- readFile m >>= return . parseEdit
  let enc = encode False p_s a_s b_s m_s
  writeFile o $ show $ prettyprint enc 
runOption (FinerMerge p a b m o) = do
  p_s <- readFile p >>= return . parseProg
  a_s <- readFile a >>= return . parseEdit
  b_s <- readFile b >>= return . parseEdit
  m_s <- readFile m >>= return . parseEdit
  let enc = fine_encode False p_s a_s b_s m_s
  writeFile o $ show $ prettyprint enc 
runOption (WithGuardFinerMerge p a b m o) = do
  p_s <- readFile p >>= return . parseProg
  a_s <- readFile a >>= return . parseEdit
  b_s <- readFile b >>= return . parseEdit
  m_s <- readFile m >>= return . parseEdit
  let enc = encode True p_s a_s b_s m_s
  writeFile o $ show $ prettyprint enc 
