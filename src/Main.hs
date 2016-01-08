{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Encoding
import qualified Examples.SimpleEncoding as SE
import qualified Examples.ToyLoop as TL
import Language.SMTLib2.Base
import Language.SMTLib2.Printer

import Prelude hiding (product)
import System.Console.CmdArgs
import System.FilePath.Posix

_program, _summary :: String
_summary = unlines ["wiz - v0.1","Semantic program merging."
                   ,"Copyright 2016 @ Marcelo Sousa"]
_program = "wiz"
_help    = "No input files supported yet."
_helpProduct = unlines ["wiz product -t=test_nr"]
_helpMerge = unlines ["wiz merge -t=test_nr -o=file.smt2"]
                   
data Option = Product {test_nr :: Int}
            | Merge {test_nr :: Int, output :: FilePath}
  deriving (Show, Data, Typeable, Eq)

productMode :: Option
productMode = Product { test_nr = def } &= help _helpProduct

mergeMode :: Option
mergeMode = Merge { test_nr = def 
                  , output = def } &= help _helpMerge

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [productMode, mergeMode]
         &= help _help
         &= program _program
         &= summary _summary
         
-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption (Product t) = product t
runOption (Merge t o) = merge t o 

product :: Int -> IO ()
product 0 = putStrLn $ pp_prod_prog $ generate_product SE.p SE.a SE.b SE.m 
product 1 = putStrLn $ pp_prod_prog $ generate_product TL.p TL.a TL.b TL.m 

merge :: Int -> FilePath -> IO ()
merge t o = writeFile o $ show $ prettyprint $ encode t

encode :: Int -> SMod
encode 0 = main_merge SE.p SE.a SE.b SE.m
encode 1 = main_merge TL.p TL.a TL.b TL.m 
