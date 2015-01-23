module Main where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.AST as SC
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Merger

import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State.Strict
import Debug.Trace

parseFile :: FilePath -> IO CTranslUnit
parseFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

printCAST :: CTranslUnit -> IO ()
printCAST ctu = print ctu -- (print . pretty) ctu

printSCAST :: CTranslUnit -> IO ()
printSCAST ctu = print $ translate ctu

parseMain fn f = parseFile fn >>= f

mainMerge :: FilePath -> FilePath -> FilePath -> IO ()
mainMerge base v1 v2 = do
  abase <- parseFile base
  av1   <- parseFile v1
  av2   <- parseFile v2
  let cbase = translate abase
      cv1 = translate av1
      cv2 = translate av2
  case mergeProg cbase cv1 cv2 of
    Nothing -> error "failed to merge"
    Just cmerge -> print cmerge
