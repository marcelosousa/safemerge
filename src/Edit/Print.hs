{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Print
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit.Print where

import Edit.Types

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

printEdit :: Edit -> String
printEdit e = unlines $ map (\(n,(s,sc)) -> "Hole " ++ show n ++ ":\n" ++ show sc ++ ": " ++ prettyPrint s) $ zip [1..] e 

printProg :: [BlockStmt] -> String
printProg e = unlines $ map prettyPrint e 

print_edit_simple :: Edit -> IO ()
print_edit_simple e = do
  mapM_ (\(e,s) -> putStrLn $ show s ++ ": " ++ prettyPrint e) e
  putStrLn ""

print_edit :: (Edit, Int) -> IO ()
print_edit (e,n) = do
  putStrLn $ to_s n
  mapM_ (\(e,s) -> putStrLn $ show s ++ ": " ++ prettyPrint e) e
  putStrLn ""
 
to_s :: Int -> String
to_s 0 = "base edit:"
to_s 1 = "variant a edit:"
to_s 2 = "variant b edit:"
to_s 3 = "merge edit:"
