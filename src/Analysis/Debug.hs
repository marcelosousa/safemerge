{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Debug
-- Copyright :  (c) 2017 Marcelo Sousa
-- Utilities to Debug the Analysis
-------------------------------------------------------------------------------
module Analysis.Debug (menuText, prompt, wizPrint, wizBreak, debugger, printStat, printProg, printEdits) where

import Analysis.Java.AST
import Analysis.Pretty
import Analysis.Types
import Analysis.Util
import Calculus
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (join)
import Data.List
import Language.Java.Pretty
import Language.Java.Syntax
import System.Console.ANSI
import Z3.Monad

import qualified Data.Map as M

breaker :: String
breaker = "=========================================="
header  = "=============ANALYSER STATE==============="
subhead = "=================OPTIONS=================="
opt1    = "======= 1. Print current statement ======="
opt2    = "======= 2. Print edits             ======="
opt3    = "======= 3. Print program           ======="
opt4    = "======= 4. Print current VC        ======="
opt5    = "======= 5. Print SSA map           ======="
opt6    = "======= 6. Print full analyzer env ======="
opt7    = "======= c. Continue analysis       ======="
opt8    = "======= q. Exit                    ======="
prompt  = "======= Select Option: "

menuText :: String
menuText = unlines [breaker,header,subhead,opt1,opt2,opt3,opt4,opt5,opt6,opt7,opt8]

wizPrint :: String -> EnvOp ()
--wizPrint = liftIO . putStrLn 
wizPrint s = return () 

wizBreak :: EnvOp ()
wizBreak = do
 _ <- liftIO $ getChar
 return ()

debugger :: ProdProgram -> EnvOp ()
debugger prog = do
  -- liftIO $ clearScreen
  liftIO $ putStrLn menuText
  liftIO $ putStrLn prompt 
  debug
 where
  debug :: EnvOp ()
  debug = do
    env@Env{..} <- get
    c <- liftIO $ getLine
    liftIO $ clearLine
    if null c 
    then debugger prog
    else case head c of
      '1' -> printProg True  prog >> debug 
      '2' -> printEdits           >> debug 
      '3' -> printProg False prog >> debug 
      '4' -> printPre             >> debug
      '5' -> printSSA _e_ssamap   >> debug
      '6' -> printEnv             >> debug
      'c' -> return ()
      'q' -> error "Exiting..."
      _   -> debugger prog 

printStat :: AnnBlockStmt -> EnvOp ()
printStat bstmt = do
  let str = prettyPrint (fromAnn bstmt :: BlockStmt)
      scope = getAnn bstmt  
  liftIO $ putStrLn $ "Statement of versions " ++ show scope
  liftIO $ putStrLn str

printProg :: Bool -> ProdProgram -> EnvOp ()
printProg b stmts = do
 env@Env{..} <- get
 case stmts of
  [] -> liftIO $ putStrLn "End of Program" 
  (bstmt:_) ->
    if b 
    then printStat bstmt 
    else do 
      let str = unlines $ map (\s -> prettyPrint (fromAnn s :: BlockStmt)) stmts
      liftIO $ putStrLn "Program:"
      liftIO $ putStrLn str

printEdits :: EnvOp ()
printEdits = do
  env@Env{..} <- get
  let holes       = transpose $ M.elems _e_edits
      h_holes     = zip holes [1..]
      edits     h = zip h     [1..]
      h_edit    1 = "Edit Base:      "
      h_edit    2 = "Edit Variant A: "
      h_edit    3 = "Edit Variant B: "
      h_edit    4 = "Edit Merge:     "
      h_hole    h = "Hole " ++ show h ++ ":"
      printEdit (s,i) = h_edit i ++  prettyPrint (fromAnn s :: BlockStmt)
      printCode h = map printEdit $ edits h 
      printHole (h,i) = unlines ((h_hole i):(printCode h))
      str = unlines $ map printHole h_holes
  liftIO $ putStrLn $ "Edits:"
  liftIO $ putStrLn str 

printPre :: EnvOp ()
printPre = do
  env@Env{..} <- get
  cPhi    <- lift $ checkSAT _e_pre
  cPhiStr <- lift $ astToString _e_pre 
  solverStr <- lift $ solverToString
  liftIO $ putStrLn $ "Pre-condition (" ++ show cPhi ++ ")"
  liftIO $ putStrLn cPhiStr
  liftIO $ putStrLn $ show _e_pre 
  liftIO $ putStrLn solverStr 

printEnv :: EnvOp ()
printEnv = do
  env@Env{..} <- get
  wizPrint $ show _e_fnmap 

