{-# LANGUAGE DoAndIfThenElse #-}
module Main where

-- Project Liff: Identifies relevant merges based on the git log 

import Analysis.Java.ClassInfo
import Analysis.Java.Liff
import Control.Monad
import Data.List
import Data.Map (Map) 
import Data.Maybe
import GitMerge.Analysis.ChangeSet
import GitMerge.Analysis.GitUtil 
import GitMerge.Analysis.Types
import Language.Java.Parser 
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import System.Directory
import System.FilePath.Posix
import qualified Data.List as L 
import qualified Data.Map as M
import qualified Debug.Trace as T

trace a b = b

-- | Executable:
--   Executes liff over the set of git merges in a project
main :: IO ()
main = do
  mMerges <- git_log
  case mMerges of
    Nothing -> error "liff_project: failed to obtain the merges from git log"
    Just merges -> do
      let merge_lines = lines merges
      versions <- foldM merge_base [] merge_lines 
      relevant_merges <- foldM analyse_merge_tree [] versions 
      foldM_ liff_merge 1 relevant_merges 
      putStrLn "Done"

liff_merge :: Int -> GitMerge -> IO Int
liff_merge n m@(GitMerge args chs) = foldM (liff_main args) n chs

liff_main :: [String] -> Int -> Change -> IO Int
liff_main [o,m,a,b] i ch@(Change _ f) = do 
  (o_ast,o_str) <- parse_git_show o f >>= \m -> return $ maybe (error "liff_main") id m 
  (a_ast,a_str) <- parse_git_show a f >>= \m -> return $ maybe (error "liff_main") id m 
  (b_ast,b_str) <- parse_git_show b f >>= \m -> return $ maybe (error "liff_main") id m 
  (m_ast,m_str) <- parse_git_show m f >>= \m -> return $ maybe (error "liff_main") id m 
  let r = liff o_ast a_ast b_ast m_ast
  if null $ _merges r
  then return i
  else do
    let dir = "results/inst"++show i++"/"
        fl  = takeBaseName f
        f_o = dir ++ fl ++ "_o.java"
        f_a = dir ++ fl ++ "_a.java"
        f_b = dir ++ fl ++ "_b.java"
        f_m = dir ++ fl ++ "_m.java"
    putStrLn $ "liff_main: found changes in " ++ f ++ "\n" ++ show [o,a,b,m]
    putStrLn $"\t: printing to files " ++ show [f_o,f_a,f_b,f_m]
    createDirectoryIfMissing True dir
    writeFile f_o o_str
    writeFile f_a a_str
    writeFile f_b b_str
    writeFile f_m m_str
    return $ i + 1

-- | API to retrieve & process information from git commands
-- | Analyse a git merge-tree result
analyse_merge_tree :: [GitMerge] -> [String] -> IO [GitMerge]
analyse_merge_tree gmerges as@[o,m,a,b] = do 
  let args = [o,a,b]
  mStr <- git_merge_tree args
  case mStr of
    Nothing -> return gmerges
    Just str -> do
      let str_lines = lines str
          changes = analyse_changes str_lines []
          gmerge = GitMerge as changes
      if not $ null changes
      then return (gmerge:gmerges) 
      else return gmerges 

-- ^ merge_base: retrieves the hash of the base based on the variants
merge_base :: [[String]] -> String -> IO [[String]]
merge_base res merge_line = do
  let args = words merge_line
      vars = tail args
  mStr <- git_merge_base vars
  case mStr of
    Nothing -> return res
    Just str -> do
      let o = lines str
      return ((o++args):res)

-- | Parse the result of a git show
parse_git_show :: String -> FilePath -> IO (Maybe (Program, String))
parse_git_show hash file = do
  let arg = hash++":"++file
  mStr <- git_show arg
  case mStr of
    Nothing -> return Nothing -- error "parse: git show was unsucessful" 
    Just str -> do 
      let ast_ = parser compilationUnit str 
      case ast_ of
        Right ast -> return $ Just (ast, str) 
        Left err -> return Nothing -- error $ "parse error..." ++ show err
