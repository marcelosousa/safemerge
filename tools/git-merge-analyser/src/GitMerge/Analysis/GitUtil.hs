-------------------------------------------------------------------------------
-- Module    :  GitMerge.Analysis.GitUtil
-- Copyright :  (c) 2017 Marcelo Sousa

-- This module implements several utility
-- functions that class Git commands 
-------------------------------------------------------------------------------
module GitMerge.Analysis.GitUtil where

import Control.Exception.Base
import System.Exit
import System.FilePath.Posix
import System.Process

my_catch :: IOException -> IO (ExitCode, String, String) 
my_catch _ = return (ExitFailure 1, "", "")

safe_call fn args = 
  catch (readProcessWithExitCode fn args []) my_catch 

git_call :: String -> [String] -> IO (Maybe String)
git_call cmd args = do
  (ex_code, str, _) <- safe_call "git" (cmd:args)
  if ex_code == ExitSuccess
  then return $ Just str
  else return Nothing 

git_merge_tree :: [String] -> IO (Maybe String)
git_merge_tree = git_call "merge-tree" 

git_merge_base :: [String] -> IO (Maybe String)
git_merge_base = git_call "merge-base" 

git_show :: String -> IO (Maybe String)
git_show arg = git_call "show" [arg]

git_log :: IO (Maybe String)
git_log = git_call "log" git_log_opts 
 where 
  git_log_opts :: [String]
  git_log_opts = ["--merges","--abbrev-commit","--decorate","--format=format:%H %P","--all"]
