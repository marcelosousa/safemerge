module Main where

import System.Exit
import System.Process
import Control.Monad
import Control.Exception.Base
import System.FilePath.Posix
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L 
import Types
import Liff

-- process the result of git merge-tree
process :: [String] -> [Change] -> [Change]
process [] changes = changes
process ls changes =
  let (ch,ls') = process_changes ls
  in process ls' (ch:changes)

process_changes :: [String] -> (Change,[String])
process_changes [] = error "process_changes: invalid input"
process_changes (h:t) = case h of
  "changed in both"   -> process_change ChangedBoth t 
  "merged"            -> process_change Merged      t
  "removed in remote" -> process_change Removed     t 
  "removed in local" ->  process_change Removed     t 
  "added in remote"   -> process_change Added       t 
  "added in both"     -> process_change AddedBoth   t 
  _ -> error $ "new kind of change " ++ h

process_change :: ChangeType -> [String] -> (Change,[String])
process_change ty lns = case lns of
  (a:rest) -> let ls = dropWhile (\l -> not $ elem l keywords) rest
              in (Change ty (last $ words a), ls) 
  _ -> error $ "process_change: unexpected format\n" ++ show lns

keywords :: [String] 
keywords = 
 [
   "changed in both"   
 , "merged"           
 , "removed in remote"
 , "added in remote"  
 , "added in both"    
 ]

is_interesting :: Change -> Bool
is_interesting c@(Change ty file) = 
  let c1 = case ty of 
        ChangedBoth -> True
        _ -> False 
      -- c2 = takeExtension file == ".c"
      c2 = takeExtension file == ".java"
  in c1 && c2

get_merge :: [String] -> IO Merge
get_merge as@[o,m,a,b] = do 
  let args = [o,a,b]
  (ex_code, str, _) <- safe_call "git" ("merge-tree":args)
  if ex_code == ExitSuccess
  then do 
    let str_lines = lines str
        changes = filter is_interesting $ process str_lines []
    return $ Merge as changes True
  else do
    return $ Merge as [] False

git_opts :: [String]
git_opts = ["log", "--merges", "--abbrev-commit", "--decorate", "--format=format:%H %P", "--all"]

is_of_interest :: Merge -> Bool
is_of_interest (Merge _ ch va) = va && (not $ null ch)

process_merge :: [[String]] -> [String] -> IO [[String]]
process_merge res args = do
  let vars = tail args
  (ex_code, str, _) <- safe_call "git" ("merge-base":vars)
  if ex_code == ExitSuccess
  then do
    let o = lines str
    return ((o++args):res)
  else return res  

main :: IO ()
main = do
  merges <- readProcess "git" git_opts []
  let merge_lines = lines merges
      args = map words merge_lines
  versions <- foldM process_merge [] args 
  diffs <- mapM get_merge versions 
  let both_changed_files = filter is_of_interest diffs
  -- putStrLn $ print_merges both_changed_files
  meths' <- mapM liff both_changed_files
  -- let meths = filter (\(_,x) -> not $ null x) meths' 
  -- putStrLn $ print_merges meths 
  putStrLn "Done"

