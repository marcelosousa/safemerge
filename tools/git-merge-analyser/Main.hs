module Main where

import System.Exit
import System.Process
import Control.Exception.Base
import System.FilePath.Posix
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L 

{-
 changed in both
 merged
 @@ 
-}

data Merge = Merge
  {
    variants :: [String]
  , changes :: [Change]
  , valid :: Bool
  }

--data DiffMerge = DiffMerge [String] ([DiffChange], [DiffChange]) Bool
--data DiffMerge = DiffMerge [String] (Map String [String], Map String [String]) Bool
data DiffMerge = DiffMerge [String] (Map String [String]) Bool

instance Show Merge where
  show (Merge [o,m,a,b] changes valid) = 
    let os = "Base commit:  " ++ o ++ "\n"
        as = "A commit:     " ++ a ++ "\n"
        bs = "B commit:     " ++ b ++ "\n"
        ms = "Merge commit: " ++ m ++ "\n"
        v  = "Valid: " ++ show valid ++ "\n"
    in os++as++bs++ms++(print_merges changes)

instance Show DiffMerge where
  show (DiffMerge [o,m,a,b] ch valid) = 
    let os = "Base commit:  " ++ o ++ "\n"
        as = "A commit:     " ++ a ++ "\n"
        bs = "B commit:     " ++ b ++ "\n"
        ms = "Merge commit: " ++ m ++ "\n"
        v  = "Valid: " ++ show valid ++ "\n"
        acs  = "Same changes:\n" ++ print_merges (M.toList ch) ++ "\n"
    in os++as++bs++ms++acs

data ChangeType = Merged | ChangedBoth | Removed | Added | AddedBoth
  deriving Show

is_of_interest :: Merge -> Bool
is_of_interest (Merge _ ch va) = va && (not $ null ch)

is_dof_interest :: DiffMerge -> Bool
is_dof_interest (DiffMerge _ ch va) = va && (not $ null ch)

is_merged :: Change -> Bool
is_merged (Change ty file meth) = case ty of
  Merged -> True
  _ -> False

is_interesting :: Change -> Bool
is_interesting c@(Change ty file meth) = 
  let c1 = not $ is_merged c
      c2 = takeExtension file == ".java"
     -- c3 = not $ null meth
  in c1 && c2

data Change = Change 
  {
    ty   :: ChangeType
  , file :: String
  , meth :: [String] 
  }
  deriving Show

data DiffChange = DiffChange String [String]
  deriving Show

is_dinteresting :: String -> [String] -> Bool
is_dinteresting file meth = 
  let c1 = takeExtension file == ".java"
      c2 = not $ null meth
  in c1 && c2

get_scope :: String -> Maybe String
get_scope s =
  case words s of
    ("@@":r) -> let x = dropWhile (/= "@@") r 
                in Just $ takeWhile (/= '(') $ unwords $ tail x
    _ -> Nothing

print_merges :: (Show a) => [a] -> String
print_merges = foldr (\h r -> show h ++ "\n" ++ r) ""

process_merges :: [String] -> [Change]
process_merges lines = process lines []

-- this process the result of git merge-tree
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
  "added in remote"   -> process_change Added       t 
  "added in both"     -> process_change AddedBoth   t 
  _ -> error $ "new kind of change " ++ h

keywords :: [String] 
keywords = 
 [
   "changed in both"   
 , "merged"           
 , "removed in remote"
 , "added in remote"  
 , "added in both"    
 ]

process_change :: ChangeType -> [String] -> (Change,[String])
process_change ty ls = case ls of
  (a:rest) -> let (meths, ls') = advance_next keywords ([],rest)
              in (Change ty (last $ words a) meths, ls') 
  _ -> error $ "process_change: unexpected format\n" ++ show ls

process_diffs :: [String] -> Map String [String]
process_diffs lines = process_d lines M.empty

process_d :: [String] -> Map String [String] -> Map String [String]
process_d [] changes = changes
process_d ls changes =
  let (k,ch,ls') = process_d_changes ls
  in process_d ls' $ M.insert k ch changes

process_d_changes :: [String] -> (String, [String], [String])
process_d_changes [] = error "process_d_changes: invalid input"
process_d_changes (h:t) = case words h of
  ("diff":rest) ->
    let fl = drop 2 $ last rest
        (meths,ls) = advance_next ["diff"] ([],t)
    in (fl, L.nub meths, ls)  
  _ -> error $ "new kind of change " ++ h
 
advance_next :: [String] -> ([String],[String]) -> ([String],[String])
advance_next keys (ms,[]) = (ms,[])
advance_next keys (ms,l@(h:t)) =
  if h `elem` keys
  then (ms,l)
  else case get_scope h of
    Nothing -> advance_next keys (ms,t)
    Just "" -> advance_next keys (ms,t)
    Just s  -> advance_next keys (s:ms,t)

my_catch :: IOException -> IO (ExitCode, String, String) 
my_catch _ = return (ExitFailure 1, "", "")
 
get_merge :: [String] -> IO Merge
get_merge as@[o,m,a,b] = do 
  let args = [o,a,b]
  -- putStrLn $ "get_merge: " ++ show args
  -- h <- getChar
  (ex_code, str, _) <- catch (readProcessWithExitCode "git" ("merge-tree":args) []) my_catch 
  if ex_code == ExitSuccess
  then do 
    let str_lines = lines str
        changes = filter is_interesting $ process_merges str_lines
    return $ Merge as changes True
  else do
    return $ Merge as [] False

get_merge_diff :: [String] -> IO DiffMerge
get_merge_diff as@[o,m,a,b] = do 
  putStrLn $ "Doing the git diff between " ++ o ++ " " ++ a ++ " " ++ b
  (a_ex_code, a_str, _) <- catch (readProcessWithExitCode "git" ["diff",o,a] []) my_catch 
  (b_ex_code, b_str, _) <- catch (readProcessWithExitCode "git" ["diff",o,b] []) my_catch 
  if a_ex_code == ExitSuccess && b_ex_code == ExitSuccess
  then do
    -- process the diff a 
    let a_str_lines = lines a_str
        a_changes = M.filterWithKey is_dinteresting $ process_diffs a_str_lines
        b_str_lines = lines b_str
        b_changes = M.filterWithKey is_dinteresting $ process_diffs b_str_lines
        ch = M.filter (not . null) $  M.intersectionWith (L.intersect) a_changes b_changes
    return $ DiffMerge as ch True
  else do
    return $ DiffMerge as M.empty False

git_opts :: [String]
git_opts = ["log", "--merges", "--abbrev-commit", "--decorate", "--format=format:'%H %P'", "--all"]

git_merge_tree :: IO ()
git_merge_tree = do
  merges <- readProcess "git" git_opts []
  let merge_lines = lines merges
      args = map words merge_lines
      (m,mvars) = unzip $ map (\l -> (head l, tail l)) args 
  bases <- mapM (\l -> readProcess "git" ("merge-base":l) []) mvars
  let variants = map (\(o,l) -> lines o ++ l) $ zip bases args 
  merges <- mapM get_merge variants
  let merges' = filter is_of_interest merges 
  putStrLn $ print_merges merges' 

main :: IO ()
main = do
  merges <- readProcess "git" git_opts []
  let merge_lines = lines merges
      args = map (\l -> words $ init $ tail l) merge_lines
      (m,mvars) = unzip $ map (\l -> (head l, tail l)) args 
  bases <- mapM (\l -> readProcess "git" ("merge-base":l) []) mvars
  let variants = map (\(o,l) -> lines o ++ l) $ zip bases args 
  merges <- mapM get_merge_diff variants
  let merges' = filter is_dof_interest merges 
  putStrLn $ print_merges merges' 
