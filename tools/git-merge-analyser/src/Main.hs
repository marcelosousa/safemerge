{-# LANGUAGE DoAndIfThenElse, RecordWildCards #-}
module Main where

-- Project Liff: Identifies relevant merges based on the git log 

import Analysis.Java.ClassInfo
import Analysis.Java.Liff
import Edit 
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
          stats = init_stats $ length merge_lines
      -- in each element of merge_lines, we have [merge,varA,varB]
      versions <- foldM merge_base [] merge_lines 
      (stats',relevant_merges) <- foldM analyse_merge_tree (stats,[]) versions 
      (res_stats,k) <- foldM liff_merge (stats',1) relevant_merges 
      putStrLn $ show res_stats
      putStrLn "Done"

liff_merge :: (Stats,Int) -> GitMerge -> IO (Stats,Int)
liff_merge (stats,n) m@(GitMerge args chs) = foldM (liff_main args) (stats,n) chs

-- | Checks the changes per Java file
liff_main :: [String] -> (Stats,Int) -> Change -> IO (Stats,Int)
liff_main [o,m,a,b] (stats,i) ch@(Change _ f) = do 
  m_o <- parse_git_show o f
  m_a <- parse_git_show a f
  m_b <- parse_git_show b f
  m_m <- parse_git_show m f
  case (m_o,m_a,m_b,m_m) of
    (Just (o_ast,o_str), Just (a_ast,a_str),Just (b_ast,b_str),Just (m_ast,m_str)) -> do
      let (lstats,mergeInst) = liff o_ast a_ast b_ast m_ast
          stats' = inc_liff_stats lstats stats
          diffInst = diffMethods mergeInst
          merges = _merges diffInst
      -- putStrLn $ show lstats
      -- putStrLn $ show stats'
      if null merges 
      then return (stats',i)
      else do
        k <- foldM (\_i ident -> printInst _i f ident (l_map lstats) o_str a_str b_str m_str) i merges 
        return (stats',k + 1)
    _ -> return (inc_show_errs stats,i)

printInst :: Int -> FilePath -> MethInst -> Map MIdent MSummary -> String -> String -> String -> String -> IO Int  
printInst i f m@(ident,_,_,_,_,_) map o_str a_str b_str m_str = do 
  dir <- getDir ident map i 
  let fl  = takeBaseName f
      f_o = dir ++ fl ++ "_o.java"
      f_a = dir ++ fl ++ "_a.java"
      f_b = dir ++ fl ++ "_b.java"
      f_m = dir ++ fl ++ "_m.java"
      log = dir ++ "diff.txt"
  putStrLn "-------------------------------------------"
  putStrLn $ "Inst: " ++ show i
  -- putStrLn $ "File: " ++ f 
  -- putStrLn $ "Meth: " ++ show (_merges r)
  -- putStrLn $ "Orig: " ++ o
  -- putStrLn $ "VarA: " ++ a
  -- putStrLn $ "VarB: " ++ b
  -- putStrLn $ "Merg: " ++ m
  writeFile f_o o_str
  writeFile f_a a_str
  writeFile f_b b_str
  writeFile f_m m_str
  writeFile log $ printMethInst m
  return (i+1)

getDir :: MIdent -> Map MIdent MSummary -> Int -> IO String
getDir ident map i = do 
  case M.lookup ident map of
    Nothing -> do
      let dir = "results/misc/inst"++show i++"/" 
      createDirectoryIfMissing True dir
      return dir 
    Just s@MSum{..} -> do 
      let h = "results/cat"++show _m_ctx ++"/" 
      case _m_diff of
        Nothing -> do 
          let dir = h ++ show _m_meth ++ "/" ++ show _m_edit ++ "/inst" ++ show i ++ "/"
          createDirectoryIfMissing True dir
          return dir 
        Just  e -> do
          let dir = h ++ "differr/inst" ++ show i ++ "/"
              err = dir ++ "diff.log"
          createDirectoryIfMissing True dir
          writeFile err e
          return dir 
       
-- | API to retrieve & process information from git commands
-- | Analyse a git merge-tree result
analyse_merge_tree :: (Stats,[GitMerge]) -> [String] -> IO (Stats,[GitMerge])
analyse_merge_tree (stats,gmerges) as@[o,m,a,b] = 
  -- removes the triangle pattern:
  --     m
  --     | \
  --     |  b
  --     | /
  --     a
  if o == a || o == b
  then return (inc_trivial stats, gmerges)
  else do
    let args = [o,a,b]
    mStr <- git_merge_tree args
    case mStr of
      Nothing -> return (inc_error stats, gmerges)
      Just str -> do
        let str_lines = lines str
            (stats',changes) = analyse_changes str_lines (stats,[])
            gmerge = GitMerge as changes
        if not $ null changes
        then return (inc_java_merges stats', gmerge:gmerges) 
        else return (inc_other_merges stats',gmerges) 
analyse_merge_tree (stats,gmerges) _ = return (inc_error stats, gmerges)

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
