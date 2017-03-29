-------------------------------------------------------------------------------
-- Module    :  GitMerge.Analysis.ChangeSet
-- Copyright :  (c) 2017 Marcelo Sousa
-- This module implements a simple analysis of 
-- the result of a git merge tree to identify
-- concurrent changes in a merge to a Java file
-------------------------------------------------------------------------------
module GitMerge.Analysis.ChangeSet (analyse_changes) where

import GitMerge.Analysis.Types
import System.FilePath.Posix

-- analyse_changes: analyse the result of git merge-tree
analyse_changes :: [String] -> [Change] -> [Change]
analyse_changes [] changes = changes
analyse_changes ls changes =
  let (ch,ls') = process_changes ls
  in if is_interesting ch 
     then analyse_changes ls' (ch:changes)
     else analyse_changes ls' changes

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
 where
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
