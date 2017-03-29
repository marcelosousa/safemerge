{-# LANGUAGE RecordWildCards #-}
module GitMerge.Analysis.Types where

import System.FilePath.Posix
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L 
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import System.FilePath.Posix

-- Data Type to represent a Git Merge
data GitMerge = GitMerge
  {
    _gm_vars :: [String]
  , _gm_chgs :: [Change]
  }

instance Show GitMerge where
  show (GitMerge [o,m,a,b] changes) = 
    let os = "Base commit:  " ++ o ++ "\n"
        as = "A commit:     " ++ a ++ "\n"
        bs = "B commit:     " ++ b ++ "\n"
        ms = "Merge commit: " ++ m ++ "\n"
    in os++as++bs++ms++(print_merges changes)

print_merges :: (Show a) => [a] -> String
print_merges = foldr (\h r -> show h ++ "\n" ++ r) ""

-- Data type to represent a change in a Git merge
data Change = Change 
  {
    _ch_ty  :: ChangeType
  , _ch_src :: FilePath 
  }
  deriving Show

data ChangeType = 
  Merged | ChangedBoth | Removed | Added | AddedBoth
  deriving Show

