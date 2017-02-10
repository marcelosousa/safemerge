module Types where

import System.FilePath.Posix
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L 
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

data Merge = Merge
  {
    variants :: [String]
  , changes  :: [Change]
  , valid    :: Bool
  }

instance Show Merge where
  show (Merge [o,m,a,b] changes valid) = 
    let os = "Base commit:  " ++ o ++ "\n"
        as = "A commit:     " ++ a ++ "\n"
        bs = "B commit:     " ++ b ++ "\n"
        ms = "Merge commit: " ++ m ++ "\n"
        v  = "Valid: " ++ show valid ++ "\n"
    in os++as++bs++ms++(print_merges changes)

print_merges :: (Show a) => [a] -> String
print_merges = foldr (\h r -> show h ++ "\n" ++ r) ""

data Change = Change 
  {
    ty   :: ChangeType
  , file :: String
  }
  deriving Show

data ChangeType = 
  Merged | ChangedBoth | Removed | Added | AddedBoth
  deriving Show

type Program = CompilationUnit

-- A qualified method name is a pair (class, method signature)
type QName = (Ident, Ident, MethodBody)
type SQName = (Ident, Ident)

-- Information regarding the methods of a compilation unit
-- Map (ClassName, MethodName) [MethodBody]
--  ClassName has to be a list of Class Identifiers to take care of 
--  inner classes
--  MethodName is just the identifier and because of that we need to record multiple methods
-- Going to simplify in a first stage
type MethInfo = Map Ident MethodBody
type ClassInfo = Map Ident MethInfo

