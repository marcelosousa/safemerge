{-# LANGUAGE RecordWildCards #-}
module Analysis.Liff.Types where

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

type MemberInfo = Map Ident MemberDecl 
type ClassInfo = Map Ident ClassSum 

-- Information regarding the methods of a compilation unit
data ClassSum = 
  ClassSum {
    _cl_name   :: [Ident]    -- List of class names from the current to Object
  , _cl_fields :: MemberInfo -- List of fields
  , _cl_meths  :: MemberInfo -- List of methods
  , _cl_cons   :: MemberInfo -- List of constructors
  -- Missing information such as inner classes and interfaces
  }

i_clsum :: [Ident] -> ClassSum
i_clsum name = ClassSum name M.empty M.empty M.empty 

is_null :: ClassSum -> Bool
is_null (ClassSum _ a b c) = all M.null [a,b,c] 

add_clfield :: Ident -> MemberDecl -> ClassSum -> ClassSum
add_clfield _id _field s@ClassSum{..} = 
  let cl_fields = M.insert _id _field _cl_fields 
  in s { _cl_fields = cl_fields }

add_clmeth :: Ident -> MemberDecl -> ClassSum -> ClassSum
add_clmeth _id _meth s@ClassSum{..} = 
  let cl_meths = M.insert _id _meth _cl_meths 
  in s { _cl_meths = cl_meths }

add_clcons :: Ident -> MemberDecl -> ClassSum -> ClassSum
add_clcons _id _cons s@ClassSum{..} = 
  let cl_cons = M.insert _id _cons _cl_cons 
  in s { _cl_cons = cl_cons }

mth_body :: MemberDecl -> MethodBody
mth_body (MethodDecl _ _ _ _ _ _ b) = b
mth_body m = error $ "mth_body: fatal " ++ show m 
