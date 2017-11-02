{-# LANGUAGE RecordWildCards #-}
module GitMerge.Analysis.Types where

import Analysis.Java.Liff
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

data Stats = Stats
 {
 -- Information in the git log
   log_merge     :: Int -- Number of merges
 , log_trivial   :: Int -- Merges with base = variant
 , log_error     :: Int -- Merges where git merge-tree fails
 -- Valid merges = log_merge - (log_trivial + log_error)
 -- Information in the git merge-tree
 , java_merges   :: Int -- Merges where at least one Java file contains changes from both variants 
 , other_merges  :: Int -- Merges not related to java programs or where only one of the variants changes  
 , total_files   :: Int -- Number of files involved in all valid merges 
 , log_file_java :: Int -- Number of Java files
 , log_file_both :: Int -- Changes in the same Java file by both variants
 , git_show_err  :: Int -- Number of git show errors in retrieving the versions of the file 
 -- Information per interesting Java file
 , class_nr     :: Int  -- number of base classes analyzed
 , err_class    :: Int  -- the number of classes in base and not in all 3 other versions
 , meth_class   :: Int  -- number of base methods analyzed
 , err_meth     :: Int  -- number of methods in base and not in all 3 other classes 
 , meth_total   :: Int  -- number of methods in the 4 versions
 , meth_changed :: Int  -- number of methods changed by at least one variant 
 , meth_context :: MContexts -- method contexts
 , meth_edits   :: Int  -- *number of methods with successful edit script generation 
 , meth_complex :: Int  -- number of methods with complex code  (concurrency, exceptions, reflection, inner method definitions)
 , meth_simple  :: Int  -- number of methods with straight line code
 , meth_cond    :: Int  -- number of methods with conditionals 
 , meth_loop    :: Int  -- number of methods with loops
 , meth_stateless :: Int  -- number of methods without return statements where the class has no fields
 , edit_simple  :: Int  -- number of methods where the edit is in straight line code
 , edit_cond    :: Int  -- number of methods where the edit is in a conditional
 , edit_loop    :: Int  -- number of methods where the edit is in a loop
 , meth_size_dist :: (Int,Int,Int,Int) -- method size histogram (<10,>=10&<30,>=30&<50,>=50)
 , edit_size_dist :: (Int,Int,Int) -- number of holes histogram (<5,>=5&<10,>=10)
 } deriving (Eq,Ord)

instance Show Stats where
 show (s@Stats{..}) = 
  let a = "Git Merge Statistics"
      a_ = "Graph Statistics"
      b = "|Merges|         =\t " ++ show log_merge 
      c = "|Variant=Base|   =\t " ++ show log_trivial 
      d = "|Merge-Tree Err| =\t " ++ show log_error
      e = "|Valid Merge|    =\t " ++ show (log_merge - (log_trivial + log_error)) 
      e_ = "Merge Commit Statistics"
      i = "|Java Merge|     =\t " ++ show java_merges 
      j = "|Non-Java Merge| =\t " ++ show other_merges 
      f = "|Total Files|    =\t " ++ show total_files 
      g = "|Java Files|     =\t " ++ show log_file_java 
      h = "|Java Changed 2| =\t " ++ show log_file_both 
      k = "|Git Show Error| =\t " ++ show git_show_err

      k_ = "Code Statistics"
      l = "|Java Class|     =\t " ++ show class_nr 
      m = "|Java Class Err| =\t " ++ show err_class 
      ad = "|Method Analyzed| =\t " ++ show meth_class
      ae = "|Method Error|    =\t " ++ show err_meth
      n = "|Method Valid|   =\t " ++ show meth_total
      o = "|Method Changed| =\t " ++ show meth_changed 
      x = show meth_context
      p = "|Method Edit|    =\t " ++ show meth_edits 
      q = "|Method Unsup|   =\t " ++ show meth_complex
      r = "|Method Stateless|=\t " ++ show meth_stateless 
      s = "|Method Simple|  =\t " ++ show meth_simple
      t = "|Method Ifs|     =\t " ++ show meth_cond
      u = "|Method Loops|   =\t " ++ show meth_loop
      y = "|Edit Simple|    =\t " ++ show edit_simple 
      z = "|Edit Ifs|       =\t " ++ show edit_cond
      aa = "|Edit Loop|      =\t " ++ show edit_loop
      ab = "Method Sizes     =\t " ++ show meth_size_dist 
      ac = "Edit Sizes       =\t " ++ show edit_size_dist 
  in unlines [a,b,c,d,e,i,j,f,g,h,k,l,m,ad,ae,n,o,x,p,q,r,s,t,u,y,z,aa,ab,ac] 
--  in unlines [a,a_,b,c,d,e,e_,i,j,f,g,h,k,k_,l,m,ad,ae,n,o,p,q,r,s,t,u,v,x,y,z,aa,ab,ac] 

init_stats :: Int -> Stats
init_stats n = Stats n 0 0 0 0 0 0 0 0 0 0 0 0 0 0 i_context 0 0 0 0 0 0 0 0 0 (0,0,0,0) (0,0,0) 

inc_trivial :: Stats -> Stats
inc_trivial s =
  let n = log_trivial s
  in s { log_trivial = n + 1 }

inc_error :: Stats -> Stats
inc_error s =
  let n = log_error s
  in s { log_error = n + 1 }

-- Valid Merges Information  
inc_java_merges :: Stats -> Stats
inc_java_merges s =
  let n = java_merges s
  in s { java_merges = n + 1 }

inc_other_merges :: Stats -> Stats
inc_other_merges s =
  let n = other_merges s
  in s { other_merges = n + 1 }

inc_total_files :: Stats -> Stats
inc_total_files s =
  let n = total_files s
  in s { total_files = n + 1 }

inc_file_java :: Stats -> Stats
inc_file_java s =
  let n = log_file_java s
  in s { log_file_java = n + 1 }

inc_file_both :: Stats -> Stats
inc_file_both s =
  let n = log_file_both s
  in s { log_file_both = n + 1 }

inc_liff_stats :: LiffStats -> Stats -> Stats
inc_liff_stats l s =
  let _class_nr       = class_nr       s + l_class_nr       l 
      _err_class      = err_class      s + l_err_class      l 
      _meth_class     = meth_class     s + l_meth_class     l 
      _err_meth       = err_meth       s + l_err_meth       l 
      _meth_total     = meth_total     s + l_meth_total     l 
      _meth_changed   = meth_changed   s + l_meth_changed   l 
      _meth_edits     = meth_edits     s + l_meth_edits     l 
      _meth_complex   = meth_complex   s + l_meth_complex   l 
      _meth_simple    = meth_simple    s + l_meth_simple    l 
      _meth_cond      = meth_cond      s + l_meth_cond      l 
      _meth_loop      = meth_loop      s + l_meth_loop      l 
      _meth_stateless = meth_stateless s + l_meth_stateless l
      _edit_simple    = edit_simple    s + l_edit_simple    l
      _edit_cond      = edit_cond      s + l_edit_cond      l
      _edit_loop      = edit_loop      s + l_edit_loop      l
      _meth_context   = join_context (meth_context s) (l_m_context l) 
      _meth_size_dist = 
         let (a,b,c,d) = meth_size_dist s 
             (a',b',c',d') = l_meth_size_dist l
         in (a+a',b+b',c+c',d+d')
      _edit_size_dist = 
         let (a,b,c) = edit_size_dist s 
             (a',b',c') = l_edit_size_dist l
         in (a+a',b+b',c+c')
  in s { class_nr        =  _class_nr        
       , err_class       =  _err_class       
       , meth_class      =  _meth_class      
       , err_meth        =  _err_meth        
       , meth_total      =  _meth_total      
       , meth_changed    =  _meth_changed    
       , meth_context    =  _meth_context
       , meth_edits      =  _meth_edits      
       , meth_complex    =  _meth_complex    
       , meth_simple     =  _meth_simple     
       , meth_cond       =  _meth_cond       
       , meth_loop       =  _meth_loop       
       , meth_stateless  =  _meth_stateless   
       , edit_simple     =  _edit_simple       
       , edit_cond       =  _edit_cond        
       , edit_loop       =  _edit_loop        
       , meth_size_dist  =  _meth_size_dist   
       , edit_size_dist  =  _edit_size_dist } 


inc_show_errs :: Stats -> Stats
inc_show_errs s = 
  let n = git_show_err s
  in s { git_show_err = n + 1 }
