{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
module Analysis.Java.Liff where

-- Liff: Lightweight Diff 

import Control.Monad
import Data.List
import Data.Map (Map) 
import Edit 
import Edit.Types
import Language.Java.Parser 
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Analysis.Java.ClassInfo
import System.FilePath.Posix
import Util
import qualified Data.List as L 
import qualified Data.Map as M
import qualified Debug.Trace as T

-- Statistics of Liff
-- Per Java File
data LiffStats = 
  LStats {
    l_class_nr     :: Int  -- number of base classes analyzed
  , l_err_class    :: Int  -- the number of classes in base and not in all 3 other versions
  , l_meth_class   :: Int  -- number of base methods analyzed
  , l_err_meth     :: Int  -- number of methods in base and not in all 3 other classes 
  , l_meth_total   :: Int  -- number of methods in the 4 versions
  , l_meth_changed :: Int  -- number of methods where the 4 versions are different
  , l_meth_edits   :: Int  -- *number of methods with successful edit script generation 
  , l_meth_complex :: Int  -- number of methods with complex code  (concurrency, exceptions, reflection, inner method definitions)
  , l_meth_simple  :: Int  -- number of methods with straight line code
  , l_meth_cond    :: Int  -- number of methods with conditionals 
  , l_meth_loop    :: Int  -- number of methods with loops
  , l_meth_cond_loop :: Int  -- number of methods with conditionals and loops
  , l_meth_return    :: Int  -- number of methods with return statements 
  , l_meth_stateless :: Int  -- number of methods without return statements where the class has no fields
  , l_edit_simple  :: Int  -- number of methods where the edit is in straight line code
  , l_edit_cond    :: Int  -- number of methods where the edit is in a conditional
  , l_edit_loop    :: Int  -- number of methods where the edit is in a loop
  , l_meth_size_dist :: (Int,Int,Int,Int) -- method size histogram (<10,>=10&<30,>=30&<50,>=50)
  , l_edit_size_dist :: (Int,Int,Int) -- number of holes histogram (<5,>=5&<10,>=10)
  } deriving Show 

i_lstats :: LiffStats
i_lstats = LStats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (0,0,0,0) (0,0,0)

inc_class_nr :: LiffStats -> LiffStats
inc_class_nr s = 
  let x = l_class_nr s
  in s { l_class_nr = x + 1 } 

inc_err_class :: LiffStats -> LiffStats
inc_err_class s = 
  let x = l_err_class s
  in s { l_err_class = x + 1} 

inc_meth_class :: LiffStats -> LiffStats
inc_meth_class s = 
  let x = l_meth_class s
  in s { l_meth_class = x + 1} 

inc_err_meth :: LiffStats -> LiffStats
inc_err_meth s = 
  let x = l_err_meth s
  in s { l_err_meth = x + 1} 

inc_meth_total :: LiffStats -> LiffStats
inc_meth_total s = 
  let x = l_meth_total s
  in s { l_meth_total = x + 1} 

inc_meth_changed :: LiffStats -> LiffStats
inc_meth_changed s = 
  let x = l_meth_changed s
  in s { l_meth_changed = x + 1} 

inc_meth_edits :: LiffStats -> LiffStats
inc_meth_edits s = 
  let x = l_meth_edits s
  in s { l_meth_edits = x + 1} 

inc_meth_complex :: LiffStats -> LiffStats
inc_meth_complex s = 
  let x = l_meth_complex s
  in s { l_meth_complex = x + 1} 

inc_meth_simple :: LiffStats -> LiffStats
inc_meth_simple s = 
  let x = l_meth_simple s
  in s { l_meth_simple = x + 1} 

inc_meth_cond :: LiffStats -> LiffStats
inc_meth_cond s = 
  let x = l_meth_cond s
  in s { l_meth_cond = x + 1} 

inc_meth_loop :: LiffStats -> LiffStats
inc_meth_loop s = 
  let x = l_meth_loop s
  in s { l_meth_loop = x + 1} 

inc_meth_cond_loop :: LiffStats -> LiffStats
inc_meth_cond_loop s = 
  let x = l_meth_cond_loop s
  in s { l_meth_cond_loop = x + 1} 

inc_meth_stateless :: LiffStats -> LiffStats
inc_meth_stateless s = 
  let x = l_meth_stateless s
  in s { l_meth_stateless = x + 1} 

inc_meth_return :: LiffStats -> LiffStats
inc_meth_return s = 
  let x = l_meth_return s
  in s { l_meth_return = x + 1} 

inc_edit_simple :: LiffStats -> LiffStats
inc_edit_simple s = 
  let x = l_edit_simple s
  in s { l_edit_simple = x + 1} 

inc_edit_cond :: LiffStats -> LiffStats
inc_edit_cond s = 
  let x = l_edit_cond s
  in s { l_edit_cond = x + 1} 

inc_edit_loop :: LiffStats -> LiffStats
inc_edit_loop s = 
  let x = l_edit_loop s
  in s { l_edit_loop = x + 1} 

inc_meth_size_dist :: Int -> LiffStats -> LiffStats
inc_meth_size_dist l s = 
  let (a,b,c,d) = l_meth_size_dist s
  in if l < 10
     then s { l_meth_size_dist = (a+1,b,c,d) } 
     else if l < 30
          then s { l_meth_size_dist = (a,b+1,c,d) } 
          else if l < 50
               then s { l_meth_size_dist = (a,b,c+1,d) } 
               else s { l_meth_size_dist = (a,b,c,d+1) } 

inc_edit_size_dist :: Int -> LiffStats -> LiffStats
inc_edit_size_dist l s = 
  let (a,b,c) = l_edit_size_dist s
  in if l < 5
     then s { l_edit_size_dist = (a+1,b,c) } 
     else if l < 10
          then s { l_edit_size_dist = (a,b+1,c) } 
          else s { l_edit_size_dist = (a,b,c+1) } 

-- | Liff: Checks the AST differences between 4 sets of classes
--         based on the methods that were present in the original version
liff :: Program -> Program -> Program -> Program -> (LiffStats,MergeInst) 
liff o_ast a_ast b_ast m_ast = 
  let o_class = toClassInfo o_ast 
      a_class = toClassInfo a_ast 
      b_class = toClassInfo b_ast 
      m_class = toClassInfo m_ast 
      (stats,merges) = M.foldWithKey (liff' a_class b_class m_class) (i_lstats,[]) o_class 
  in (stats,MInst o_class a_class b_class m_class merges) 
 where
  liff' :: ClassInfo -> ClassInfo -> ClassInfo -> Ident -> ClassSum -> (LiffStats,[MIdent]) -> (LiffStats,[MIdent]) 
  liff' c_a c_b c_m cls info_o (stats,r) =
    case (M.lookup cls c_a, M.lookup cls c_b, M.lookup cls c_m) of
      (Just info_a, Just info_b, Just info_m) -> 
        let (stats',r') = class_diff cls [info_o,info_a,info_b,info_m] (stats,r)
        in (inc_class_nr stats',r')
      _ -> (inc_err_class stats, r)

-- | class_diff : Checks the differences in the AST of four classes based on the
--   information collected in the datatype ClassSum (Class Summary)
--   For now, just checks and returns the set of methods that were modified
class_diff :: Ident -> [ClassSum] -> (LiffStats,[MIdent]) -> (LiffStats,[MIdent])
class_diff cls_name (cl_o:cls) (stats,r) =
  M.foldWithKey (class_diff' (null $ _cl_fields cl_o) (map _cl_meths cls) cls_name) (stats,r) (_cl_meths cl_o)
 where
   -- | Method level
   class_diff' :: Bool -> [MemberInfo] -> Ident -> MemberSig -> MemberDecl -> (LiffStats,[MIdent]) -> (LiffStats,[MIdent])
   class_diff' noFields [mi_a,mi_b,mi_m] cls mth_sig@(mi,mty) m_o (stats,r) =
    let stats1 = inc_meth_class stats 
    in case (M.lookup mth_sig mi_a, M.lookup mth_sig mi_b, M.lookup mth_sig mi_m) of
       (Just m_a, Just m_b, Just m_m) ->
         let stats2 = inc_meth_total stats1 
         in if m_o /= m_a && m_o /= m_b && m_a /= m_b && m_a /= m_m && m_b /= m_m -- && simple_scope e_o 
            then let stats3 = inc_meth_changed stats2
                     res    = (cls,fst mth_sig, snd mth_sig):r
                 in case diff4gen_meth (cls,mi,mty) m_o m_a m_b m_m of
                   Nothing -> (stats3,res)
                   Just (_,_,e_o,e_a,e_b,e_m) ->
                    let code_m = prettyPrint m_m
                        stats4 = inc_meth_edits stats3
                    in if is_code_complex code_m
                       then (inc_meth_complex stats4,res) 
                       else if is_code_stateless noFields code_m
                            then (inc_meth_stateless stats4,res)
                            else
                             let code_stats1 = if is_code_simple code_m then inc_meth_simple stats4 else stats4
                                 code_stats2 = if is_code_cond code_m then inc_meth_simple code_stats1 else code_stats1
                                 code_stats3 = if is_code_loop code_m then inc_meth_cond code_stats2 else code_stats2
                                 code_stats4 = if is_code_cond_loop code_m then inc_meth_cond_loop code_stats3 else code_stats3
                                 code_stats5 = if is_code_return code_m then inc_meth_return code_stats4 else code_stats4
                                 edit_stats1 = if simple_scope e_o then inc_edit_simple code_stats5 else code_stats5
                                 edit_stats2 = if if_scope e_o then inc_edit_cond edit_stats1 else edit_stats1
                                 edit_stats3 = if loop_scope e_o then inc_edit_loop edit_stats2 else edit_stats2
                                 res_stats = inc_meth_size_dist (length $ lines code_m) $ inc_edit_size_dist (length e_o) edit_stats3
                             in (res_stats,res) 
            else (stats2,r)
       _ -> (inc_err_meth stats1, r)

is_code_complex :: String -> Bool
is_code_complex str = any (\s -> isInfixOf s str) complexKeywords

complexKeywords :: [String]
complexKeywords = ["try","catch","synchronized","getMethod","getClass","goto"]

is_code_cond :: String -> Bool
is_code_cond str = any (\s -> isInfixOf s str) condKeywords

condKeywords :: [String]
condKeywords = ["if","switch"]

is_code_loop :: String -> Bool
is_code_loop str = any (\s -> isInfixOf s str) loopKeywords

loopKeywords :: [String]
loopKeywords = ["while","for"]

is_code_simple :: String -> Bool
is_code_simple str = not (is_code_cond str || is_code_loop str)

is_code_cond_loop :: String -> Bool
is_code_cond_loop str = is_code_cond str && is_code_loop str

is_code_return :: String -> Bool
is_code_return str = isInfixOf "return" str

is_code_stateless :: Bool -> String -> Bool
is_code_stateless f str = f && (not $ is_code_return str)

