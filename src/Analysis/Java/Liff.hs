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

-- | Liff: Checks the AST differences between 4 sets of classes
--         based on the methods that were present in the original version
liff :: Program -> Program -> Program -> Program -> MergeInst 
liff o_ast a_ast b_ast m_ast = 
  let o_class = toClassInfo o_ast 
      a_class = toClassInfo a_ast 
      b_class = toClassInfo b_ast 
      m_class = toClassInfo m_ast 
      merges = M.foldWithKey (liff' a_class b_class m_class) [] o_class 
  in MInst o_class a_class b_class m_class merges 
 where
  liff' :: ClassInfo -> ClassInfo -> ClassInfo -> Ident -> ClassSum -> [MIdent] -> [MIdent] 
  liff' c_a c_b c_m cls info_o r =
    case (M.lookup cls c_a, M.lookup cls c_b, M.lookup cls c_m) of
      (Just info_a, Just info_b, Just info_m) -> 
        class_diff cls [info_o,info_a,info_b,info_m] r
      _ -> r

-- | class_diff : Checks the differences in the AST of four classes based on the
--   information collected in the datatype ClassSum (Class Summary)
--   For now, just checks and returns the set of methods that were modified
class_diff :: Ident -> [ClassSum] -> [MIdent] -> [MIdent] 
class_diff cls_name (cl_o:cls) r =
  M.foldWithKey (class_diff' (map _cl_meths cls) cls_name) r (_cl_meths cl_o)
 where
   class_diff' :: [MemberInfo] -> Ident -> MemberSig -> MemberDecl -> [MIdent] -> [MIdent]
   class_diff' [mi_a,mi_b,mi_m] cls mth_sig@(mi,mty) m_o r = 
     case (M.lookup mth_sig mi_a, M.lookup mth_sig mi_b, M.lookup mth_sig mi_m) of
       (Just m_a, Just m_b, Just m_m) -> 
         let (_,_,e_o,e_a,e_b,e_m) = diff4gen_meth (cls,mi,mty) m_o m_a m_b m_m 
         in if e_o /= e_a -- && e_o /= e_b && e_a /= e_b && e_a /= e_m && e_b /= e_m -- && loop_scope e_o -- 
            then (cls,fst mth_sig, snd mth_sig):r
            else r
       _ -> r

