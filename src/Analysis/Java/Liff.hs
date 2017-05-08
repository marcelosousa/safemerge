{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
module Analysis.Java.Liff where

-- Liff: Lightweight Diff 

import Control.Monad
import Data.List
import Data.Map (Map) 
import Language.Java.Parser 
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Analysis.Java.ClassInfo
import System.FilePath.Posix
import qualified Data.List as L 
import qualified Data.Map as M
import qualified Debug.Trace as T

trace a b = b

type MergeInst = PMergeInst MIdent

data PMergeInst a = MInst
 {
   _o_info :: ClassInfo
 , _a_info :: ClassInfo
 , _b_info :: ClassInfo
 , _m_info :: ClassInfo
 , _merges :: [a]
 }
 deriving (Ord,Eq)

instance Show a => Show (PMergeInst a) where
  show m@MInst{..} = "Merges: " ++show _merges
         
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
  liff'  :: ClassInfo -> ClassInfo -> ClassInfo -> Ident -> ClassSum -> [MIdent] -> [MIdent] 
  liff' c_a c_b c_m cls info_o r =
    case (M.lookup cls c_a, M.lookup cls c_b, M.lookup cls c_m) of
      (Just info_a, Just info_b, Just info_m) -> 
        class_diff cls info_o info_a info_b info_m r
      _ -> r

-- | class_diff : Checks the differences in the AST of two classes based on the
--   information collected in the datatype ClassSum (Class Summary)
--   For now, just checks and returns the set of methods that were modified
class_diff :: Ident -> ClassSum -> ClassSum -> ClassSum -> ClassSum -> [MIdent] -> [MIdent] 
class_diff cls cl_o cl_a cl_b cl_m r =
  M.foldWithKey (class_diff' cls (_cl_meths cl_a) (_cl_meths cl_b) (_cl_meths cl_m)) r (_cl_meths cl_o)
 where
   class_diff' :: Ident -> MemberInfo -> MemberInfo -> MemberInfo -> MemberSig -> MemberDecl -> [MIdent] -> [MIdent]
   class_diff' cls mi_a mi_b mi_m mth_sig m_o r = trace ("checking method " ++ show mth_sig) $ 
     case (M.lookup mth_sig mi_a, M.lookup mth_sig mi_b, M.lookup mth_sig mi_m) of
       (Just m_a, Just m_b, Just m_m) -> 
         -- @TODO: make sure the parameters (names) are the same
         if mbody_diff (mth_body m_o) (mth_body m_a) (mth_body m_b) (mth_body m_m)
         then trace ("changes found") $ (cls,fst mth_sig, snd mth_sig):r
         else r
       _ -> r

mbody_diff :: MethodBody -> MethodBody -> MethodBody -> MethodBody -> Bool 
mbody_diff bdy_o bdy_a bdy_b bdy_m =
 let c1 = bdy_o /= bdy_a && bdy_o /= bdy_b
     c2 = bdy_a /= bdy_b
     c3 = bdy_m /= bdy_a && bdy_m /= bdy_b
     [bo,ba,bb,bm] = [show bdy_o, show bdy_a, show bdy_b, show bdy_m] 
     c4 = isInfixOf "Return" bo 
     c5 = and $ map (\x -> not $ isInfixOf "Synchronized" x) [bo,ba,bb,bm] 
     c6 = and $ map (\x -> not $ isInfixOf "Throw"        x) [bo,ba,bb,bm] 
     c7 = and $ map (\x -> not $ isInfixOf "Exception"    x) [bo,ba,bb,bm] 
     c8 = and $ map (\x -> not $ isInfixOf "Try"          x) [bo,ba,bb,bm] 
     c9 = True -- not $ isInfixOf "Public" $ show bdy_o
     c10 = True -- and $ map (\c -> not $ isInfixOf "lock" $ show c) [bdy_o,bdy_a,bdy_b,bdy_m]
 in and [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10] 
