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
-- Per Method
-- Nine interesting merge scenarios expressed as equivalence classes
--  assuming at least one of the variants changes the base. 
--  This excludes the cases {O,V} {V,M} which means only one of them changed the base
--  and the merge candidate is that variant.
--  1. {O,A,M} {B}     --- Variant B is discarded 
--  2. {O,B,M} {A}     --- Variant A is discarded
--  3. {O,M} {A} {B}   --- The variants are different than the base but the merge is the base (Different concurrent changes discarded) 
                       --- O == M && A != B && A != O && A !== O --- 
--  4. {O,M} {A,B}     --- The variants are the same and the merge is the base different than the variants (Same concurrent changes discarded)
--  5. {O} {A,B,M}     --- Same concurrent changes incorporated in the merge 
--  6. {O} {A,B} {M}   --- Same concurrent changes different than the merge
--  7. {O} {A,M} {B}   --- Merge is variant A and variant B is different than all versions 
--  8. {O} {A} {M,B}   --- Merge is variant B and variant A is different than all versions
--  9. {O} {A} {B} {M} --- All versions are different 
--  These equivalence classes are modelled with Int
type MContext = Int

show_context :: MContext -> String
show_context n = 
  case n of 
    1 -> "1. {O,A,M} {B}     --- Variant B is discarded" 
    2 -> "2. {O,B,M} {A}     --- Variant A is discarded" 
    3 -> "3. {O,M} {A} {B}   --- Different concurrent changes discarded" 
    4 -> "4. {O,M} {A,B}     --- Same concurrent changes discarded" 
    5 -> "5. {O} {A,B,M}     --- Same concurrent changes incorporated in the merge" 
    6 -> "6. {O} {A,B} {M}   --- Same concurrent changes not incorporated in the merge" 
    7 -> "7. {O} {A,M} {B}   --- Merge is variant A and variant B is different than all versions" 
    8 -> "8. {O} {A} {M,B}   --- Merge is variant B and variant A is different than all versions" 
    9 -> "9. {O} {A} {B} {M} --- All versions are different" 
    _ -> error $ "Context: " ++ show n ++ " does not match any equivalence class"

show_short_context :: MContext -> String
show_short_context n = 
  case n of 
    1 -> "1. |{O,A,M}     {B}| = " 
    2 -> "2. |{O,B,M}     {A}| = " 
    3 -> "3. |{O,M}  {A}  {B}| = " 
    4 -> "4. |{O,M}     {A,B}| = " 
    5 -> "5. |{O}     {A,B,M}| = " 
    6 -> "6. |{O}  {A,B}  {M}| = " 
    7 -> "7. |{O}  {A,M}  {B}| = " 
    8 -> "8. |{O}   {A} {M,B}| = " 
    9 -> "9. |{O} {A} {B} {M}| = " 

data MContexts = 
 MCont {
   b_discard_nr    :: Int
 , a_discard_nr    :: Int
 , aNb_discard_nr  :: Int
 , aEb_discard_nr  :: Int
 , aEb_inmerge_nr  :: Int
 , aEb_Ninmerge_nr :: Int
 , aEm_bdiff_nr    :: Int
 , bEm_adiff_nr    :: Int
 , all_diff_nr     :: Int
 } deriving (Eq,Ord) 

instance Show MContexts where
  show c@MCont{..} =
    let l = [ "Merge Scenario Distribution"
            , (show_short_context 1 ++ show b_discard_nr    )
            , (show_short_context 2 ++ show a_discard_nr    )
            , (show_short_context 3 ++ show aNb_discard_nr  )
            , (show_short_context 4 ++ show aEb_discard_nr  )
            , (show_short_context 5 ++ show aEb_inmerge_nr  )
            , (show_short_context 6 ++ show aEb_Ninmerge_nr )
            , (show_short_context 7 ++ show aEm_bdiff_nr    )
            , (show_short_context 8 ++ show bEm_adiff_nr    )
            , (show_short_context 9 ++ show all_diff_nr     )]
    in unlines l

get_context :: MContext -> (MContexts -> Int)
get_context n = 
  case n of 
    1 -> b_discard_nr     
    2 -> a_discard_nr     
    3 -> aNb_discard_nr   
    4 -> aEb_discard_nr   
    5 -> aEb_inmerge_nr   
    6 -> aEb_Ninmerge_nr  
    7 -> aEm_bdiff_nr     
    8 -> bEm_adiff_nr     
    9 -> all_diff_nr      
    _ -> error $ "Context: " ++ show n ++ " does not match any equivalence class"

inc_context :: MContext -> MContexts -> MContexts
inc_context n c = 
  let k = get_context n
      v = k c + 1 
  in case n of 
       1 -> c { b_discard_nr    = v }  
       2 -> c { a_discard_nr    = v }  
       3 -> c { aNb_discard_nr  = v }  
       4 -> c { aEb_discard_nr  = v }  
       5 -> c { aEb_inmerge_nr  = v }  
       6 -> c { aEb_Ninmerge_nr = v }  
       7 -> c { aEm_bdiff_nr    = v }  
       8 -> c { bEm_adiff_nr    = v }  
       9 -> c { all_diff_nr     = v }  

join_context :: MContexts -> MContexts -> MContexts
join_context c1 c2 = 
  let a = b_discard_nr    c1 + b_discard_nr    c2  
      b = a_discard_nr    c1 + a_discard_nr    c2  
      c = aNb_discard_nr  c1 + aNb_discard_nr  c2  
      d = aEb_discard_nr  c1 + aEb_discard_nr  c2  
      e = aEb_inmerge_nr  c1 + aEb_inmerge_nr  c2  
      f = aEb_Ninmerge_nr c1 + aEb_Ninmerge_nr c2  
      g = aEm_bdiff_nr    c1 + aEm_bdiff_nr    c2  
      h = bEm_adiff_nr    c1 + bEm_adiff_nr    c2  
      i = all_diff_nr     c1 + all_diff_nr     c2
  in MCont a b c d e f g h i

i_context :: MContexts
i_context = MCont 0 0 0 0 0 0 0 0 0

type Error = String
data MethCategory = LComplex | LStateless | LSimple | LCond | LLoop | LNone
  deriving (Show,Eq,Ord)

is_rest :: MethCategory -> Bool
is_rest n 
 | n == LComplex = False
 | n == LStateless = False
 | otherwise = True 
 
data EditCategory = ESimple | ECond | ELoop | ENone  
  deriving (Show,Eq,Ord)

data MSummary = MSum { 
   _m_ctx  :: MContext
 , _m_diff :: Maybe Error  -- Nothing means the diff4 was successful 
 , _m_meth :: MethCategory -- Type of method
 , _m_edit :: EditCategory -- Type of edit 
 } deriving (Show,Eq,Ord)

i_sum :: MContext -> MSummary
i_sum ctx = MSum ctx (Just "error") LNone ENone

-- Per Java File
data LiffStats = 
  LStats {
    l_class_nr       :: Int  -- number of base classes analyzed
  , l_err_class      :: Int  -- the number of classes in base and not in all 3 other versions
  , l_meth_class     :: Int  -- number of base methods analyzed
  , l_err_meth       :: Int  -- number of methods in base and not in all 3 other classes 
  , l_meth_total     :: Int  -- number of methods in the 4 versions
  , l_meth_changed   :: Int  -- number of potential instances 
  , l_m_context      :: MContexts -- merge context  
  , l_meth_edits     :: Int  -- *number of methods with successful edit script generation 
  , l_meth_complex   :: Int  -- number of methods with complex code  (concurrency, exceptions, reflection, inner method definitions)
  , l_meth_stateless :: Int  -- number of methods without return statements where the class has no fields
  , l_meth_simple    :: Int  -- number of methods with straight line code
  , l_meth_cond      :: Int  -- number of methods with conditionals 
  , l_meth_loop      :: Int  -- number of methods with loops
  , l_edit_simple    :: Int  -- number of methods where the edit is in straight line code
  , l_edit_cond      :: Int  -- number of methods where the edit is in a conditional
  , l_edit_loop      :: Int  -- number of methods where the edit is in a loop
  , l_meth_size_dist :: (Int,Int,Int,Int) -- method size histogram (<10,>=10&<30,>=30&<50,>=50)
  , l_edit_size_dist :: (Int,Int,Int) -- number of holes histogram (<5,>=5&<10,>=10)
  , l_map :: Map MIdent MSummary 
  } deriving Show 

i_lstats :: LiffStats
i_lstats = LStats 0 0 0 0 0 0 i_context 0 0 0 0 0 0 0 0 0 (0,0,0,0) (0,0,0) M.empty

-- | Increment the number of classes analysed
inc_class_nr :: LiffStats -> LiffStats
inc_class_nr s = 
  let x = l_class_nr s
  in s { l_class_nr = x + 1 } 

-- | Increment the number of error in analysing classes 
inc_err_class :: LiffStats -> LiffStats
inc_err_class s = 
  let x = l_err_class s
  in s { l_err_class = x + 1} 

-- | Increment the number of base methods analysed
inc_meth_class :: LiffStats -> LiffStats
inc_meth_class s = 
  let x = l_meth_class s
  in s { l_meth_class = x + 1} 

-- | Increment the number of error in analysing base methods 
inc_err_meth :: LiffStats -> LiffStats
inc_err_meth s = 
  let x = l_err_meth s
  in s { l_err_meth = x + 1} 

-- | Increment the total number of methods present in the 4 variants
inc_meth_total :: LiffStats -> LiffStats
inc_meth_total s = 
  let x = l_meth_total s
  in s { l_meth_total = x + 1} 

-- | Increment the number of methods modified by at least one of the variants
--   This our base number for the possible merge benchmarks
inc_meth_changed :: LiffStats -> LiffStats
inc_meth_changed s = 
  let x = l_meth_changed s
  in s { l_meth_changed = x + 1} 

-- | Increment the number of method merges which we can compute the edit scripts
inc_meth_edits :: LiffStats -> LiffStats
inc_meth_edits s = 
  let x = l_meth_edits s
  in s { l_meth_edits = x + 1} 

-- | Update statistics based on the method category
up_meth_cat :: MethCategory -> LiffStats -> LiffStats
up_meth_cat m st = 
  case m of 
   LComplex   -> inc_meth_complex   st
   LStateless -> inc_meth_stateless st
   LSimple    -> inc_meth_simple    st   
   LCond      -> inc_meth_cond      st
   LLoop      -> inc_meth_loop      st
   LNone      -> error "up_meth_cat: LNone is not expected"
 where
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
  
  inc_meth_stateless :: LiffStats -> LiffStats
  inc_meth_stateless s = 
    let x = l_meth_stateless s
    in s { l_meth_stateless = x + 1} 

-- | Update statistics based on the edit category
up_edit_cat :: EditCategory -> LiffStats -> LiffStats
up_edit_cat m st = 
  case m of 
   ESimple    -> inc_edit_simple    st   
   ECond      -> inc_edit_cond      st
   ELoop      -> inc_edit_loop      st
   ENone      -> error "up_meth_cat: LNone is not expected"
 where 
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

inc_m_context :: MContext -> LiffStats -> LiffStats
inc_m_context k s =
  let c = l_m_context s
  in s { l_m_context = inc_context k c }
 
add_to_map :: MIdent -> MSummary -> LiffStats -> LiffStats
add_to_map key val st = 
 let m = l_map st
 in case M.lookup key m of
      Nothing -> 
       let m' = M.insert key val m
       in st { l_map = m' } 
      Just v  -> error $ "add_to_map: duplicated key: " ++ show key 

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
class_diff :: Ident -> [ClassSum] -> (LiffStats,[MIdent]) -> (LiffStats,[MIdent])
class_diff cls_name cl@(cl_o:cls) (stats,r) =
  M.foldWithKey (class_diff' (M.unions $ map _cl_fields cl) (map _cl_meths cls) cls_name) (stats,r) (_cl_meths cl_o)
 where
   -- | Method level
   class_diff' :: FieldInfo -> [MemberInfo] -> Ident -> MemberSig -> MemberDecl -> (LiffStats,[MIdent]) -> (LiffStats,[MIdent])
   class_diff' fields [mi_a,mi_b,mi_m] cls mth_sig@(mi,mty) m_o (stats,r) =
    let stats1 = inc_meth_class stats 
    in case (M.lookup mth_sig mi_a, M.lookup mth_sig mi_b, M.lookup mth_sig mi_m) of
       (Just m_a, Just m_b, Just m_m) ->
         let stats2 = inc_meth_total stats1 
             ctx    = get_merge_context m_o m_a m_b m_m
             MethodBody m_bdy = mth_body m_m
             code_m = show m_bdy 
             mcat   = get_meth_category (M.null fields) code_m 
             isStat = mcat == LStateless && (not $ modFields fields (findLhs m_m))
         in if ctx == 9 -- && mcat /= LComplex && (mcat /= LStateless || modFields fields (findLhs m_m)) 
            -- | This is a potential interesting merge instance
            then let stats3 = inc_m_context ctx $ inc_meth_changed stats2
                     mident = (cls,mi,mty)
                     res    = mident:r
                 in (get_stats fields mident ctx m_o m_a m_b m_m stats3, res)
            else (stats2,r)
       _ -> (inc_err_meth stats1, r)

modFields :: FieldInfo -> [Lhs] -> Bool
modFields fields lhs = any (\l -> isField (lhsToIdent l) fields) lhs 

lhsToIdent :: Lhs -> Ident
lhsToIdent lhs = case lhs of 
  NameLhs (Name i) -> head i
  FieldLhs fa -> case fa of 
    PrimaryFieldAccess _ i -> i 
    SuperFieldAccess i -> i 
    ClassFieldAccess _ i -> i
  ArrayLhs _ -> Ident "" 

isField :: Ident -> FieldInfo -> Bool 
isField i m = case M.lookup i m of
  Nothing -> False
  Just _  -> True

-- | Pre-condition;
--   The variants are different than the base
--   Stats already updated the total count of methods 
get_stats :: FieldInfo -> MIdent -> MContext -> MemberDecl -> MemberDecl -> MemberDecl -> MemberDecl -> LiffStats -> LiffStats
get_stats fields ident ctx o a b m st = 
  -- Check diff result 
  case diff4gen_meth ident o a b m of
    Nothing -> 
      let sum = i_sum ctx 
      in add_to_map ident sum st
    Just (_,_,e_o,e_a,e_b,e_m) ->
      let -- code_m = show m -- prettyPrint m
          MethodBody m_bdy = mth_body m
          code_m = show m_bdy 
          st1    = inc_meth_edits st
          mcat   = get_meth_category (M.null fields) code_m 
          isStat = if mcat == LStateless && (not $ modFields fields (findLhs m)) then LStateless else mcat
          ecat   = get_edit_category e_m
          sum    = MSum ctx Nothing isStat ecat
          st'    = inc_meth_size_dist (length $ lines code_m) $ inc_edit_size_dist (length e_m) st 
      in add_to_map ident sum $ inc_meth_edits $ up_meth_cat mcat $ up_edit_cat ecat st'

-- | Compute the equivalence classes between the programs based on syntactic equality
--  This excludes the cases {O,A} {B,M} which means only one of them changed the base
--  and the merge candidate is that variant.
--   o != a || o != b
get_merge_context :: MemberDecl -> MemberDecl -> MemberDecl -> MemberDecl -> MContext 
get_merge_context o a b m 
  --  9. {O} {A} {B} {M} 
  | o /= a && o /= b && o /= m && a /= b && a /= m && b /= m = 9
  --  1. {O,A,M} {B}    
  | o == a && a == m && a /= b = 1
  --  2. {O,B,M} {A}   
  | o == b && b == m && a /= b = 2   
  --  3. {O,M} {A} {B}   
  | o == m && a /= o && b /= o && a /= b = 3   
  --  4. {O,M} {A,B}     
  | o == m && o /= a && a == b = 4
  --  5. {O} {A,B,M}     
  | o /= a && a == b && b == m = 5
  --  6. {O} {A,B} {M}   
  | o /= a && a == b && b /= m && o /= m = 6
  --  7. {O} {A,M} {B}   
  | o /= a && a == m && b /= m && o /= m = 7
  --  8. {O} {A} {M,B}   
  | o /= a && o /= b && a /= b && b == m = 8
  | otherwise = 0
 
get_meth_category :: Bool -> String -> MethCategory 
get_meth_category fields code = 
  if is_code_stateless fields code 
  then LStateless
  else if is_code_complex code
       then LComplex
       else if is_code_loop code
            then LLoop
            else if is_code_cond code 
                 then LCond
                 else LSimple

is_code_complex :: String -> Bool
is_code_complex str = any (\s -> isInfixOf s str) complexKeywords

complexKeywords :: [String]
complexKeywords = ["Throw ","Try ","Catch ","Synchronized ","MethodDecl ","MethodBody "]
-- complexKeywords = ["Throw","Try","Catch","Synchronized","getMethod","getClass"]

is_code_cond :: String -> Bool
is_code_cond str = any (\s -> isInfixOf s str) condKeywords

condKeywords :: [String]
condKeywords = ["IfThen ","IfThenElse ","Switch "]

is_code_loop :: String -> Bool
is_code_loop str = any (\s -> isInfixOf s str) loopKeywords

loopKeywords :: [String]
loopKeywords = ["While ","Do ","BasicFor ","EnhancedFor "]

is_code_simple :: String -> Bool
is_code_simple str = not (is_code_cond str || is_code_loop str)

is_code_return :: String -> Bool
is_code_return str = isInfixOf "Return " str

is_code_stateless :: Bool -> String -> Bool
is_code_stateless f str = not $ is_code_return str

get_edit_category :: Edit -> EditCategory
get_edit_category e
  | simple_scope e = ESimple
  | if_scope     e = ECond
  | loop_scope   e = ELoop 
  | otherwise      = ENone 
