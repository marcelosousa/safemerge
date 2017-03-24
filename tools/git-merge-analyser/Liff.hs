module Liff where

-- Liff: Lightweight Diff 

import System.Exit
import System.Process
import Control.Exception.Base
import Control.Monad
import System.FilePath.Posix
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Data.List
import Types
import Util 
import qualified Data.Map as M
import Data.Map (Map) 
import qualified Debug.Trace as T

trace a b = b

my_catch :: IOException -> IO (ExitCode, String, String) 
my_catch _ = return (ExitFailure 1, "", "")

safe_call fn args = 
  catch (readProcessWithExitCode fn args []) my_catch 

parse :: String -> FilePath -> IO (Maybe Program)
parse hash file = do
  let arg = hash++":"++file
  (ex_code, str, _) <- safe_call "git" ["show",arg]
  if ex_code == ExitSuccess
  then do 
    let ast_ = parser compilationUnit str 
    case ast_ of
      Right ast -> return $ Just ast 
      Left err -> return Nothing -- error $ "parse error..." ++ show err
  else return Nothing -- error "parse: git show was unsucessful" 

liff :: Merge -> IO (Merge, [SQName])
liff m@(Merge args chs x) = do
  -- putStrLn $ "liff " ++ show args 
  (chs',r) <- foldM (liff_main args) ([],[]) chs
  let m' = Merge args chs' x 
  return (m',r)

dir = "results/"

type LiffRetTy = [(SQName,[MemberDecl])]

liff_main :: [String] -> ([Change], [SQName]) -> Change -> IO ([Change],[SQName])
liff_main [o,m,a,b] (chs,res) ch@(Change _ f) = do 
  o_ast <- parse o f >>= return . toClassInfo 
  a_ast <- parse a f >>= return . toClassInfo 
  b_ast <- parse b f >>= return . toClassInfo 
  m_ast <- parse m f >>= return . toClassInfo 
--  putStrLn "liff_main"
--  putStrLn "Class Info of base"
--  print o_ast
--  putStrLn "Class Info of variant a"
--  print a_ast
--  putStrLn "Class Info of variant b"
--  print b_ast
--  putStrLn "Class Info of merge"
--  print m_ast
  let r = check_diffs o_ast a_ast b_ast m_ast
  if null r
  then return (chs,res)
  else do
    let r' = map fst r
  --  mapM_ (liff_result o a b m f) r 
    putStrLn $ "Changes found in versions of file:" ++ f
    putStrLn $ "Base: " ++ o
    putStrLn $ "Variant A: " ++ a
    putStrLn $ "Variant B: " ++ b
    putStrLn $ "Merge: " ++ m
    putStrLn $ "Changes: " ++ show r'
    putStrLn "-----------------"
    return (ch:chs, r' ++ res)

liff_result :: String -> String -> String -> String -> String -> (SQName,[MemberDecl]) -> IO ()
liff_result o a b m f ((Ident cl,Ident mth),[ob,ab,bb,mb]) = do
  let res = dir ++ (take 10 m)
      n = takeFileName f
      resfile = res ++ "_" ++ cl ++ "_" ++ mth  
      stro = "Base: " ++ o 
      stra = "\nVariant A: " ++ a
      strb = "\nVariant B: " ++ b
      strm = "\nMerge: " ++ m
      strinfo = "\nClass " ++ show cl ++ ", method " ++ show mth
      resstr = stro ++ stra ++ strb ++ strm ++ strinfo ++ "\n" ++ prettyPrint ob ++ "\n" ++ prettyPrint ab ++ "\n" ++ prettyPrint bb ++ "\n" ++ prettyPrint mb 
      bench ast = "class " ++ cl ++ "{ \n " ++ "void " ++ mth ++ "() " ++ prettyPrint ast ++ "\n}"
  writeFile (resfile++"_o.java") (bench ob) 
  writeFile (resfile++"_a.java") (bench ab) 
  writeFile (resfile++"_b.java") (bench bb) 
  writeFile (resfile++"_m.java") (bench mb) 

-- | Checks the AST differences between 4 sets of classes
check_diffs :: ClassInfo -> ClassInfo -> ClassInfo -> ClassInfo -> LiffRetTy 
check_diffs c_o c_a c_b c_m = M.foldWithKey (check_diff c_a c_b c_m) [] c_o 

check_diff :: ClassInfo -> ClassInfo -> ClassInfo -> Ident -> ClassSum -> LiffRetTy -> LiffRetTy 
check_diff c_a c_b c_m cls info_o r =
  case M.lookup cls c_a of
    Nothing -> r
    Just info_a -> case M.lookup cls c_b of
      Nothing -> r
      Just info_b -> case M.lookup cls c_m of
        Nothing -> r
        Just info_m -> check_class_diff r cls info_o info_a info_b info_m

-- | check_class_diff : Checks the differences in the AST of two classes based on the
--   information collected in the datatype ClassSum (Class Summary)
--   For now, just checks and returns the set of methods that were modified
check_class_diff :: LiffRetTy -> Ident -> ClassSum -> ClassSum -> ClassSum -> ClassSum -> LiffRetTy 
check_class_diff r cls cl_o cl_a cl_b cl_m =
  let r_mth = M.foldWithKey (check_diff_meth cls (_cl_meths cl_a) (_cl_meths cl_b) (_cl_meths cl_m)) r (_cl_meths cl_o)
  in r_mth

check_diff_meth :: Ident -> MemberInfo -> MemberInfo -> MemberInfo -> Ident -> MemberDecl -> LiffRetTy -> LiffRetTy 
check_diff_meth cls mi_a mi_b mi_m mth mo r = 
  case M.lookup mth mi_a of
    Nothing -> r
    Just ma -> case M.lookup mth mi_b of
      Nothing -> r
      Just mb -> case M.lookup mth mi_m of
        Nothing -> r
        Just mm -> 
         if check_diff_mbody (mth_body mo) (mth_body ma) (mth_body mb) (mth_body mm)
         then trace ("changes found") $ ((cls,mth),[mo,ma,mb,mm]):r
         else r

check_diff_mbody :: MethodBody -> MethodBody -> MethodBody -> MethodBody -> Bool 
check_diff_mbody bdy_o bdy_a bdy_b bdy_m =
 let c1 = bdy_o /= bdy_a
     c2 = bdy_o /= bdy_b
     c3 = bdy_a /= bdy_b
     c4 = bdy_m /= bdy_a
     c5 = isInfixOf "Return" $ show bdy_o
     c6 = not $ isInfixOf "Synchronized" $ show bdy_o
     c7 = not $ isInfixOf "Synchronised" $ show bdy_o
     c8 = not $ isInfixOf "Exception" $ show bdy_o
     c9 = not $ isInfixOf "Public" $ show bdy_o
     c10 = True -- and $ map (\c -> not $ isInfixOf "lock" $ show c) [bdy_o,bdy_a,bdy_b,bdy_m]
 in and [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10] 
