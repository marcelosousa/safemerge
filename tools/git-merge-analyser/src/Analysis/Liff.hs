{-# LANGUAGE DoAndIfThenElse #-}
module Analysis.Liff where


-- Liff: Lightweight Diff 

import Control.Exception.Base
import Control.Monad
import Data.List
import Data.Map (Map) 
import Language.Java.Parser 
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import System.Exit
import System.FilePath.Posix
import System.Process
import Analysis.Liff.Types
import Analysis.Liff.Util 
import qualified Data.List as L 
import qualified Data.Map as M
import qualified Debug.Trace as T

trace a b = b

my_catch :: IOException -> IO (ExitCode, String, String) 
my_catch _ = return (ExitFailure 1, "", "")

safe_call fn args = 
  catch (readProcessWithExitCode fn args []) my_catch 

-- process the result of git merge-tree
process :: [String] -> [Change] -> [Change]
process [] changes = changes
process ls changes =
  let (ch,ls') = process_changes ls
  in process ls' (ch:changes)

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

get_merge :: [String] -> IO Merge
get_merge as@[o,m,a,b] = do 
  let args = [o,a,b]
  (ex_code, str, _) <- safe_call "git" ("merge-tree":args)
  if ex_code == ExitSuccess
  then do 
    let str_lines = lines str
        changes = filter is_interesting $ process str_lines []
    return $ Merge as changes True
  else do
    return $ Merge as [] False

is_of_interest :: Merge -> Bool
is_of_interest (Merge _ ch va) = va && (not $ null ch)

-- ^ process_merge: retrieves the hash of the base based on the variants
process_merge :: [[String]] -> [String] -> IO [[String]]
process_merge res args = do
  let vars = tail args
  (ex_code, str, _) <- safe_call "git" ("merge-base":vars)
  if ex_code == ExitSuccess
  then do
    let o = lines str
    return ((o++args):res)
  else return res  

git_opts :: [String]
git_opts = ["log", "--merges", "--abbrev-commit", "--decorate", "--format=format:%H %P", "--all"]

main :: IO ()
main = do
  merges <- readProcess "git" git_opts []
  let merge_lines = lines merges
      args = map words merge_lines
  versions <- foldM process_merge [] args 
  diffs <- mapM get_merge versions 
  let both_changed_files = filter is_of_interest diffs
  -- putStrLn $ print_merges both_changed_files
  meths' <- mapM liff both_changed_files
  -- let meths = filter (\(_,x) -> not $ null x) meths' 
  -- putStrLn $ print_merges meths 
  putStrLn "Done"

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
