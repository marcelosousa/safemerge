module Liff where

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
    mapM_ (liff_result o a b m f) r 
    putStrLn $ "Changes found in versions of file:" ++ f
    putStrLn $ "Base: " ++ o
    putStrLn $ "Variant A: " ++ a
    putStrLn $ "Variant B: " ++ b
    putStrLn $ "Merge: " ++ m
    putStrLn $ "Changes: " ++ show r'
    putStrLn "-----------------"
    return (ch:chs, r' ++ res)

liff_result :: String -> String -> String -> String -> String -> (SQName,[MethodBody]) -> IO ()
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

toClassInfo :: Maybe Program -> ClassInfo 
toClassInfo Nothing = M.empty
toClassInfo (Just (CompilationUnit pkg imp ty)) = foldr toClassInfo_ty M.empty ty

toClassInfo_ty :: TypeDecl -> ClassInfo -> ClassInfo
toClassInfo_ty ty r = case ty of
  ClassTypeDecl cls -> case cls of
    ClassDecl _ id _ _ _ (ClassBody dcls) ->
      let mth_map = foldr toMethodInfo M.empty dcls
      in if M.null mth_map
         then r
         else M.insert id mth_map r 
    _ -> r 
  _ -> r 

toMethodInfo :: Decl -> MethInfo -> MethInfo 
toMethodInfo decl r = case decl of 
  MemberDecl mem -> case mem of  
    MethodDecl _ _ _ id _ _ mbody -> M.insert id mbody r 
    _ -> r
  _ -> r 

check_diffs :: ClassInfo -> ClassInfo -> ClassInfo -> ClassInfo -> [(SQName, [MethodBody])]
check_diffs c_o c_a c_b c_m = M.foldWithKey (check_diff c_a c_b c_m) [] c_o 

check_diff :: ClassInfo -> ClassInfo -> ClassInfo -> Ident -> MethInfo -> [(SQName, [MethodBody])] -> [(SQName, [MethodBody])]
check_diff c_a c_b c_m cls mths_o r =
  case M.lookup cls c_a of
    Nothing -> r
    Just mths_a -> case M.lookup cls c_b of
      Nothing -> r
      Just mths_b -> case M.lookup cls c_m of
        Nothing -> r
        Just mths_m -> M.foldWithKey (check_diff_meth cls mths_a mths_b mths_m) r mths_o

check_diff_meth :: Ident -> MethInfo -> MethInfo -> MethInfo -> Ident -> MethodBody -> [(SQName, [MethodBody])] -> [(SQName, [MethodBody])]
check_diff_meth cls m_a m_b m_m mth bdy_o r = 
  case M.lookup mth m_a of
    Nothing -> r
    Just bdy_a -> case M.lookup mth m_b of
      Nothing -> r
      Just bdy_b -> case M.lookup mth m_m of
        Nothing -> r
        Just bdy_m -> trace ("check_diff_meth: " ++ show mth) $ 
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
          in if and [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10] 
             then trace ("changes found") $ ((cls,mth),[bdy_o,bdy_a,bdy_b,bdy_m]):r
             else r


--- OBSOLETE

liff2 :: Program -> Program -> [QName]
liff2 o_ast v_ast =
  case (o_ast, v_ast) of
    (CompilationUnit o_pkg o_imp o_ty, CompilationUnit v_pkg v_imp v_ty) ->
      liff_tys o_ty v_ty

liff_tys :: [TypeDecl] -> [TypeDecl] -> [QName] 
liff_tys xs ys =
  if length xs /= length ys
  then [] -- error "liff_tys: different lengths"
  else let xys = zip xs ys
       in foldr liff_tys_gen_aux [] xys 

liff_tys_gen_aux :: (TypeDecl,TypeDecl) -> [QName] -> [QName] 
liff_tys_gen_aux (ty_o, ty_v) r =
  if ty_o == ty_v
  then r
  else let r_ty = liff_ty_gen ty_o ty_v
       in r_ty ++ r 

liff_ty_gen :: TypeDecl -> TypeDecl -> [QName] 
liff_ty_gen ty_o ty_v = case ty_o of
  ClassTypeDecl o_class -> case ty_v of
    ClassTypeDecl v_class -> liff_class_gen o_class v_class
    InterfaceTypeDecl v_inter -> [] -- error "liff_ty_gen: misalignment or fundamental changes" 
  InterfaceTypeDecl o_inter -> [] -- error "liff_ty_gen: unsupported differences" 

liff_class_gen :: ClassDecl -> ClassDecl -> [QName] 
liff_class_gen o_class v_class =
  case (o_class, v_class) of
    (ClassDecl _ o_id _ _ _ o_body, ClassDecl _ v_id _ _ _ v_body) ->
      if o_id == v_id 
      then liff_class_body_gen o_id o_body v_body
      else [] -- error "liff_class_gen: misalignment or fundamental changes"
    -- @TODO: if necessary, introduce error here to improve coverage
    _ -> []

liff_class_body_gen :: Ident -> ClassBody -> ClassBody -> [QName] 
liff_class_body_gen ident (ClassBody o_decls) (ClassBody v_decls) =
   if length o_decls /= length v_decls
   then [] -- error "@TODO liff_class_body_gen: different number of declaration"
   else let xyz = zip o_decls v_decls
        in foldr (liff_class_body_gen_aux ident) [] xyz 

liff_class_body_gen_aux :: Ident -> (Decl, Decl) -> [QName] -> [QName]
liff_class_body_gen_aux ident (decl_o, decl_v) r =  
  if decl_o == decl_v
  then r
  else let r_decl = liff_decl_gen ident decl_o decl_v 
       in r_decl ++ r 

liff_decl_gen :: Ident -> Decl -> Decl -> [QName] 
liff_decl_gen ident o_decl v_decl =
  case (o_decl, v_decl) of
    (MemberDecl o_mem, MemberDecl v_mem) -> liff_member_gen ident o_mem v_mem
    (InitDecl o_b o_block, InitDecl v_b v_block) -> []
    _ -> [] -- error "liff_decl_gen: unsupported differences"

liff_member_gen :: Ident -> MemberDecl -> MemberDecl -> [QName] 
liff_member_gen ident o_mem v_mem = 
  case (o_mem, v_mem) of
    (MethodDecl _ _ _ o_id _ _ o_mbody, MethodDecl _ _ _ v_id _ _ v_mbody) ->
      if o_id == v_id 
      then if o_mbody == v_mbody
           then []
           else [(ident, o_id, v_mbody)] 
      else [] -- error "liff_member_gen: unsupported differences"
    _ -> [] -- error "liff_member_gen: unsupported differences"
