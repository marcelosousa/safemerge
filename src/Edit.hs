{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T

type Edit = [BlockStmt] 

test_edit_gen :: FilePath -> IO CompilationUnit
test_edit_gen orig = do
  orig_ast <- parser compilationUnit `fmap` readFile orig
  case orig_ast of
    Right o_ast -> return o_ast 
    Left err -> error $ "parse error..." ++ show err

main_edit_gen :: FilePath -> FilePath -> IO () --CompilationUnit, Edit, Edit)
main_edit_gen orig var = do
  orig_ast <- parser compilationUnit `fmap` readFile orig
  var_ast  <- parser compilationUnit `fmap` readFile var 
  case (orig_ast, var_ast) of
    (Right o_ast, Right v_ast) -> do
      let (no_ast, o_edit, v_edit) = edit_gen o_ast v_ast
      putStrLn $ prettyPrint no_ast
      print o_edit
      print v_edit  
    _ -> error "parse error..."

type Program = CompilationUnit
gen_edit :: Program -> Program -> [Edit] -> (Program, [Edit])
gen_edit p1 p2 eis =
  let (p,e2,e1) = edit_gen p1 p2
      -- (p',e2,e1') = edit_gen p2 p1
      -- assertions: p == p', e1 == e1', e2 == e2', |e1| == |e2'|
      e = zip e1 e2
      (_,eis') = foldl (gen_edit_aux eis) (0,[]) e
  in (p,eis' ++ [e1])
 where
  gen_edit_aux :: [Edit] -> (Int,[Edit]) -> (BlockStmt,BlockStmt) -> (Int,[Edit])
  gen_edit_aux eis (i,eis') (e1,e2)
   | e1 == skip || e2 == hole = (i+1,push (map (\e -> e!!i) eis) eis') 
   | otherwise = (i, push (replicate (length eis) e2) eis')

push :: [BlockStmt] -> [Edit] -> [Edit]
push xs [] = map (:[]) xs
push xs eis =
  let a = zip xs eis
  in map (\(x,ei) -> x:ei) a

kast :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Program,Program,Program,Program)
kast ofl afl bfl mfl = do
  _o <- parser compilationUnit `fmap` readFile ofl 
  _a <- parser compilationUnit `fmap` readFile afl 
  _b <- parser compilationUnit `fmap` readFile bfl 
  _m <- parser compilationUnit `fmap` readFile mfl 
  case (_o,_a,_b,_m) of
    (Right o, Right a, Right b, Right m) -> return (o,a,b,m)

kedits :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
kedits ofl afl bfl mfl = do
  (o,a,b,m) <- kast ofl afl bfl mfl
  let (no,ea) = gen_edit o a []
      (nno,eab) = gen_edit no b ea
      (nnno,eabm) = gen_edit nno m eab
 --     (fo,eabmo) = gen_edit nnno o eabm
  putStrLn $ prettyPrint nnno 
  mapM_ print eabm
  

edit_gen :: CompilationUnit -> CompilationUnit -> (CompilationUnit, Edit, Edit)
edit_gen o_ast v_ast =
  case (o_ast, v_ast) of
    (CompilationUnit o_pkg o_imp o_ty, CompilationUnit v_pkg v_imp v_ty) ->
      let (n_tys, o_edit, v_edit) = edit_tys_gen o_ty v_ty
          no_ast = CompilationUnit o_pkg o_imp n_tys
      in (no_ast, o_edit, v_edit) 

edit_tys_gen :: [TypeDecl] -> [TypeDecl] -> ([TypeDecl], Edit, Edit)
edit_tys_gen xs ys =
  if length xs /= length ys
  then error "there are fundamental differences between the files"
  else let xys = zip xs ys
       in foldr edit_tys_gen_aux ([],[],[]) xys 

edit_tys_gen_aux :: (TypeDecl,TypeDecl) -> ([TypeDecl],Edit,Edit) -> ([TypeDecl],Edit,Edit)
edit_tys_gen_aux (ty_o,ty_v) (tys,o_edit,v_edit) =
  let (nty,o_e,v_e) = edit_ty_gen ty_o ty_v
  in (nty:tys, o_e++o_edit, v_e++v_edit)

edit_ty_gen :: TypeDecl -> TypeDecl -> (TypeDecl,Edit,Edit)
edit_ty_gen ty_o ty_v =
  case ty_o of
    ClassTypeDecl o_class -> case ty_v of
      ClassTypeDecl v_class ->
        let (no_class,o_edit,v_edit) = edit_class_gen o_class v_class
        in (ClassTypeDecl no_class,o_edit,v_edit)
      InterfaceTypeDecl v_inter -> error "unsupported differences" 
    InterfaceTypeDecl o_inter -> if ty_o == ty_v then (ty_o,[],[]) else error "unsupported differences" 

edit_class_gen :: ClassDecl -> ClassDecl -> (ClassDecl,Edit,Edit)
edit_class_gen o_class v_class =
  case (o_class,v_class) of
    (ClassDecl o_mods o_id o_tys o_mref o_reftys o_body, ClassDecl v_mods v_id v_tys v_mref v_reftys v_body) ->
      let checks = [ o_mods == v_mods
                   , o_id == v_id
                   , o_tys == v_tys
                   , o_mref == v_mref
                   , o_reftys == v_reftys ]
      in if all id checks
         then let (no_body,o_edit,v_edit) = edit_class_body_gen o_body v_body
              in (ClassDecl o_mods o_id o_tys o_mref o_reftys no_body, o_edit, v_edit)
         else error "unsupported differences"
    (EnumDecl _ _ _ _, _) -> if o_class == v_class then (o_class,[],[]) else error "unsupported differences" 

edit_class_body_gen :: ClassBody -> ClassBody -> (ClassBody,Edit,Edit)
edit_class_body_gen (ClassBody o_decls) (ClassBody v_decls) =
   if length o_decls /= length v_decls
   then error "unsupported differences"
   else let xyz = zip o_decls v_decls
            (no_decls,o_edit,v_edit) = foldr edit_class_body_gen_aux ([],[],[]) xyz 
        in (ClassBody no_decls, o_edit, v_edit)

edit_class_body_gen_aux :: (Decl,Decl) -> ([Decl],Edit,Edit) -> ([Decl],Edit,Edit)
edit_class_body_gen_aux (decl_o,decl_v) (decls,o_edit,v_edit) =
  let (ndecl,o_e,v_e) = edit_decl_gen decl_o decl_v 
  in (ndecl:decls, o_e++o_edit, v_e++v_edit)

edit_decl_gen :: Decl -> Decl -> (Decl,Edit,Edit)
edit_decl_gen o_decl v_decl =
  case (o_decl, v_decl) of
    (MemberDecl o_mem, MemberDecl v_mem) ->
      let (no_mem,o_edit,v_edit) = edit_member_gen o_mem v_mem
      in (MemberDecl no_mem, o_edit, v_edit)
    (InitDecl o_b o_block, InitDecl v_b v_block) ->
      let (no_block,o_edit,v_edit) = edit_block_gen o_block v_block 
      in if o_b == v_b
         then (InitDecl o_b no_block, o_edit, v_edit)
         else error "unsupported differences"
    _ -> error "unsupported differences"

edit_member_gen :: MemberDecl -> MemberDecl -> (MemberDecl,Edit,Edit)
edit_member_gen o_mem v_mem = 
  case (o_mem,v_mem) of
    (FieldDecl _ _ _,_) -> if o_mem == v_mem then (o_mem,[],[]) else error "unsupported differences"
    (MethodDecl o_mods o_tys o_ty o_id o_fpars o_ex o_mbody, MethodDecl v_mods v_tys v_ty v_id v_fpars v_ex v_mbody) ->
      let checks = [ o_mods == v_mods
                   , o_tys == v_tys
                   , o_ty == v_ty
                   , o_id == v_id
                   , o_fpars == v_fpars
                   , o_ex == v_ex ]
          (no_mbody,o_edit,v_edit) = edit_method_body_gen o_mbody v_mbody
      in if all id checks
         then (MethodDecl o_mods o_tys o_ty o_id o_fpars o_ex no_mbody, o_edit, v_edit)
         else error "unsupported differences"
    (ConstructorDecl o_mods o_tys o_id o_fpars o_ex o_cbody, ConstructorDecl v_mods v_tys v_id v_fpars v_ex v_cbody) ->
      let checks = [ o_mods == v_mods
                   , o_tys == v_tys
                   , o_id == v_id
                   , o_fpars == v_fpars
                   , o_ex == v_ex ]
          (no_cbody,o_edit,v_edit) = edit_constructor_body_gen o_cbody v_cbody
      in if all id checks
         then (ConstructorDecl o_mods o_tys o_id o_fpars o_ex no_cbody, o_edit, v_edit)
         else error "unsupported differences"
    (MemberClassDecl o_class, MemberClassDecl v_class) ->
      let (no_class,o_edit,v_edit) = edit_class_gen o_class v_class
      in (MemberClassDecl no_class, o_edit, v_edit) 
    _ -> error "unsupported differences"

edit_method_body_gen :: MethodBody -> MethodBody -> (MethodBody,Edit,Edit)
edit_method_body_gen o_mbody v_mbody =
  if o_mbody == v_mbody
  then (o_mbody, [], [])
  else -- the method bodies are for sure not the same
    case (o_mbody, v_mbody) of
      (MethodBody Nothing, MethodBody (Just block)) -> (MethodBody $ Just $ Block [hole], [skip], [BlockStmt $ StmtBlock block])
      (MethodBody (Just block), MethodBody Nothing) -> (MethodBody $ Just $ Block [hole], [BlockStmt $ StmtBlock block], [skip])
      (MethodBody (Just o_block), MethodBody (Just v_block)) ->
        let (no_block,o_edit,v_edit) = edit_block_gen o_block v_block
        in (MethodBody $ Just no_block, o_edit, v_edit) 

edit_constructor_body_gen :: ConstructorBody -> ConstructorBody -> (ConstructorBody,Edit,Edit)
edit_constructor_body_gen o_cbody v_cbody =
  if o_cbody == v_cbody
  then (o_cbody, [], [])
  else case (o_cbody, v_cbody) of
    (ConstructorBody o_e o_stmts, ConstructorBody v_e v_stmts) -> error "todo" 

edit_block_gen :: Block -> Block -> (Block,Edit,Edit)
edit_block_gen (Block o_block) (Block v_block) =
  let c = lcs o_block v_block
      (no_block,o_edit,v_edit) = diff2edit o_block v_block c (length o_block, length v_block) ([],[],[]) 
  in (Block no_block,o_edit,v_edit)

type M = Map (Int,Int) Int

lk :: (Int,Int) -> M -> Int
lk ij c =
  case M.lookup ij c of
    Nothing -> error "lk"
    Just v  -> v

iM :: Int -> Int -> M
iM m n = 
  let keys = [ (i,j) | i <- [0..m], j <- [0..n] ]
  in M.fromList $ zip keys $ repeat 0   

lcs :: (Show a, Eq a) => [a] -> [a] -> M 
lcs xs ys =
  let -- create the initial matrix
      c = iM (length xs) (length ys) 
      -- create (x_i, i) array
      xs' = zip xs [1..]
      -- create (y_j, j) array
      ys' = zip ys [1..]
      c' = foldl (\c_i (x,i) -> foldl (lcslen (x,i)) c_i ys') c xs' 
  in c'
 where 
    lcslen :: (Show a, Eq a) => (a,Int) -> M -> (a,Int) -> M  
    lcslen (x,i) c (y,j) = 
       if x == y 
       then let p = lk (i-1,j-1) c in M.insert (i,j) (p+1) c
       else M.insert (i,j) (max (lk (i,j-1) c) (lk (i-1,j) c)) c

hole = BlockStmt Hole
skip = BlockStmt Skip

-- printdiff :: (Eq a,Show a) -> [a] -> [a] -> M -> (Int,Int) -> IO ()
diff2edit xs ys c (i,j) (o,a,b) 
  | i > 0 && j > 0 && xs!!(i-1) == ys!!(j-1) = diff2edit xs ys c (i-1,j-1) (xs!!(i-1):o,a,b)
  | j > 0 && (i == 0 || lk (i,j-1) c >= lk (i-1,j) c) = diff2edit xs ys c (i,j-1) (hole:o,skip:a, (ys!!(j-1)):b)
  | i > 0 && (j == 0 || lk (i,j-1) c < lk (i-1,j) c) = diff2edit xs ys c (i-1,j) (hole:o,xs!!(i-1):a, skip:b)
  | otherwise = (o,a,b)


