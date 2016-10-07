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

type Edits = [(BlockStmt,BlockStmt,BlockStmt,BlockStmt)]
type Edit = [BlockStmt] 
type Program = CompilationUnit

test_edit_gen :: FilePath -> IO Program 
test_edit_gen orig = do
  orig_ast <- parser compilationUnit `fmap` readFile orig
  case orig_ast of
    Right o_ast -> return o_ast 
    Left err -> error $ "parse error..." ++ show err

main_edit_gen :: FilePath -> FilePath -> IO () --CompilationUnit, Edit, Edit
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

normalize :: (Program,Edits) -> (Program,Edits)
normalize (p@(CompilationUnit pkg imp ty),es) =
  let (n_ty,es') = normalize_tys ty es
  in (CompilationUnit pkg imp n_ty,es')

normalize_tys :: [TypeDecl] -> Edits -> ([TypeDecl],Edits)
normalize_tys xs [] = (xs,[])
normalize_tys [] _  = error "normalize_tys: more edits than holes"
normalize_tys (x:xs) e =
  let (x',e',es) = normalize_ty x e
      (xs',es') = normalize_tys xs es
  in (x':xs',e'++es') 

normalize_ty :: TypeDecl -> Edits -> (TypeDecl,Edits,Edits)
normalize_ty ty e = case ty of
  ClassTypeDecl _class -> 
    let (n_class,n_e,c_e) = normalize_class _class e 
    in (ClassTypeDecl n_class,n_e,c_e)
  InterfaceTypeDecl inter -> (ty,[],e) 

normalize_class :: ClassDecl -> Edits -> (ClassDecl,Edits,Edits)
normalize_class _class e = case _class  of
  ClassDecl o_mods o_id o_tys o_mref o_reftys o_body ->
    let (no_body,n_e,c_e) = normalize_class_body o_body e 
    in (ClassDecl o_mods o_id o_tys o_mref o_reftys no_body,n_e,c_e)
  EnumDecl _ _ _ _ -> (_class,[],e) 

normalize_class_body :: ClassBody -> Edits -> (ClassBody,Edits,Edits)
normalize_class_body (ClassBody decls) e =
  let (decls',e',c_e) = normalize_decls decls e
  in (ClassBody decls',e',c_e)

normalize_decls :: [Decl] -> Edits -> ([Decl],Edits,Edits)
normalize_decls ds [] = (ds,[],[])
normalize_decls ds e  =
  case ds of
    [] -> error "normalize_decls: more edits than holes"
    (d:ds) -> let (d',e',c_e) = normalize_decl d e
                  (ds',e'',c_e') = normalize_decls ds c_e
              in (d':ds',e'++e'',c_e')

normalize_decl :: Decl -> Edits -> (Decl,Edits,Edits)
normalize_decl decl e = case decl of
  MemberDecl o_mem ->
    let (no_mem,n_e,c_e) = normalize_member o_mem e 
    in (MemberDecl no_mem,n_e,c_e)
  InitDecl o_b o_block ->
    let (no_block,n_e,c_e) = normalize_block o_block e 
    in (InitDecl o_b no_block,n_e,c_e)

normalize_member :: MemberDecl -> Edits -> (MemberDecl,Edits,Edits)
normalize_member o_mem e = 
  case o_mem of
    FieldDecl _ _ _ -> (o_mem,[],e) 
    MethodDecl o_mods o_tys o_ty o_id o_fpars o_ex o_mbody ->
      let (no_mbody,n_e,c_e) = normalize_method_body o_mbody e 
      in (MethodDecl o_mods o_tys o_ty o_id o_fpars o_ex no_mbody,n_e,c_e)
    ConstructorDecl o_mods o_tys o_id o_fpars o_ex o_cbody ->
      let (no_cbody,n_e,c_e) = normalize_constructor_body o_cbody e
      in (ConstructorDecl o_mods o_tys o_id o_fpars o_ex no_cbody,n_e,c_e)
    MemberClassDecl o_class ->
      let (no_class,n_e,c_e) = normalize_class o_class e 
      in (MemberClassDecl no_class,n_e,c_e) 

normalize_method_body :: MethodBody -> Edits -> (MethodBody,Edits,Edits)
normalize_method_body o_mbody e = case o_mbody of
  MethodBody Nothing -> (o_mbody,[],e) 
  MethodBody (Just block) -> 
    let (no_block,n_e,c_e) = normalize_block block e
    in (MethodBody $ Just no_block,n_e,c_e)

normalize_constructor_body :: ConstructorBody -> Edits -> (ConstructorBody,Edits,Edits)
normalize_constructor_body o_cbody e = (o_cbody,[],e)

normalize_block :: Block -> Edits -> (Block,Edits,Edits)
normalize_block (Block o_block) e =
  let (no_block,e',c_e) = normalize_blockstmt o_block e
  in (Block no_block,e',c_e)

normalize_blockstmt :: [BlockStmt] -> Edits -> ([BlockStmt],Edits,Edits)
normalize_blockstmt bs [] = (bs,[],[])
normalize_blockstmt (b:bs) es = case b of
  BlockStmt s -> case s of
    -- current position is a hole
    -- if the next one is not a hole, we dont apply
    Hole -> case bs of
      [] -> ([b],[head es],tail es)
      (b':bs') -> case b' of
        -- there are two consecutive holes
        BlockStmt Hole -> undefined
        _ -> let (bs',es',c_es) = normalize_blockstmt bs (tail es)
             in (b:bs',(head es):es',c_es)
    _ -> let (bs',es',c_es) = normalize_blockstmt bs es
         in (b:bs',es',c_es)
  _ -> let (bs',es',c_es) = normalize_blockstmt bs es
       in (b:bs',es',c_es)

-- | Parses the 4 files
kast :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Program,Program,Program,Program)
kast ofl afl bfl mfl = do
  _o <- parser compilationUnit `fmap` readFile ofl 
  _a <- parser compilationUnit `fmap` readFile afl 
  _b <- parser compilationUnit `fmap` readFile bfl 
  _m <- parser compilationUnit `fmap` readFile mfl 
  case (_o,_a,_b,_m) of
    (Right o, Right a, Right b, Right m) -> return (o,a,b,m)

-- Main function that gets the edit scripts
kedits :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
kedits ofl afl bfl mfl = do
  (o,a,b,m) <- kast ofl afl bfl mfl
  let (no,ea) = gen_edit o a []
      (nno,eab) = gen_edit no b ea
      (nnno,eabm) = gen_edit nno m eab
      (fo,es@[e_a,e_b,e_m,e_o]) = gen_edit nnno o eabm
      pairs = [(o,fo,e_o),(a,fo,e_a),(b,fo,e_b),(m,fo,e_m)]
  putStrLn $ prettyPrint fo 
  mapM_ print es
  mapM_ (print . check_edit_soundness) pairs
 
-- | Check the soundness of the edit script 
check_edit_soundness :: (Program,Program,Edit) -> Bool
check_edit_soundness (original,holes,edit) = 
  original == (apply_edit holes edit)

-- | Apply the edit script 
apply_edit :: CompilationUnit -> Edit -> CompilationUnit
apply_edit p@(CompilationUnit pkg imp ty) e =
  case e of
    [] -> p
    _  -> let n_ty = apply_edit_tys ty e
          in CompilationUnit pkg imp n_ty

apply_edit_tys :: [TypeDecl] -> Edit -> [TypeDecl]
apply_edit_tys xs [] = xs
apply_edit_tys [] _  = error "apply_edit_tys: more edits than holes"
apply_edit_tys (x:xs) e =
  let (x',e') = apply_edit_ty x e
  in x':(apply_edit_tys xs e')

apply_edit_ty :: TypeDecl -> Edit -> (TypeDecl,Edit)
apply_edit_ty ty e = case ty of
  ClassTypeDecl _class -> 
    let (n_class,n_e) = apply_edit_class _class e 
    in (ClassTypeDecl n_class,n_e)
  InterfaceTypeDecl inter -> (ty,e) 

apply_edit_class :: ClassDecl -> Edit -> (ClassDecl,Edit)
apply_edit_class _class e = case _class  of
  ClassDecl o_mods o_id o_tys o_mref o_reftys o_body ->
    let (no_body,n_e) = apply_edit_class_body o_body e 
    in (ClassDecl o_mods o_id o_tys o_mref o_reftys no_body,n_e)
  EnumDecl _ _ _ _ -> (_class,e) 

apply_edit_class_body :: ClassBody -> Edit -> (ClassBody,Edit)
apply_edit_class_body (ClassBody decls) e =
  let (decls',e') = apply_edit_decls decls e
  in (ClassBody decls',e')

apply_edit_decls :: [Decl] -> Edit -> ([Decl],Edit)
apply_edit_decls ds [] = (ds,[])
apply_edit_decls ds e  =
  case ds of
    [] -> error "apply_edit_decls: more edits than holes"
    (d:ds) -> let (d',e') = apply_edit_decl d e
                  (ds',e'') = apply_edit_decls ds e'
              in (d':ds',e'')

apply_edit_decl :: Decl -> Edit -> (Decl,Edit)
apply_edit_decl decl e = case decl of
  MemberDecl o_mem ->
    let (no_mem,n_e) = apply_edit_member o_mem e 
    in (MemberDecl no_mem,n_e)
  InitDecl o_b o_block ->
    let (no_block,n_e) = apply_edit_block o_block e 
    in (InitDecl o_b no_block,n_e)

apply_edit_member :: MemberDecl -> Edit -> (MemberDecl,Edit)
apply_edit_member o_mem e = 
  case o_mem of
    FieldDecl _ _ _ -> (o_mem,e) 
    MethodDecl o_mods o_tys o_ty o_id o_fpars o_ex o_mbody ->
      let (no_mbody,n_e) = apply_edit_method_body o_mbody e 
      in (MethodDecl o_mods o_tys o_ty o_id o_fpars o_ex no_mbody,n_e)
    ConstructorDecl o_mods o_tys o_id o_fpars o_ex o_cbody ->
      let (no_cbody,n_e) = apply_edit_constructor_body o_cbody e
      in (ConstructorDecl o_mods o_tys o_id o_fpars o_ex no_cbody,n_e)
    MemberClassDecl o_class ->
      let (no_class,n_e) = apply_edit_class o_class e 
      in (MemberClassDecl no_class,n_e) 

apply_edit_method_body :: MethodBody -> Edit -> (MethodBody,Edit)
apply_edit_method_body o_mbody e = case o_mbody of
  MethodBody Nothing -> (o_mbody,e) 
  MethodBody (Just block) -> 
    let (no_block,n_e) = apply_edit_block block e
    in (MethodBody $ Just no_block,n_e)

apply_edit_constructor_body :: ConstructorBody -> Edit -> (ConstructorBody,Edit)
apply_edit_constructor_body o_cbody e = (o_cbody,e)

apply_edit_block :: Block -> Edit -> (Block,Edit)
apply_edit_block (Block o_block) e =
  let (no_block,e') = apply_edit_blockstmt o_block e
  in (Block no_block,e')

apply_edit_blockstmt :: [BlockStmt] -> Edit -> ([BlockStmt],Edit)
apply_edit_blockstmt bs e = foldl apply_edit_bstmt ([],e) bs

apply_edit_bstmt :: ([BlockStmt],Edit) -> BlockStmt -> ([BlockStmt],Edit)
apply_edit_bstmt (bs,[]) bstmt = (bs++[bstmt],[])
apply_edit_bstmt (bs,e)  bstmt = case bstmt of
  BlockStmt s -> case s of
    StmtBlock b -> let (b',e') = apply_edit_block b e
                   in (bs++[BlockStmt $ StmtBlock b'],e')
    Hole -> case e of
      (BlockStmt Skip):e' -> (bs,e')
      st:e'   -> (bs++[st],e')
    Skip -> (bs,e) 
    _ -> (bs++[bstmt],e)
  _ -> (bs++[bstmt],e)
         
-- | Edit generation
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

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 [] _ _ _ = []
zip4 _ [] _ _ = []
zip4 _ _ [] _ = []
zip4 _ _ _ [] = []
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d):(zip4 as bs cs ds)
