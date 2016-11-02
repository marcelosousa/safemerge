{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Apply 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit.Apply where

import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import qualified Debug.Trace as T
import Edit.Types

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
  BlockStmt s ->
    let (s',e') = apply_edit_stmt e s
    in case s' of
      Nothing -> (bs, e') 
      Just st -> (bs++[st], e') 
  _ -> (bs++[bstmt],e)

apply_edit_stmt :: Edit -> Stmt -> (Maybe BlockStmt, Edit)
apply_edit_stmt e stmt = case stmt of
  StmtBlock b -> let (b', e') = apply_edit_block b e
                 in (Just $ BlockStmt $ StmtBlock b', e')
  Hole -> case e of
    (BlockStmt Skip):e' -> (Nothing, e')
    (bs:e')   -> (Just bs, e')
    _ -> error $ "apply_edit_stmt: " ++ show e
  Skip -> (Nothing, e)
  IfThen c1 t1 ->
    let (t1',e') = apply_edit_stmt e t1
    in case t1' of
      Just (BlockStmt _t1) -> (Just $ BlockStmt $ IfThen c1 _t1, e') 
      _ -> error $ "apply_edit_stmt: IfThen" ++ show t1'
  While c1 t1 ->
    let (t1',e') = apply_edit_stmt e t1
    in case t1' of
      Just (BlockStmt _t1) -> (Just $ BlockStmt $ While c1 _t1, e') 
      _ -> error $ "apply_edit_stmt: While" ++ show t1'
  IfThenElse c1 t1 t2 ->
    let (t1',e')  = apply_edit_stmt e t1
        (t2',e'') = apply_edit_stmt e' t2
    in case (t1',t2') of
      (Nothing,_)  -> error "apply_edit_stmt: IfThenElse"
      (_,Nothing)  -> error "apply_edit_stmt: IfThenElse"
      (Just (BlockStmt _t1), Just (BlockStmt _t2)) -> 
        let ns = IfThenElse c1 _t1 _t2
        in (Just $ BlockStmt $ ns, e'') 
  _ -> (Just $ BlockStmt stmt, e)
