{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Normalize
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit.Normalize where

import Data.Map (Map)
import Edit.Types
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Debug.Trace as T

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
