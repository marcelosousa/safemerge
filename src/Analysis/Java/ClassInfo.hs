{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Java.ClassInfo
-- Copyright :  (c) 2017 Marcelo Sousa

-- | Retrieves info from a Java Program 
-------------------------------------------------------------------------------
module Analysis.Java.ClassInfo where

import Language.Java.Syntax
import qualified Data.Map as M
import Data.Map (Map) 

type MemberSig = (Ident,[Type])
type MemberInfo = Map MemberSig MemberDecl 
type FieldInfo = Map Ident MemberDecl
type ClassInfo = Map Ident ClassSum 
-- Method Identifier: (Class Name, Method Name, Parameter Types)
-- No proper support for anonymous classes
type MIdent = (Ident,Ident,[Type]) 
type MethodSig = (Ident,[Type])
-- AbsMethodSig - Abstract Method Signature: only keeps the arity
type AbsMethodSig = (Ident,Int)

-- Information regarding the methods of a compilation unit
data ClassSum = 
  ClassSum {
    _cl_name   :: [Ident]    -- List of class names from the current to Object
  , _cl_fields :: FieldInfo  -- List of fields
  , _cl_meths  :: MemberInfo -- List of methods
  , _cl_cons   :: MemberInfo -- List of constructors
  -- Missing information such as inner classes and interfaces
  }
  deriving (Show,Ord,Eq)

findClass :: MIdent -> ClassInfo -> ClassSum
findClass (cls,_,_) class_info = 
  case M.lookup cls class_info of
    Nothing -> error $ "findClass: cant find " ++ show cls
    Just class_sum -> class_sum 

findMethod :: MIdent -> ClassInfo -> MemberDecl
findMethod (cls,name,tys) class_info =
  case M.lookup cls class_info of
    Nothing -> error $ "findMethod: cant find " ++ show cls
    Just class_sum@ClassSum{..} -> 
      case M.lookup (name,tys) _cl_meths of
        Nothing -> case M.lookup (name,tys) _cl_cons of
          Nothing -> error $ "findMethod in class: cant find " ++ show (name,tys)
          Just m -> m
        Just m -> m

findMethodGen :: Ident -> Int -> ClassSum -> [MemberDecl]
findMethodGen i ar cl@ClassSum{..} = 
  findMethodGen' i ar $ M.toList _cl_meths 
  where 
   findMethodGen' :: Ident -> Int -> [(MethodSig,MemberDecl)] -> [MemberDecl]
   findMethodGen' i a [] = []
   findMethodGen' i a (((i',a'),m):rest) = 
     if i == i' && a == (length a')
     then m:(findMethodGen' i a rest)
     else findMethodGen' i a rest 

varDeclIdToIdent :: VarDeclId -> Ident 
varDeclIdToIdent v = case v of
  VarId i -> i
  VarDeclArray v' -> varDeclIdToIdent v'

class Signature a where
  toMemberSig :: a -> [MemberSig]

instance Signature MemberDecl where
  -- toMemberSig :: MemberDecl -> [MemberSig]
  toMemberSig mDecl = case mDecl of
    MethodDecl _ _ _ _ params _ _ -> 
      map (\(FormalParam _ ty _ v) -> (varDeclIdToIdent v,[ty])) params
    ConstructorDecl _ _ _ params _ _ -> 
      map (\(FormalParam _ ty _ v) -> (varDeclIdToIdent v,[ty])) params
    FieldDecl _ ty varDecls ->
      map (\(VarDecl v _) -> (varDeclIdToIdent v,[ty])) varDecls
    _ -> []

i_clsum :: [Ident] -> ClassSum
i_clsum name = ClassSum name M.empty M.empty M.empty 

is_null :: ClassSum -> Bool
is_null (ClassSum _ a b c) = M.null a && all M.null [b,c] 

add_clfield :: Ident -> MemberDecl -> ClassSum -> ClassSum
add_clfield _id _field s@ClassSum{..} = 
  let cl_fields = M.insert _id _field _cl_fields 
  in s { _cl_fields = cl_fields }

add_clmeth :: MemberSig -> MemberDecl -> ClassSum -> ClassSum
add_clmeth _id _meth s@ClassSum{..} = 
  let cl_meths = M.insert _id _meth _cl_meths 
  in s { _cl_meths = cl_meths }

add_clcons :: MemberSig -> MemberDecl -> ClassSum -> ClassSum
add_clcons _id _cons s@ClassSum{..} = 
  let cl_cons = M.insert _id _cons _cl_cons 
  in s { _cl_cons = cl_cons }

-- | Main function
class ToClassInfo a where
  toClassInfo :: a -> ClassInfo

instance ToClassInfo Program where
  toClassInfo (CompilationUnit pkg imp ty) = foldr toClassInfo_ty M.empty ty

instance ToClassInfo (Maybe Program) where
  toClassInfo Nothing = M.empty
  toClassInfo (Just prog) = toClassInfo prog 

toClassInfo_ty :: TypeDecl -> ClassInfo -> ClassInfo
toClassInfo_ty ty r = 
 case ty of
  ClassTypeDecl cls ->
   case cls of
    -- ClassDecl Modifier Ident [TypeParam] (Maybe RefTy) [RefTy] ClassBody
    ClassDecl _ id _ _ _ (ClassBody dcls) ->
     let clsum = foldr to_info (i_clsum [id]) dcls
     in if is_null clsum 
        then r
        else M.insert id clsum r 
    _ -> r 
  _ -> r 

getParamType :: FormalParam -> Type       
getParamType (FormalParam _ ty _ _) = ty

-- | Processes the contents of a class
to_info :: Decl -> ClassSum -> ClassSum 
to_info decl _clsum = 
  case decl of 
   MemberDecl mem ->
    case mem of  
     FieldDecl mod ty vardecl -> foldr (field_info mod ty) _clsum vardecl 
     MethodDecl _ _ _ id ps _ _ -> add_clmeth (id, map getParamType ps) mem _clsum 
     ConstructorDecl _ _ id ps _ _ -> add_clcons (id, map getParamType ps) mem _clsum 
     _ -> _clsum
   _ -> _clsum

field_info :: [Modifier] -> Type -> VarDecl -> ClassSum -> ClassSum
field_info mod ty d@(VarDecl id _) _clsum = 
  add_clfield (toVarIdent id) (FieldDecl mod ty [d]) _clsum 

toVarIdent :: VarDeclId -> Ident
toVarIdent vid = case vid of
  VarId i -> i
  VarDeclArray v -> toVarIdent v 

mth_body :: MemberDecl -> MethodBody
mth_body (MethodDecl _ _ _ _ _ _ b) = b
mth_body m = error $ "mth_body: fatal " ++ show m 

toIdent :: Name -> Ident
toIdent (Name []) = error $ "nameToIdent: Name []"
toIdent (Name [x]) = x
toIdent (Name (x:xs)) = foldr (\(Ident a) (Ident b) -> Ident (a ++ "." ++ b)) x xs

findLhs :: MemberDecl -> [Lhs]
findLhs m = case m of
  MethodDecl _ _ _ _ _ _ (MethodBody (Just (Block b))) -> 
    foldr (\s r -> findLhsBStmt s ++ r) [] b  
  ConstructorDecl _ _ _ _ _ (ConstructorBody _ b) -> 
    foldr (\s r -> findLhsBStmt s ++ r) [] b  
  _ -> []

findLhsBStmt :: BlockStmt -> [Lhs]
findLhsBStmt (BlockStmt s) = findLhsStmt s
findLhsBStmt _ = [] 

findLhsStmt :: Stmt -> [Lhs]
findLhsStmt st = case st of
  StmtBlock (Block b) -> foldr (\s r -> findLhsBStmt s ++ r) [] b  
  IfThen c s -> findLhsExp c ++ findLhsStmt s 
  IfThenElse c s1 s2 -> findLhsExp c ++ findLhsStmt s1 ++ findLhsStmt s2 
  While c s -> findLhsExp c ++ findLhsStmt s 
  BasicFor mI me mE s ->
    let a = case mI of
              Just (ForInitExps l) -> foldr (\s r -> findLhsExp s ++ r) [] l  
              _ -> []
        b = case me of
              Just e -> findLhsExp e
              _ -> []
        c = case mE of
              Just es -> foldr (\s r -> findLhsExp s ++ r) [] es 
              _ -> []
    in a ++ b ++ c ++ findLhsStmt s
  EnhancedFor _ _ _ e s -> findLhsExp e ++ findLhsStmt s
  ExpStmt e -> findLhsExp e 
  Assert e me -> case me of 
   Nothing -> findLhsExp e
   Just e' -> findLhsExp e ++ findLhsExp e'
  Assume e -> findLhsExp e
  Do s e   -> findLhsStmt s ++ findLhsExp e 
  Return me -> case me of
   Nothing -> []
   Just e  -> findLhsExp e 
  Labeled _ s -> findLhsStmt s 
  _ -> []

findLhsExp :: Exp -> [Lhs]
findLhsExp e = case e of 
  FieldAccess fa -> [FieldLhs fa] 
  PostIncrement e -> findLhsExp e 
  PostDecrement e -> findLhsExp e 
  PreIncrement  e -> findLhsExp e 
  PreDecrement  e -> findLhsExp e 
  Assign lhs _ _ -> [lhs] 
  _ -> []
