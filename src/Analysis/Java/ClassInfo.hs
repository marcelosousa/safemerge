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
type QName = (Ident, Ident, MethodBody)
type SQName = (Ident, MemberSig)

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
field_info mod ty d@(VarDecl (VarId id) _) _clsum = 
  add_clfield id (FieldDecl mod ty [d]) _clsum 

mth_body :: MemberDecl -> MethodBody
mth_body (MethodDecl _ _ _ _ _ _ b) = b
mth_body m = error $ "mth_body: fatal " ++ show m 
