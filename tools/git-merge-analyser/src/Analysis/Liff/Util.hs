module Analysis.Liff.Util where

-- | Retrieves info from a CompilationUnit

import Language.Java.Syntax
import qualified Data.Map as M
import Data.Map (Map) 
import Analysis.Liff.Types

toClassInfo :: Maybe Program -> ClassInfo 
toClassInfo Nothing = M.empty
toClassInfo (Just (CompilationUnit pkg imp ty)) = foldr toClassInfo_ty M.empty ty

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

-- | Processes the contents of a class
to_info :: Decl -> ClassSum -> ClassSum 
to_info decl _clsum = 
  case decl of 
   MemberDecl mem ->
    case mem of  
     FieldDecl mod ty vardecl -> foldr (field_info mod ty) _clsum vardecl 
     MethodDecl _ _ _ id _ _ _ -> add_clmeth id mem _clsum 
     ConstructorDecl _ _ id _ _ _ -> add_clcons id mem _clsum 
     _ -> _clsum
   _ -> _clsum

field_info :: [Modifier] -> Type -> VarDecl -> ClassSum -> ClassSum
field_info mod ty d@(VarDecl (VarId id) _) _clsum = 
  add_clfield id (FieldDecl mod ty [d]) _clsum 
