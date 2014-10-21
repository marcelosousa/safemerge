{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module Converter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language as SC
import qualified Data.Map as M
import Data.Map (Map)

class Convertible a b | a -> b where
    translate :: a -> b
    
instance Convertible CTranslUnit SC.Program where
    translate (CTranslUnit decls _) = SC.Program $ translate decls  

instance Convertible [CExternalDeclaration NodeInfo] (SC.Decls, SC.Defs) where
    translate = foldr (combine . translate) ([], [])

combine :: Either SC.Declaration SC.Definition -> (SC.Decls, SC.Defs) -> (SC.Decls, SC.Defs)
combine (Left d)  (decls, defs) = (d:decls, defs)
combine (Right d) (decls, defs) = (decls, d:defs) 

instance Convertible (CExternalDeclaration NodeInfo) (Either SC.Declaration SC.Definition) where
    translate cdecl = case cdecl of
        CDeclExt decl -> Left $ translate decl
        CFDefExt def  -> Right $ translate def
        _               -> error "Unsupported CExternalDeclaration"
    
instance Convertible (CDeclaration NodeInfo) SC.Declaration where
    translate (CDecl _ [(mdecl,miniti,_)] n) =
      let pc = translate n 
      in case mdecl of
        Nothing -> error "cant convert CDecl without a name"
        Just decl -> case translate decl of
          Left i -> case miniti of
              Nothing    -> SC.GlobalDecl pc i Nothing
              Just initi -> SC.GlobalDecl pc i $ Just $ translate initi
          Right (i,p) -> SC.FunctionDecl pc i p
    translate _ = error "cant convert CDecl"

instance Convertible (CDeclarator NodeInfo) (Either SC.Ident (SC.Ident, SC.Params)) where
--    translate (CDeclr Nothing  _  _ _ _) = error "cant convert declarators without name"
    translate (CDeclr (Just i) [] _ _ _) = Left $ translate i
    translate (CDeclr (Just i) p  _ _ _) = Right (translate i, foldr (\p' r -> (translate p') ++ r) [] p)
    translate _ = error "cant convert declarators"

instance Convertible (CDerivedDeclarator NodeInfo) SC.Params where
    translate (CFunDeclr (Left is)       _ _) = map (SC.Param . translate) is
    translate (CFunDeclr (Right (is',_)) _ _) = map (SC.Param . getIdent) is'
    translate r = [] --error $ "FAIL: " ++ show k
    
instance Convertible (CInitializer NodeInfo) SC.Value where
    translate (CInitExpr expr _) = getValue expr
    translate _ = error "cant convert CInitializer"

getValue :: (CExpression NodeInfo) -> SC.Value
getValue (CConst (CIntConst (CInteger i _ _) _)) = SC.IntValue i
getValue (CConst (CFloatConst (CFloat i) _)) = SC.FloatValue $ read i
getValue c = error $  "cant get value " ++ show c
    
instance Convertible NodeInfo SC.PC where
    translate (NodeInfo _ _ (Name pc)) = pc
    translate _ = error "cant convert NodeInfo"

instance Convertible Ident SC.Ident where
    translate (Ident i _ _) = i
    
class Identifiable a where
    getIdent :: a -> SC.Ident
    
instance Identifiable (CDeclaration NodeInfo) where
  getIdent (CDecl _ [((Just (CDeclr (Just i) _ _ _ _)), _, _)] _) = translate i
  getIdent _ = error "cant get identifier of CDeclaration"

{-    
instance Convertible [CDeclarationSpecifier NodeInfo] Type where
    translate [CTypeSpec ctype] = translate ctype
    translate _ = error "cant support this CDeclarationSpecifier"

instance Convertible (CTypeSpecifier NodeInfo) Type where
    translate (CIntType _) = IntType
    translate _ = error "cant support this CTypeSpecifier"    
-}
                
instance Convertible (CFunctionDef NodeInfo) SC.Definition where
    translate (CFunDef _ nameDecl _ stat n) = 
        let pc = translate n
        in case translate nameDecl of
            Left _ -> error "cant conver this function definition"
            Right (name, params) -> 
              let stats = translate stat
              in SC.FunctionDef pc name params stats

instance Convertible (CStatement NodeInfo) SC.Statement where
    translate (CReturn (Just expr) n) = SC.Return (translate n) $ translate expr
    translate (CExpr   (Just expr) _) = case expr of
        CAssign _ lhs rhs n -> 
          let lhsExpr = translate lhs
              -- varName = getIdent lhs
              rhsExpr = translate rhs
              pc      = translate n
          in SC.Assign pc lhsExpr rhsExpr
        CCall fname args n  -> 
          let fName = getIdent fname
              asExpr = map translate args
              pc = translate n
          in SC.CallS pc fName asExpr
        CStatExpr stat n    -> translate stat 
    translate (CCompound _ stats _) = translate stats
    translate (CIf condExpr thenStat melseStat n) =
        let expr = translate condExpr
            sthen = translate thenStat
            pc = translate n
        in case melseStat of
            Nothing -> SC.IfThen pc expr sthen
            Just elseStat -> 
              let selse = translate elseStat
              in SC.If pc expr sthen selse
    translate (CWhile condExpr bodyStat False n) = 
        let expr = translate condExpr
            sbody = translate bodyStat
            pc = translate n
        in SC.While pc expr sbody        
    translate s = error $ "cant convert statement " ++ show s
    
instance Convertible [CCompoundBlockItem NodeInfo] SC.Statement where
    translate [] = error "empty compound block"
    translate [s] = translate s
    translate (s:ss) = SC.Sequence (translate s) (translate ss)
    -- foldr (M.union . translate) M.empty

instance Convertible (CCompoundBlockItem NodeInfo) SC.Statement where
    translate (CBlockStmt stat) = translate stat
    translate (CBlockDecl decl) = let (v,e,pc) = getLocal decl
                                  in SC.Local pc v e
    translate _ = error "cant convert CCompoundBlockItem"

instance Identifiable (CExpression NodeInfo) where
    getIdent (CVar ident _) = translate ident
    getIdent _ = error "cant get ident of CExpression"

getLocal :: CDeclaration NodeInfo -> (SC.Expression, Maybe SC.Expression, Int)
getLocal (CDecl _ [(Just (CDeclr (Just i) _ _ _ _),Nothing,_)] n) = 
    let pc = translate n
    in (SC.Ident $ translate i, Nothing, pc)
getLocal (CDecl _ [(Just (CDeclr (Just i) _ _ _ _),Just (CInitExpr expr _),_)] n) = 
    let pc = translate n
    in (SC.Ident $ translate i, Just $ translate expr, pc)

    
instance Convertible (CExpression NodeInfo) SC.Expression where
    translate expr = case expr of
      CVar ident _ -> SC.Ident $ translate ident
      CConst c     -> SC.Const $ translate c
      CBinary binop e1 e2 _ -> 
        let expr1 = translate e1
            expr2 = translate e2
        in SC.BinOp binop expr1 expr2
      CCall fname args _ -> 
        let fName = getIdent fname
            asExpr = map translate args
        in SC.Call fName asExpr
      CUnary unOp e1 _ ->
        let expr1 = translate e1
        in SC.UnaryOp unOp expr1
      CIndex lhs rhs _ ->
        let expr1 = translate lhs
            expr2 = translate rhs
        in SC.Index expr1 expr2
      _ -> error $ show expr

instance Convertible (CConstant NodeInfo) SC.Value where
    translate cons = case cons of
        CIntConst (CInteger i _ _) _ -> SC.IntValue i
        CStrConst (CString s _) _ -> SC.StrValue s
        _ -> error "cant convert constant"      