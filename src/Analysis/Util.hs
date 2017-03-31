module Analysis.Util (get_method, getParIdents, safeLookup, toString) where

import Data.Map (Map)
import Edit.Types
import Language.Java.Syntax
import qualified Data.Map as M

get_method :: Program -> Method
get_method (CompilationUnit _ _ [ty]) = 
  case ty of 
    ClassTypeDecl (ClassDecl _ _ _ _ _ bd) ->
      case bd of
        ClassBody [dc] -> case dc of
          MemberDecl m -> case m of
            MethodDecl _ _ _ _ p _ b -> case b of
              MethodBody (Just bl) -> (p,bl)
              _ -> error "get_method"
            _ -> error "get_method"
          _ -> error "get_method"
        _ -> error "get_method"
    _ -> error "get_method" 
get_method _ = error "get_method"

getParIdents :: [FormalParam] -> [String]
getParIdents pars = map getParIdent pars

getParIdent :: FormalParam -> String
getParIdent (FormalParam _ _ _ (VarId ident)) = toString ident
getParIdent _ = error "getParIdent"

toString :: Ident -> String
toString (Ident str) = str

safeLookup :: Ord k => String -> k -> Map k a -> a
safeLookup err k m = case M.lookup k m of
    Nothing -> error err
    Just a  -> a
