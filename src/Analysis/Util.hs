module Analysis.Util  where

import Data.Map (Map)
import Edit.Types
import Language.Java.Syntax
import qualified Data.Map as M
import Z3.Monad hiding (Params)

-- Performs a SAT-query.
checkSAT phi = local (assert phi >> check) 

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
 where
  getParIdent :: FormalParam -> String
  getParIdent (FormalParam _ _ _ (VarId ident)) = toString ident
  getParIdent _ = error "getParIdent"

toString :: Ident -> String
toString (Ident str) = str

safeLookup :: Ord k => String -> k -> Map k a -> a
safeLookup err k m = case M.lookup k m of
    Nothing -> error err
    Just a  -> a

replace :: Int -> a -> [a] -> [a]
replace 0 a [] = [a] 
replace 0 a l  = a:(tail l)
replace i a [] = error "replace ..."
replace i a (h:hs) = h:(replace (i-1) a hs)

pairFst :: (a,b,c) -> (a,b,c) -> (a,a)
pairFst (a,_,_) (a',_,_) = (a,a')

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

-- computes a triangle of equalities
comb :: [(i,n,a)] -> [(i,n,n,a,a)]
comb [] = []
comb ((i,n,x):xs) = [(i,n,m,x,y) | (_,m,y) <- xs] ++ comb xs

lin :: [a] -> [(a,a)]
lin [] = []
lin (x:xs) = lin' x xs 
 where
  lin' :: a -> [a] -> [(a,a)]
  lin' l [] = []
  lin' l (x:xs) = (l,x):lin' x xs

zip4 :: [[a]] -> [(a,a,a,a)] 
zip4 []   = []
zip4 [[]] = []
zip4 [(a1:r1),(a2:r2),(a3:r3),(a4:r4)] = 
  (a1,a2,a3,a4):(zip4 [r1,r2,r3,r4])
