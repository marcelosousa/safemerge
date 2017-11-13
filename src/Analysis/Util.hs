module Analysis.Util  where

import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Data.Map (Map)
import Edit.Types hiding (push)
import Language.Java.Syntax
import qualified Data.Map as M
import Z3.Monad hiding (Params)

-- | Z3 QUERIES
-- Performs a SAT-query.
checkSAT phi = local (assert phi >> check) 

-- |= pre => post 
-- UNSAT (not (pre => post))  
helper pre post = do
  formula <- mkImplies pre post >>= mkNot 
  assert formula
  (r, m)  <- getModel
  preStr  <- astToString pre
  -- trace ("helper: " ++ preStr) $ return (r,m)
  return (r,m)

-- \phi not models \psi
-- \phi |= \psi 
implies :: AST -> AST -> Z3 Bool
implies phi psi = do 
  formula <- mkImplies phi psi >>= mkNot 
  push 
  assert formula 
  res <- check
  pop 1
  -- phi_str <- astToString phi
  -- psi_str <- astToString psi
  -- liftIO $ putStrLn $ "implies:\n" ++ phi_str ++ "\n" ++ psi_str ++ "\nresult: = " ++ (show $ res == Unsat)
  -- _ <- liftIO $ getChar
  case res of
    Unsat -> return True
    _     -> return False 

-- | END OF Z3 QUERIES


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

expToIdent :: Exp -> Ident
expToIdent exp = case exp of
  FieldAccess (PrimaryFieldAccess _ i) -> i
  ArrayAccess (ArrayIndex e _)         -> expToIdent e
  ExpName n                            -> toIdent n

getIdentsExp :: Exp -> [Ident]
getIdentsExp exp = case exp of
  ThisClass n -> [toIdent n] 
  FieldAccess fa -> getIdentsFA fa 
  MethodInv   ma -> getIdentsMA ma 
  ArrayAccess (ArrayIndex e es) -> foldr (\e r -> getIdentsExp e ++ r) [] (e:es) 
  ExpName n -> [toIdent n] 
  PostIncrement e -> getIdentsExp e 
  PostDecrement e -> getIdentsExp e 
  PreIncrement  e -> getIdentsExp e  
  PreDecrement  e -> getIdentsExp e  
  PrePlus       e -> getIdentsExp e  
  PreMinus      e -> getIdentsExp e  
  PreBitCompl   e -> getIdentsExp e  
  PreNot        e -> getIdentsExp e  
  Cast _        e -> getIdentsExp e 
  BinOp e _ e'    -> getIdentsExp e ++ getIdentsExp e' 
  InstanceOf  e _ -> getIdentsExp e 
  Cond e e' e'' ->  getIdentsExp e ++ getIdentsExp e' ++ getIdentsExp e'' 
  Assign lhs _ e -> getIdentsLhs lhs ++ getIdentsExp e 

getIdentsFA :: FieldAccess -> [Ident]
getIdentsFA fa = case fa of
  PrimaryFieldAccess e i -> [i] ++ getIdentsExp e
  SuperFieldAccess   i -> [i] 
  ClassFieldAccess n i -> [toIdent n, i] 

getIdentsMA :: MethodInvocation -> [Ident]
getIdentsMA met = case met of
  MethodCall n ars -> [toIdent n] ++ foldr (\e r -> getIdentsExp e ++ r) [] ars 
  PrimaryMethodCall e _ i ars -> [i] ++ foldr (\e r -> getIdentsExp e ++ r) [] (e:ars) 
  SuperMethodCall _ i ars -> [i] ++ foldr (\e r -> getIdentsExp e ++ r) [] ars
  ClassMethodCall n _ i ars -> [i,toIdent n] ++ foldr (\e r -> getIdentsExp e ++ r) [] ars 
  TypeMethodCall  n _ i ars -> [i,toIdent n] ++ foldr (\e r -> getIdentsExp e ++ r) [] ars 

getIdentsLhs :: Lhs -> [Ident]
getIdentsLhs lhs = case lhs of
  NameLhs n -> [toIdent n]
  FieldLhs fa -> getIdentsFA fa
  ArrayLhs (ArrayIndex e es) -> foldr (\e r -> getIdentsExp e ++ r) [] (e:es) 

getReturnType :: AnnMemberDecl -> Maybe Type
getReturnType m = case m of
  AnnMethodDecl _ _ rTy _ _ _ _ -> rTy 
  _ -> error "getReturnType: is not a method"

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

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

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

concatIdent :: [Ident] -> Ident
concatIdent [x] = x
concatIdent ((Ident x):xs) = 
  let Ident y = concatIdent xs
  in Ident (x ++ "." ++ y)
