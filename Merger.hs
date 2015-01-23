module Merger where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.AST as SC
import Language.SimpleC.Converter
import Language.SimpleC.Printer

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Control.Monad.State.Strict

import qualified Debug.Trace as T

--trace = T.trace
trace a b = b

type Environment = Map SC.Expression ValLat
type Context = Environment -- Map Int Environment 
type Triplet = (Context, Context, Context)
data Dir = Branches | BranchLeft | BranchRight
type MergeState = [Triplet]

data ValLat = Bot | Top | V SC.Value
  deriving (Show,Ord)
  
instance Eq ValLat where
  (==) (V v1) (V v2) = v1 == v2
  (==) _ _ = False
  
iMergeState :: MergeState
iMergeState = [(M.empty, M.empty, M.empty)]
    
mergeProg :: SC.Program -> SC.Program -> SC.Program -> Maybe SC.Program
mergeProg (SC.Program ([],[dbase])) (SC.Program ([],[dv1])) (SC.Program ([],[dv2])) =
    case mergeDef dbase dv1 dv2 of
      Nothing -> error "mergeProg: Failed"
      Just dm -> Just $ SC.Program ([], [dm])
mergeProg _ _ _ = error "mergeProg: TODO"
    
mergeDef :: SC.Definition -> SC.Definition -> SC.Definition -> Maybe SC.Definition
mergeDef base@(SC.FunctionDef pcb fb psb sb) v1@(SC.FunctionDef pcv1 fv1 psv1 sv1) v2@(SC.FunctionDef pcv2 fv2 psv2 sv2) = 
    if fb==fv1 && fb==fv2 && psb == psv1 && psb == psv2
    then case evalState (merge sb sv1 sv2) iMergeState of
        Nothing -> error "mergeDef: Failed"
        Just s  -> Just $ SC.FunctionDef pcb fb psb s
    else Nothing

{-
merge :: Maybe SC.Statement -> Maybe SC.Statement -> Maybe SC.Statement -> State MergeState (Maybe SC.Statement)
merge Nothing Nothing     Nothing     = error "merge: Nothing Nothing Nothing"
merge Nothing s1@(Just _) Nothing     = return s1
merge Nothing Nothing     s2@(Just _) = return s2
merge Nothing (Just s1)   (Just s2)   = mergeBin Branches s1 s2
merge (Just s) Nothing    Nothing     = error "merge: Just Nothing Nothing"
merge (Just s) (Just s1)  Nothing     = mergeBin BranchLeft  s s1
merge (Just s) Nothing    (Just s2)   = mergeBin BranchRight s s2 
merge (Just s) (Just s1)  (Just s2)   = merge' s s1 s2
-}

merge :: SC.Statement -> SC.Statement -> SC.Statement -> State MergeState (Maybe SC.Statement)
merge base v1 v2 = case base of
  SC.Sequence bh bt -> case (v1, v2) of
    (SC.Sequence v1h v1t, SC.Sequence v2h v2t) -> do
      mh <- step bh v1h v2h -- Assumes that sequences grow to the right
      mt <- merge bt v1t v2t
      return $ liftM2 SC.Sequence mh mt
    (SC.Sequence v1h v1t, v2l) -> do
      mh <- step bh v1h v2l
      mt <- mergeBin BranchLeft bt v1t
      return $ liftM2 SC.Sequence mh mt
    (v1l, SC.Sequence v2h v2t) -> do
      mh <- step bh v1l v2h
      mt <- mergeBin BranchRight bt v2t
      return $ liftM2 SC.Sequence mh mt
    (v1l, v2l) -> step bh v1l v2l
  bl -> case (v1, v2) of
    (SC.Sequence v1h v1t, SC.Sequence v2h v2t) -> do
      mh <- step bl v1h v2h
      mt <- mergeBin Branches v1t v2t
      return $ liftM2 SC.Sequence mh mt
    (SC.Sequence v1h v1t, v2l) -> do
      mh <- step bl v1h v2l
      return $ liftM (\s -> SC.Sequence s v1t) mh 
    (v1l, SC.Sequence v2h v2t) -> do
      mh <- step bl v1l v2h
      return $ liftM (\s -> SC.Sequence s v2t) mh 
    (v1l, v2l) -> step bl v1l v2l

mergeBin :: Dir -> SC.Statement -> SC.Statement -> State MergeState (Maybe SC.Statement)
mergeBin Branches    s1 s2 = error "mergeBin: can't do it yet"
mergeBin BranchLeft  sb s1 = return $ Just s1
mergeBin BranchRight sb s2 = return $ Just s2

-- step symbolically runs one atomic statement of each program
step :: SC.Statement -> SC.Statement -> SC.Statement -> State MergeState (Maybe SC.Statement)
step sb v1 v2 = trace( "step: " ++ show sb ++ " |-| " ++ show v1 ++ " |-| " ++ show v2 ++ "\n") $ 
 do
  check <- execute sb v1 v2 
  s <- get
  if check 
  then if sb == v1 && sb == v2
       then return $ Just sb
       else if sb == v1
            then return $ Just v2
            else if sb == v2
                 then return $ Just v1
                 else return $ Just $ SC.Sequence v1 v2
  else error "" -- $ "step: Failed with " ++ show sb ++ " |-| " ++ show v1 ++ " |-| " ++ show v2 ++ "\n" ++ show s

-- symbolically runs the statements in the contexts and checks if the final context is valid
execute :: SC.Statement -> SC.Statement -> SC.Statement -> State MergeState Bool
execute base v1 v2 = do
  triplets <- get
  let triplets' = T.trace ("executing with: " ++ show triplets) $ concatMap (run base v1 v2) triplets
  T.trace ("Putting triplets: " ++ show triplets') $ put triplets'
  if all isValid triplets'
  then return True
  else error $ "execute: Failed with \n" ++ show base ++ " |-|\n " ++ show v1 ++ " |-|\n" ++ show v2 ++ "\n" ++ foldr (\t s -> show t ++ "\n" ++ s) "" triplets'
 
run :: SC.Statement -> SC.Statement -> SC.Statement -> Triplet -> [Triplet]
run base v1 v2 (be,v1e,v2e) = 
 -- [(be',v1e',v2e') | be' <- runStat be base, v1e' <- runStat v1e v1, v2e' <- runStat v2e v2]
  let be' = runStat be base
      v1e' = runStat v1e v1
      v2e' = runStat v2e v2
      r = zip3 be' v1e' v2e'
  in (T.trace $ "run: " ++ show base ++"\n" ++ show be' ++ "\n" ++ show v1e' ++ "\n" ++ show v2e' ++ "\n" ++ show r) $ r

-- Symbolic execution engine 
runStat :: Context -> SC.Statement -> [Context]
runStat env s = trace ("running " ++ show s )$ case s of
    SC.Assign _ x expr -> 
      let v = value env expr
      in [M.insert x v env]
    SC.Local  _ x mv -> case mv of
        Nothing -> [env]                                   
        Just v  -> 
          let v' = value env v
          in [M.insert x v' env]
    SC.Sequence s1 s2 -> 
      concatMap (\e -> runStat e s2) $ runStat env s1
    SC.IfThen _ cond s1 ->
      let ncond = SC.UnaryOp CNegOp cond
          tenv  = if env `satisfy` cond 
                  then runStat env s1
                  else []
      in if env `satisfy` ncond 
         then env:tenv
         else if tenv == []
              then error "runStat: IfThen"
              else tenv
    SC.If _ cond s1 s2 -> 
      let ncond = SC.UnaryOp CNegOp cond
          tenv  = if env `satisfy` cond 
                  then runStat env s1
                  else []
          eenv  = if env `satisfy` ncond 
                  then runStat env s2
                  else []
      in tenv ++ eenv
    SC.While _ cond s1 -> 
      let ncond = SC.UnaryOp CNegOp cond
      in if env `satisfy` cond 
         then let env' = runStat env s1 
              in if env `satisfy` ncond -- needs to change
                 then nub $ env:env'
                 else env'
         else [env]
    SC.Return _ expr -> [env]   
    SC.CallS _ "output" args -> [env] 
    SC.CallS _ fn args -> error "undefined: runStat CallS"

satisfy :: Context -> SC.Expression -> Bool
satisfy env expr = 
  case value env expr of
    Top -> True
    V (SC.IntValue 1) -> True
    _ -> False 

value :: Context -> SC.Expression -> ValLat
value env expr = case expr of  
  SC.Call "even" [expr] -> 
    case value env expr of
      V (SC.IntValue x) -> 
        if even x
        then V $ SC.IntValue 1
        else V $ SC.IntValue 0
      _ -> Top
  SC.Call fn args   -> Top
  SC.BinOp op e1 e2 -> apply op (value env e1) (value env e2)
  SC.UnaryOp op e -> apply1 op (value env e) 
  SC.Const v -> V v
  SC.Ident var -> 
    case M.lookup expr env of
        Nothing -> Top -- error $  "value: Ident: " ++ show var ++ "; " ++ show env
        Just e  -> e

apply :: SC.OpCode -> ValLat -> ValLat -> ValLat
apply op Bot _ = Bot
apply op Top _ = Top
apply op _ Bot = Bot
apply op _ Top = Top
apply op v1@(V (SC.IntValue i1)) v2@(V (SC.IntValue i2)) = 
  case op of
    CMulOp -> V $ SC.IntValue $ i1 * i2
    CDivOp -> V $ SC.IntValue $ i1 `div` i2
    CRmdOp -> V $ SC.IntValue $ i1 `mod` i2
    CAddOp -> V $ SC.IntValue $ i1 + i2
    CSubOp -> V $ SC.IntValue $ i1 - i2
    CLeOp  -> V $ SC.IntValue $ bool2int $ i1 < i2
    CGrOp  -> V $ SC.IntValue $ bool2int $ i1 > i2 
    CLeqOp -> V $ SC.IntValue $ bool2int $ i1 <= i2
    CGeqOp -> V $ SC.IntValue $ bool2int $ i1 >= i2
    CEqOp  -> V $ SC.IntValue $ bool2int $ i1 == i2
    CNeqOp -> V $ SC.IntValue $ bool2int $ i1 /= i2
    CLndOp -> V $ SC.IntValue $ bool2int $ (i1 == 1) && (i2 == 1)
    CLorOp ->  V $ SC.IntValue $ bool2int $ (i1 == 1) || (i2 == 1)
    _ -> error "apply: not supported"

apply1 :: SC.UOpCode -> ValLat -> ValLat
apply1 op Top = Top
apply1 op Bot = Bot
apply1 op v@(V (SC.IntValue i)) = 
  case op of
    CPreIncOp  -> error "apply1: 1 not supported" 
    CPreDecOp  -> error "apply1: 2 not supported" 
    CPostIncOp -> error "apply1: 3 not supported" 
    CPostDecOp -> error "apply1: 4 not supported" 
    CAdrOp	   -> error "apply1: 5 not supported"
    CIndOp	   -> error "apply1: 6 not supported"
    CPlusOp	   -> error "apply1: 7 not supported"
    CMinOp	   -> error "apply1: 8 not supported"
    CCompOp	   -> error "apply1: 9 not supported"
    CNegOp	   -> V $ SC.IntValue $ negInt i 

bool2int :: Bool -> Integer
bool2int True = 1
bool2int False = 0

negInt :: Integer -> Integer
negInt 0 = 1
negInt 1 = 0

isValid :: Triplet -> Bool
isValid (envb, env1, env2) = 
    M.foldrWithKey (\x e b -> b && soundCheck envb env2 x e) True env1
 && M.foldrWithKey (\x e b -> b && soundCheck envb env1 x e) True env2

soundCheck :: Context -> Context -> SC.Expression -> ValLat -> Bool 
soundCheck envb envbr var e = 
  case M.lookup var envb of
    Nothing -> case M.lookup var envbr of
      Nothing -> True
      Just e' -> e == e'
    Just eb -> 
      if e == eb
      then True
      else case M.lookup var envbr of
        Nothing -> True
        Just e' -> eb == e' || e == e'
