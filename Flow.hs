module Flow where

import Control.Monad.State
import Language

type Flow = [(Int,Int)]
type FlowState = [Int]
startState = [0]

computeFlow :: Program -> Flow
computeFlow (Program (decls, defs)) = 
    concatMap (\s -> evalState (flow' s) startState) defs

flow' :: Definition -> State FlowState Flow
flow' (FunctionDef pc _ _ s) = do
    put [pc]
    flow s
    
flow :: Statement -> State FlowState Flow
flow s = do 
  i <- get
  let pc = getPC s
      f = map (\j -> (j,pc)) i
  case s of
    Sequence s1 s2 -> do
      fs1 <- flow s1
      fs2 <- flow s2
      return $ fs1 ++ fs2
    If _ _ s1 s2 -> do 
      put [pc]
      fs1 <- flow s1
      i1 <- get
      put [pc]
      fs2 <- flow s2
      i2 <- get
      put $ i1++i2
      return $ f ++ fs1 ++ fs2
    IfThen _ _ s1 -> do
      put [pc]
      fs1 <- flow s1
      i1 <- get
      put (pc:i1)
      return $ f ++ fs1
    While _ _ s1 -> do
      put [pc]
      fs1 <- flow s1
      i1 <- get
      put (pc:i1)
      return $ f ++ fs1
    _ -> do
      put [pc]
      return f

getPC :: Statement -> PC
getPC s = case s of
    Assign pc _ _ -> pc
    Local  pc _   -> pc
    Sequence _ _  -> undefined
    IfThen pc _ _ -> pc
    If pc _ _ _   -> pc
    While pc _ _  -> pc
    Return pc _   -> pc
    CallS pc _ _  -> pc
