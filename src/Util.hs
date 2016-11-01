{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Util where

import Data.List
import qualified Data.Map as M
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import Language.SMTLib2.Base
import Language.SMTLib2.Builder
import Language.SMTLib2.Printer
import Printer
import Types

-- Class to retrieve all the function signatures from a.
-- A Function Signature is a pair (Function Name, Arity)
type FunctionSig = (Var, Int)

class GetFunctionSig a where
  getFunctionSig :: a -> [FunctionSig]

instance GetFunctionSig ProdProgram where
  getFunctionSig (a, prog, b) =
    nub $ M.fold (\l r -> (concatMap getFunctionSig l) ++ r) [] prog

instance GetFunctionSig (Stat, [Label]) where
  getFunctionSig = (getFunctionSig . fst)

instance GetFunctionSig Stat where
  getFunctionSig s = case s of
    Skip -> []
    Assume e -> getFunctionSig e
    Assign v e -> getFunctionSig e

instance GetFunctionSig Expr where
  getFunctionSig e = case e of
    Op lhs _ rhs -> getFunctionSig lhs ++ getFunctionSig rhs
    C _ -> []
    V v -> []
    F v es -> (v, length es):(concatMap getFunctionSig es)
    A v e -> getFunctionSig e

-- Class to retrieve all variables from a.
-- A Variable can be a simple variable or an array variable.
type Variables = (Vars, Vars)

(-++-) :: Variables -> Variables -> Variables
(-++-) (a1,b1) (a2,b2) = (a1++a2, b1++b2)

combineVars :: [Variables] -> Variables
combineVars = foldr (-++-) ([],[])

nubVars :: Variables -> Variables
nubVars (a,b) = (nub a, nub b)

class GetVariable a where
  getVariable :: a -> Variables

instance GetVariable ProdProgram where
  getVariable (a, prog, b) =
    nubVars $ M.fold (\l r -> (combineVars $ map getVariable l) -++- r) ([],[]) prog

instance GetVariable (Stat, [Label]) where
  getVariable = (getVariable . fst)

instance GetVariable Stat where
  getVariable s = case s of
    Skip -> ([],[])
    Assume e -> getVariable e
    Assign v e -> getVariable v -++- getVariable e

instance GetVariable Lhs where
  getVariable lhs = case lhs of
    LhsVar v -> ([v],[])
    LhsArray v e -> ([],[v]) -++- getVariable e

instance GetVariable Expr where
  getVariable e = case e of
    Op lhs _ rhs -> getVariable lhs -++- getVariable rhs
    C _ -> ([],[])
    V v -> ([v],[])
    F v es -> combineVars $ map getVariable es
    A v e -> ([],[v]) -++- getVariable e
