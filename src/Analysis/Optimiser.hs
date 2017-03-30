{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Optimiser
-- Copyright :  (c) 2017 Marcelo Sousa
--
-- Uses the dependence analysis to annotate 
-- blocks of statements in program with holes
-- with the read & write sets that will be used
-- in the encoding to model these with uninterpreted functions 
-------------------------------------------------------------------------------
module Analysis.Optimiser where

import Analysis.Dependence
import Analysis.Java.ClassInfo
import Analysis.Java.Liff
import Control.Monad.State.Lazy
import Data.List
import Data.Map (Map)
import Data.Set (Set)
import Debug.Trace as T
import Edit.Types
import Edit
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Data.Set as S

-- OptLevel: turns the block level optimisation on
data OptLevel = OptBlock | OptSimple

type OptMethInst = (MIdent, MemberDecl, AnnMemberDecl, OptLevel, Edit, Edit, Edit, Edit) 
type VerInst = PMergeInst OptMethInst

data AnnMemberDecl =
    AnnOther
  | AnnMethodDecl [Modifier]
                  [TypeParam]
                  (Maybe Type)
                  Ident
                  [FormalParam]
                  [ExceptionType]
                  AnnBody
  | AnnConstructorDecl [Modifier]
                       [TypeParam]
                       Ident
                       [FormalParam]
                       [ExceptionType]
                       (Maybe ExplConstrInv)
                       AnnBody 

-- Sequential composition
type AnnBody = [AnnBlock]
 
-- Annotated Block
data AnnBlock = ABlock 
 {
   _ab_code :: Block
 , _ab_tag  :: Bool  -- True if it has a hole, False otherwise
 , _ab_ann  :: DepMap
 }

-- flatten body: remove StmtBlock from the stmt
flatten_body :: [BlockStmt] -> [BlockStmt]
flatten_body = undefined 

-- main optimisation function: converts the top level
-- block into the annotated body
toAnnBody :: [BlockStmt] -> AnnBody
toAnnBody stmts =
  let flatten_stmts = flatten_body stmts
      blocks = partitionBody flatten_stmts
  in map toAnnBlock blocks

partitionBody :: [BlockStmt] -> [([BlockStmt],Bool)]
partitionBody = undefined

-- Here I need more information since I might have to call
-- the interprocedural analysis
toAnnBlock :: ([BlockStmt],Bool) -> AnnBlock
toAnnBlock = undefined

-- converts a MemberDecl with holes into an annotated MemberDecl
-- ready to be passed to the verifier
toAnnMemberDecl :: OptLevel -> MemberDecl -> AnnMemberDecl
toAnnMemberDecl opt_level mDecl = case mDecl of
  MethodDecl mod tys rty id parms exTy (MethodBody mBody) ->
    let annBlock = case mBody of 
                     Nothing -> []
                     Just (Block b)  -> toAnnBody b
    in AnnMethodDecl mod tys rty id parms exTy annBlock 
  ConstructorDecl mod tys id parms exTy (ConstructorBody mInv cBody) ->
    AnnConstructorDecl mod tys id parms exTy mInv $ toAnnBody cBody 

-- Receives a program with holes 
-- and partition its in a list 
-- of statements that are annotated
optMethods :: OptLevel -> DiffInst -> VerInst
optMethods opt_level d = 
  let merges = map (optMethod opt_level) $ _merges d 
  in d { _merges = merges }

optMethod :: OptLevel -> MethInst -> OptMethInst
optMethod opt_level (id,meth,o,a,b,m) =
  let ann_meth = toAnnMemberDecl opt_level meth
  in (id,meth,ann_meth,opt_level,o,a,b,m) 
