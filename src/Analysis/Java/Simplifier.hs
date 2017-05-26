{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Java.Simplifier
-- Copyright :  (c) 2017 Marcelo Sousa

-- | Simplifies a method body and statements 
-------------------------------------------------------------------------------
module Analysis.Java.Simplifier where

import Edit.Types
import Language.Java.Syntax

simplifyMDecl :: MemberDecl -> MemberDecl 
simplifyMDecl m = case m of
  MethodDecl mods tys ty mId parms exTy bdy ->
    MethodDecl mods tys ty mId parms exTy (simplifyMBody bdy)
  ConstructorDecl mods tys mId parms exTy bdy -> 
    ConstructorDecl mods tys mId parms exTy (simplifyCBody bdy)
  _ -> m

simplifyMBody :: MethodBody -> MethodBody
simplifyMBody (MethodBody m) = case m of
  Nothing -> MethodBody $ Nothing
  Just b  -> MethodBody $ Just $ simplifyBlock b

simplifyBlock :: Block -> Block
simplifyBlock (Block stmts) = Block $ map simplifyBStmt stmts

simplifyCBody :: ConstructorBody -> ConstructorBody
simplifyCBody (ConstructorBody m stmts) =
  ConstructorBody m $ map simplifyBStmt stmts

simplifyEdit :: Edit -> Edit
simplifyEdit = map (\(stmt,s) -> (simplifyBStmt stmt,s))

simplifyBStmt :: BlockStmt -> BlockStmt
simplifyBStmt bstmt = case bstmt of
  BlockStmt s -> BlockStmt $ simplifyStmt s
  _ -> bstmt

simplifyStmt :: Stmt -> Stmt 
simplifyStmt stmt = case stmt of
  StmtBlock b -> StmtBlock $ simplifyBlock b
  IfThen c t  -> IfThenElse (simplifyExp c) (simplifyStmt t) Empty
  IfThenElse c t e ->
    IfThenElse (simplifyExp c) (simplifyStmt t) (simplifyStmt e)
  While cond stmt -> While (simplifyExp cond) (simplifyStmt stmt)
  BasicFor mInit cond end bdy ->
    -- i :: Stmt
    let i = case mInit of
          Nothing -> Empty
          Just init -> case init of
            ForLocalVars mods tys vars -> StmtBlock $ Block [LocalVars mods tys vars]
            ForInitExps exps -> 
              StmtBlock (Block $ map (BlockStmt . ExpStmt . simplifyExp) exps)
    -- c :: Exp
        c = case cond of
          Nothing -> Lit $ Boolean True
          Just e  -> simplifyExp e
    -- bdy :: Stmt
        b = let bdy' = simplifyStmt bdy
            in case end of
                Nothing -> bdy' 
                Just exps ->
                  let _bdy = BlockStmt bdy'
                  in StmtBlock (Block $ _bdy:(map (BlockStmt . ExpStmt . simplifyExp) exps))
    in StmtBlock $ Block [BlockStmt i, BlockStmt $ While c b] 
  EnhancedFor mods ty i e bdy ->
    let idI = Ident "wiz_i" 
        varI = VarDecl (VarId idI) $ Just $ InitExp $ Lit $ Int 0 
        nameI = ExpName $ Name [idI]
        varDeclI = LocalVars [] (PrimType IntT) [varI]
        -- varDecl for i
        varK = VarDecl (VarId i) $ Just $ InitExp $ ArrayAccess $ ArrayIndex exp [nameI] 
        varDeclK = LocalVars mods ty [varK]
        -- condition for the while
        exp = simplifyExp e
        cond = BinOp nameI LThan $ MethodInv $ PrimaryMethodCall exp [] (Ident "length") [] 
        -- create the increment
        inc = ExpStmt $ BinOp nameI Add $ Lit $ Int 1 
        body = simplifyStmt bdy
        body' = StmtBlock $ Block [BlockStmt body, BlockStmt inc] 
    in StmtBlock $ Block [varDeclI, varDeclK, BlockStmt $ While cond body'] 
  Empty -> Empty
  ExpStmt exp -> ExpStmt $ simplifyExp exp
  Do bdy c ->
    let body = simplifyStmt bdy
        cond = simplifyExp c
    in StmtBlock $ Block $ map BlockStmt [body, While cond body]  
  _ -> stmt

simplifyExp :: Exp -> Exp
simplifyExp exp = 
  let one = Lit $ Int 1
  in case exp of
    PostIncrement e ->
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Add one
    PreIncrement  e -> 
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Add one
    PostDecrement e ->
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Sub one
    PreDecrement  e -> 
      let lhs = expToLhs e
      in  Assign lhs EqualA $ BinOp e Sub one
    Assign lhs op rhs -> 
      case op of
        EqualA -> exp
        _ ->
          let e = lhsToExp lhs 
              op' = toOp op
          in Assign lhs EqualA $ BinOp e op' rhs
    _ -> exp

toOp :: AssignOp -> Op
toOp op = case op of
  MultA -> Mult
  DivA  -> Div
  RemA  -> Rem
  AddA  -> Add
  SubA  -> Sub
  LShiftA -> LShift
  RShiftA -> RShift
  AndA -> And
  XorA -> Xor
  OrA  -> Or

lhsToExp :: Lhs -> Exp
lhsToExp lhs = case lhs of
  NameLhs n -> ExpName n
  FieldLhs f -> FieldAccess f
  ArrayLhs ai -> ArrayAccess ai

expToLhs :: Exp -> Lhs
expToLhs e = case e of
  ExpName n -> NameLhs n
  _ -> error $ "expToLhs: Unsupported " ++ show e
