{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Java.AST
-- Copyright :  (c) 2017 Marcelo Sousa

-- | Annotates each Stmt with [PID]   
-------------------------------------------------------------------------------
module Analysis.Java.AST where

import Edit.Types
import Language.Java.Syntax 
import Analysis.Java.Simplifier 

data AnnMemberDecl
  = AnnFieldDecl [Modifier] Type [VarDecl]
  | AnnMethodDecl [Modifier]
                  [TypeParam]
                  (Maybe Type)
                  Ident
                  [FormalParam]
                  [ExceptionType]
                  AnnMethodBody
  | AnnConstructorDecl [Modifier]
                       [TypeParam]
                       Ident
                       [FormalParam]
                       [ExceptionType]
                       AnnConstructorBody
  | AnnMemberClassDecl ClassDecl
  | AnnMemberInterfaceDecl InterfaceDecl

data AnnMethodBody = AnnMethodBody (Maybe AnnBlock)

data AnnBlock = AnnBlock [AnnBlockStmt]

data AnnConstructorBody = AnnConstructorBody (Maybe ExplConstrInv) [AnnBlockStmt]

data AnnBlockStmt =
   AnnBlockStmt AnnStmt
 | AnnLocalClass ClassDecl
 | AnnLocalVars [Int] [Modifier] Type [VarDecl]

data AnnStmt 
  = AnnStmtBlock    [Int] AnnBlock
  | AnnIfThen       [Int] Exp AnnStmt
  | AnnIfThenElse   [Int] Exp AnnStmt AnnStmt
  | AnnWhile        [Int] Exp AnnStmt
  | AnnBasicFor     [Int] (Maybe ForInit) (Maybe Exp) (Maybe [Exp]) AnnStmt
  | AnnEnhancedFor  [Int] [Modifier] Type Ident Exp AnnStmt
  | AnnEmpty        [Int]
  | AnnExpStmt      [Int] Exp
  | AnnAssert       [Int] Exp (Maybe Exp)
  | AnnAssume       [Int] Exp
  | AnnSwitch       [Int] Exp [AnnSwitchBlock]
  | AnnDo           [Int] AnnStmt Exp
  | AnnBreak        [Int] (Maybe Ident)
  | AnnContinue     [Int] (Maybe Ident)
  | AnnReturn       [Int] (Maybe Exp)
  | AnnSynchronized [Int] Exp AnnBlock
  | AnnThrow        [Int] Exp
  | AnnTry          [Int] AnnBlock [AnnCatch] (Maybe AnnBlock)
  | AnnLabeled      [Int] Ident AnnStmt
  | AnnHole         [Int]
  | AnnSkip         [Int]

data AnnSwitchBlock = AnnSwitchBlock [Int] SwitchLabel [AnnBlockStmt]

data AnnCatch = AnnCatch [Int] FormalParam AnnBlock

class Annotate a b where
  toAnn :: [Int] -> a -> b
  fromAnn :: b -> a

instance Annotate MemberDecl AnnMemberDecl where
  toAnn p mDecl = case mDecl of
    FieldDecl  m t v -> AnnFieldDecl m t v
    MethodDecl m ts t i ps e bdy -> AnnMethodDecl m ts t i ps e (toAnn p bdy)
    ConstructorDecl m ts i ps e bdy -> AnnConstructorDecl m ts i ps e (toAnn p bdy)
    MemberClassDecl c -> AnnMemberClassDecl c
    MemberInterfaceDecl i -> AnnMemberInterfaceDecl i
  fromAnn amDecl = case amDecl of
    AnnFieldDecl  m t v -> FieldDecl m t v
    AnnMethodDecl m ts t i ps e bdy -> MethodDecl m ts t i ps e (fromAnn bdy)
    AnnConstructorDecl m ts i ps e bdy -> ConstructorDecl m ts i ps e (fromAnn bdy)
    AnnMemberClassDecl c -> MemberClassDecl c
    AnnMemberInterfaceDecl i -> MemberInterfaceDecl i

instance Annotate MethodBody AnnMethodBody where
  toAnn p (MethodBody mBlock) =
    case mBlock of
      Nothing -> AnnMethodBody Nothing
      Just b  -> AnnMethodBody $ Just $ toAnn p b 
  fromAnn (AnnMethodBody mBlock) = 
    case mBlock of
      Nothing -> MethodBody Nothing
      Just b  -> MethodBody $ Just $ fromAnn b 
  
instance Annotate ConstructorBody AnnConstructorBody where
  toAnn p (ConstructorBody mInv blockStmts) = AnnConstructorBody mInv $ map (toAnn p) blockStmts 
  fromAnn (AnnConstructorBody mInv blockStmts) = ConstructorBody mInv $ map fromAnn blockStmts 

instance Annotate Block AnnBlock where
  toAnn p (Block stmts) = AnnBlock $ map (toAnn p) stmts 
  fromAnn (AnnBlock stmts) = Block $ map fromAnn stmts 

instance Annotate BlockStmt AnnBlockStmt where
  toAnn p blockStmt =  
    case blockStmt of
      BlockStmt stmt -> AnnBlockStmt $ toAnn p stmt
      LocalClass cl  -> AnnLocalClass cl
      LocalVars mods ty decls -> AnnLocalVars p mods ty decls
  fromAnn blockStmt = 
    case blockStmt of
      AnnBlockStmt stmt -> BlockStmt $ fromAnn stmt
      AnnLocalClass cl  -> LocalClass cl
      AnnLocalVars p mods ty decls -> LocalVars mods ty decls

instance Annotate Stmt AnnStmt where
  toAnn p stmt = 
    case stmt of
      StmtBlock    b         -> AnnStmtBlock    p (toAnn p b)
      IfThen       e s       -> AnnIfThen       p e (toAnn p s)
      IfThenElse   e s t     -> AnnIfThenElse   p e (toAnn p s) (toAnn p t)
      While        e s       -> AnnWhile        p e (toAnn p s)       
      BasicFor     e f g s   -> AnnBasicFor     p e f g (toAnn p s) 
      EnhancedFor  m t i e s -> AnnEnhancedFor  p m t i e (toAnn p s) 
      Empty                  -> AnnEmpty        p
      ExpStmt      e         -> AnnExpStmt      p e
      Assert       e f       -> AnnAssert       p e f
      Assume       e         -> AnnAssume       p e
      Switch       e b       -> AnnSwitch       p e (map (toAnn p) b) 
      Do           s e       -> AnnDo           p (toAnn p s) e
      Break        i         -> AnnBreak        p i
      Continue     i         -> AnnContinue     p i
      Return       e         -> AnnReturn       p e
      Synchronized e b       -> AnnSynchronized p e (toAnn p b)
      Throw        e         -> AnnThrow        p e 
      Try          b c mb    -> AnnTry          p (toAnn p b) (map (toAnn p) c) (fmap (toAnn p) mb) 
      Labeled      i s       -> AnnLabeled      p i (toAnn p s) 
      Hole                   -> AnnHole         p
      Skip                   -> AnnSkip         p
  fromAnn stmt = 
    case stmt of
      AnnStmtBlock    p b         -> StmtBlock    (fromAnn b)
      AnnIfThen       p e s       -> IfThen       e (fromAnn s)
      AnnIfThenElse   p e s t     -> IfThenElse   e (fromAnn s) (fromAnn t)
      AnnWhile        p e s       -> While        e (fromAnn s)       
      AnnBasicFor     p e f g s   -> BasicFor     e f g (fromAnn s) 
      AnnEnhancedFor  p m t i e s -> EnhancedFor  m t i e (fromAnn s) 
      AnnEmpty        p           -> Empty        
      AnnExpStmt      p e         -> ExpStmt      e
      AnnAssert       p e f       -> Assert       e f
      AnnAssume       p e         -> Assume       e
      AnnSwitch       p e b       -> Switch       e (map (fromAnn) b) 
      AnnDo           p s e       -> Do           (fromAnn s) e
      AnnBreak        p i         -> Break        i
      AnnContinue     p i         -> Continue     i
      AnnReturn       p e         -> Return       e
      AnnSynchronized p e b       -> Synchronized e (fromAnn b)
      AnnThrow        p e         -> Throw        e 
      AnnTry          p b c mb    -> Try          (fromAnn b) (map (fromAnn) c) (fmap (fromAnn) mb) 
      AnnLabeled      p i s       -> Labeled      i (fromAnn s) 
      AnnHole         p           -> Hole         
      AnnSkip         p           -> Skip         

instance Annotate SwitchBlock AnnSwitchBlock where
  toAnn p (SwitchBlock l stmts) = AnnSwitchBlock p l (map (toAnn p) stmts)
  fromAnn (AnnSwitchBlock p l stmts) = SwitchBlock l (map fromAnn stmts)

instance Annotate Catch AnnCatch where
  toAnn p (Catch fp b) = AnnCatch p fp (toAnn p b) 
  fromAnn (AnnCatch p fp b) = Catch fp (fromAnn b)
