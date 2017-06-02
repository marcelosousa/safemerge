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

import Language.Java.Syntax 
import Analysis.Java.ClassInfo

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
  deriving (Show,Eq)

data AnnMethodBody = AnnMethodBody (Maybe AnnBlock)
  deriving (Show,Eq)

data AnnBlock = AnnBlock [AnnBlockStmt]
  deriving (Show,Eq)

data AnnConstructorBody = AnnConstructorBody (Maybe ExplConstrInv) [AnnBlockStmt]
  deriving (Show,Eq)

data AnnBlockStmt =
   AnnBlockStmt AnnStmt
 | AnnLocalClass ClassDecl
 | AnnLocalVars [Int] [Modifier] Type [VarDecl]
  deriving (Show,Eq)

data AnnStmt 
  = AnnStmtBlock    [Int] AnnBlock
  | AnnIfThen       [Int] Exp AnnStmt
  | AnnIfThenElse   [Int] Exp AnnStmt AnnStmt
  | AnnWhile        [(Int,Exp)] AnnStmt
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
  deriving (Show,Eq)

data AnnSwitchBlock = AnnSwitchBlock [Int] SwitchLabel [AnnBlockStmt]
  deriving (Show,Eq)

data AnnCatch = AnnCatch [Int] FormalParam AnnBlock
  deriving (Show,Eq)

class Annotate a b where
  toAnn   :: [Int] -> a -> b
  fromAnn :: b -> a

class GetAnnotation a where
  getAnn  :: a -> [Int]


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

instance GetAnnotation AnnMemberDecl where
  getAnn amDecl = case amDecl of
    AnnMethodDecl m ts t i ps e bdy    -> getAnn bdy 
    AnnConstructorDecl m ts i ps e bdy -> getAnn bdy
    _ -> []

instance Annotate MethodBody AnnMethodBody where
  toAnn p (MethodBody mBlock) =
    case mBlock of
      Nothing -> AnnMethodBody Nothing
      Just b  -> AnnMethodBody $ Just $ toAnn p b 
  fromAnn (AnnMethodBody mBlock) = 
    case mBlock of
      Nothing -> MethodBody Nothing
      Just b  -> MethodBody $ Just $ fromAnn b 

instance GetAnnotation AnnMethodBody where
  getAnn (AnnMethodBody mBlock) =
    case mBlock of
      Nothing -> [] 
      Just b  -> getAnn b 

instance Annotate ConstructorBody AnnConstructorBody where
  toAnn p (ConstructorBody mInv blockStmts) = AnnConstructorBody mInv $ map (toAnn p) blockStmts 
  fromAnn (AnnConstructorBody mInv blockStmts) = ConstructorBody mInv $ map fromAnn blockStmts 

instance GetAnnotation AnnConstructorBody where
  getAnn (AnnConstructorBody mInv blockStmts) = getAnn $ head blockStmts 

instance Annotate Block AnnBlock where
  toAnn p (Block stmts) = AnnBlock $ map (toAnn p) stmts 
  fromAnn (AnnBlock stmts) = Block $ map fromAnn stmts 

instance GetAnnotation AnnBlock where
  getAnn  (AnnBlock stmts) = getAnn $ head stmts 

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

instance GetAnnotation AnnBlockStmt where
  getAnn blockStmt = 
    case blockStmt of
      AnnBlockStmt stmt -> getAnn stmt
      AnnLocalVars p mods ty decls -> p 
      _ -> []

instance Annotate Stmt AnnStmt where
  toAnn p stmt = 
    case stmt of
      StmtBlock    b         -> AnnStmtBlock    p (toAnn p b)
      IfThen       e s       -> AnnIfThen       p e (toAnn p s)
      IfThenElse   e s t     -> AnnIfThenElse   p e (toAnn p s) (toAnn p t)
      While        e s       -> AnnWhile        [(_p,e) | _p <- p] (toAnn p s)       
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
      AnnWhile        p s         -> While        (andAST $ snd $ unzip p) (fromAnn s)       
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

andAST :: [Exp] -> Exp
andAST = foldr (\a b -> BinOp a And b) $ Lit $ Boolean True 

instance GetAnnotation AnnStmt where
  getAnn stmt = 
    case stmt of
      AnnStmtBlock    p b         -> p 
      AnnIfThen       p e s       -> p 
      AnnIfThenElse   p e s t     -> p 
      AnnWhile        p s         -> fst $ unzip p 
      AnnBasicFor     p e f g s   -> p 
      AnnEnhancedFor  p m t i e s -> p 
      AnnEmpty        p           -> p 
      AnnExpStmt      p e         -> p 
      AnnAssert       p e f       -> p 
      AnnAssume       p e         -> p 
      AnnSwitch       p e b       -> p 
      AnnDo           p s e       -> p 
      AnnBreak        p i         -> p 
      AnnContinue     p i         -> p 
      AnnReturn       p e         -> p 
      AnnSynchronized p e b       -> p 
      AnnThrow        p e         -> p 
      AnnTry          p b c mb    -> p 
      AnnLabeled      p i s       -> p 
      AnnHole         p           -> p 
      AnnSkip         p           -> p 

instance Annotate SwitchBlock AnnSwitchBlock where
  toAnn p (SwitchBlock l stmts) = AnnSwitchBlock p l (map (toAnn p) stmts)
  fromAnn (AnnSwitchBlock p l stmts) = SwitchBlock l (map fromAnn stmts)

instance GetAnnotation AnnSwitchBlock where
  getAnn (AnnSwitchBlock p l stmts) = p 

instance Annotate Catch AnnCatch where
  toAnn p (Catch fp b) = AnnCatch p fp (toAnn p b) 
  fromAnn (AnnCatch p fp b) = Catch fp (fromAnn b)

instance GetAnnotation AnnCatch where
  getAnn (AnnCatch p fp b) = p 

instance Signature AnnMemberDecl where
  toMemberSig mDecl = case mDecl of
    AnnMethodDecl _ _ _ _ params _ _ -> 
      map (\(FormalParam _ ty _ v) -> (varDeclIdToIdent v,[ty])) params
    AnnConstructorDecl _ _ _ params _ _ -> 
      map (\(FormalParam _ ty _ v) -> (varDeclIdToIdent v,[ty])) params
    AnnFieldDecl _ ty varDecls ->
      map (\(VarDecl v _) -> (varDeclIdToIdent v,[ty])) varDecls
    _ -> []

ann_mth_body :: AnnMemberDecl -> AnnMethodBody
ann_mth_body (AnnMethodDecl _ _ _ _ _ _ b) = b
ann_mth_body m = error $ "ann_mth_body: fatal " ++ show m 

size_of :: AnnBlockStmt -> Int
size_of stmt = case stmt of
  AnnBlockStmt (AnnStmtBlock _ (AnnBlock stmts)) -> foldr (\s r -> size_of s + r) 1 stmts
  _ -> 1 

flatten :: AnnBlockStmt -> [AnnBlockStmt]
flatten stmt = case stmt of
  AnnBlockStmt (AnnStmtBlock _ (AnnBlock stmts)) -> concatMap flatten stmts
  _ -> [stmt] 

add_skip :: Int -> AnnBlockStmt -> [AnnBlockStmt]
add_skip n stmt =
  let pid = getAnn stmt
      skip = AnnBlockStmt $ AnnSkip pid
  in flatten stmt ++ replicate n skip 

is_loop :: AnnBlockStmt -> Bool
is_loop stmt = case stmt of
  AnnBlockStmt (AnnWhile _ _) -> True
  _ -> False 
