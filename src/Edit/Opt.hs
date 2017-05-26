{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Edit.Opt 
-- Copyright :  (c) 2017 Marcelo Sousa
--  Collapse consecutive holes
-------------------------------------------------------------------------------
module Edit.Opt where

import Edit.Types
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Debug.Trace as T

-- | Collapse consecutive holes
opt_holes :: MemberDecl -> [Edit] -> (MemberDecl, [Edit]) 
opt_holes pHoles es = case pHoles of
  MethodDecl mods tys ty id fpars ex (MethodBody mbody) ->
    case mbody of
      Nothing -> (pHoles,es)
      Just (Block b)  -> 
        let (_,_,b',es') = opt_holes_block es (False,0,[]) b 
        in (MethodDecl mods tys ty id fpars ex (MethodBody $ Just $ Block b'),es') 
  ConstructorDecl mods tys id fpars ex (ConstructorBody inv b) -> 
    let (_,_,b',es') = opt_holes_block es (False,0,[]) b 
    in (ConstructorDecl mods tys id fpars ex (ConstructorBody inv b'),es') 
  _ -> (pHoles,es)

opt_holes_block :: [Edit] -> (Bool,Int,[Edit]) -> [BlockStmt] -> (Bool,Int,[BlockStmt],[Edit])
opt_holes_block es (pH,nH,_es) b = foldl (opt_holes_bstmt es) (pH,nH,[],_es) b

opt_holes_bstmt :: [Edit] -> (Bool,Int,[BlockStmt],[Edit]) -> BlockStmt -> (Bool,Int,[BlockStmt],[Edit])
opt_holes_bstmt es (ph,nh,bs,_es) b =
  case b of
    BlockStmt stmt -> 
      let (ph',nh',mstmt,es') = opt_holes_stmt es (ph,nh,_es) stmt
      in case mstmt of
          Nothing -> (ph',nh',bs,es')
          Just stmt' ->
            let b' = BlockStmt stmt'
            in (ph',nh',bs++[b'],es')
    _ -> (False,nh,bs++[b],_es) 

opt_holes_stmt ::[Edit] -> (Bool,Int,[Edit]) -> Stmt -> (Bool,Int,Maybe Stmt,[Edit])
opt_holes_stmt es (ph,nh,_es) stmt = -- T.trace ("opt_holes_stmt: " ++ prettyPrint stmt) $ 
 case stmt of
   StmtBlock (Block block) ->
     let (ph',nh',b',es') = opt_holes_block es (ph,nh,_es) block
     in if null b'
        then (ph',nh',Nothing,es')
        else (ph',nh',Just $ StmtBlock $ Block b',es') 
   IfThen cond sThen ->
     let (_,nh',mThen,es') = opt_holes_stmt es (False,nh,_es) sThen
     in case mThen of
          Nothing -> error $ "opt_holes_stmt: IfThen optimisation" 
          Just _Then -> (False,nh',Just $ IfThen cond _Then,es')
   IfThenElse cond sThen sElse -> 
     let (_,nh',mThen,es') = opt_holes_stmt es (False,nh,_es) sThen
         (_,nh'',mElse,es'') = opt_holes_stmt es (False,nh',es') sElse
     in case (mThen,mElse) of
         (Just _Then, Just _Else) -> (False,nh'',Just $ IfThenElse cond _Then _Else,es'')
         _ -> error $ "opt_holes_stmt: IfThenElse optimisation"
   While cond body -> 
     let (_,nh',mBody,es') = opt_holes_stmt es (False,nh,_es) body 
     in case mBody of
          Nothing -> error $ "opt_holes_stmt: While optimisation" 
          Just _body -> (False,nh',Just $ While cond _body, es')
   Do body cond -> 
     let (_,nh',mBody,es') = opt_holes_stmt es (False,nh,_es) body 
     in case mBody of
          Nothing -> error $ "opt_holes_stmt: Do optimisation" 
          Just _body -> (False,nh',Just $ Do _body cond, es')
   BasicFor fInit cond fEnd body ->
     let (_,nh',mBody,es') = opt_holes_stmt es (False,nh,_es) body 
     in case mBody of
          Nothing -> error $ "opt_holes_stmt: BasicFor optimisation" 
          Just _body -> (False,nh',Just $ BasicFor fInit cond fEnd _body, es')
   EnhancedFor mods ty i cond body -> 
     let (_,nh',mBody,es') = opt_holes_stmt es (False,nh,_es) body 
     in case mBody of
          Nothing -> error $ "opt_holes_stmt: EnhancedFor optimisation" 
          Just _body -> (False,nh',Just $ EnhancedFor mods ty i cond _body, es')
   Switch cond body -> (False,nh,Just stmt,_es) 
   Labeled i body -> 
     let (_,nh',mBody,es') = opt_holes_stmt es (False,nh,_es) body 
     in case mBody of
          Nothing -> error $ "opt_holes_stmt: Labeled optimisation" 
          Just _body -> (False,nh',Just $ Labeled i _body, es')
   Hole -> 
     if ph
     then (True,nh+1,Nothing, addToEdit nh es _es) 
     else (True,nh+1,Just stmt, push (map (\e -> e!!nh) es) _es) 
   _ -> (False,nh,Just stmt,_es)

addToEdit :: Int -> [Edit] -> [Edit] -> [Edit]
addToEdit nh es _es =
  let o_stmts = map (\e -> e !! nh) es 
      e_stmts = map last _es
      n_stmts = map (uncurry joinEdit) $ zip e_stmts o_stmts
      p_es = map init _es
  in map (\(l,s) -> l ++ [s]) $ zip p_es n_stmts

joinEdit :: (BlockStmt,[Scope]) -> (BlockStmt,[Scope]) -> (BlockStmt,[Scope])
joinEdit (p_bstmt,sp) (c_bstmt,sc) =
 let s = sp ++ sc 
 in case p_bstmt of
     BlockStmt p_stmt ->
      case p_stmt of
       StmtBlock (Block l) -> (BlockStmt $ StmtBlock $ Block $ l ++ [c_bstmt],s)
       _ ->  (BlockStmt $ StmtBlock $ Block [p_bstmt,c_bstmt],s) 
     _ -> (BlockStmt $ StmtBlock $ Block [p_bstmt,c_bstmt],s) 
