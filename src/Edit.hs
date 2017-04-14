{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Edit 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit where

import Analysis.Java.Liff
import Analysis.Java.ClassInfo
import Data.Map (Map)
import Edit.Apply
import Edit.Diff
import Edit.Gen
import Edit.Normalize
import Edit.Print
import Edit.Types
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Debug.Trace as T

type MethInst = (MIdent, MemberDecl, Edit, Edit, Edit, Edit)
type DiffInst = PMergeInst MethInst 

printMethInsts :: [MethInst] -> String
printMethInsts [] = ""
printMethInsts ((mid,mdecl,o,a,b,m):rest) = 
  let header = "Merge Instance for method " ++ show mid ++ "\n"
      mDStr   = prettyPrint mdecl ++ "\n"
      oStr   = "Edit Base:\n" ++ printEdit o ++ "\n"
      aStr   = "Edit A:\n" ++ printEdit a ++ "\n"
      bStr   = "Edit B:\n" ++ printEdit b ++ "\n"
      mStr   = "Edit M:\n" ++ printEdit m ++ "\n"
      restStr = printMethInsts rest
  in header++mDStr++oStr++aStr++bStr++mStr++restStr

diffMethods :: MergeInst -> DiffInst
diffMethods m@MInst{..} = 
  let merges = foldr (diffMethods' m) []  _merges  
  in m { _merges = merges }
 where
  diffMethods' :: MergeInst -> MIdent -> [MethInst] -> [MethInst] 
  diffMethods' m@MInst{..} i@(cls,mth,tys) res =
    case (M.lookup cls _o_info,M.lookup cls _a_info,M.lookup cls _b_info,M.lookup cls _m_info) of
      (Just o, Just a, Just b, Just m) ->
        let o_ms = _cl_meths o
            a_ms = _cl_meths a
            b_ms = _cl_meths b
            m_ms = _cl_meths m
            m_s = (mth,tys)
        in case (M.lookup m_s o_ms,M.lookup m_s a_ms,M.lookup m_s b_ms,M.lookup m_s m_ms) of
          (Just m_o, Just m_a, Just m_b, Just m_m) ->
            let minst = diff4gen_meth i m_o m_a m_b m_m 
            in (minst:res)
          _ -> error $ "diffMethods: can't find info for method " ++ show m_s 
      _ -> error $ "diffMethods: can't find info for class " ++ show cls

diff4gen_meth :: MIdent -> MemberDecl -> MemberDecl -> MemberDecl -> MemberDecl -> MethInst
diff4gen_meth ident o a b m =
  let (no, eo, ea) = edit_member_gen o a 
      (nno, eab)   = gen_edit_member no b [eo,ea]
  in case gen_edit_member nno m eab of
      (fo,[[]]) -> (ident, fo, [], [], [], [])
      (fo, [e_o,e_a,e_b,e_m]) ->
        let (n_fo,[n_e_o,n_e_a,n_e_b,n_e_m]) = opt_holes fo [e_o,e_a,e_b,e_m] 
            _o = fst $ apply_edit_member fo e_o
            _a = fst $ apply_edit_member fo e_a
            _b = fst $ apply_edit_member fo e_b
            _m = fst $ apply_edit_member fo e_m
            res = (ident, fo, e_o, e_a, e_b, e_m)
            nres = (ident,n_fo, n_e_o, n_e_a, n_e_b, n_e_m)
        in if o == _o && a == _a && b == _b && m == _m 
           then res 
           else 
             let oStr   = "Edit Base:\n" ++ printEdit e_o ++ "\n"
                 aStr   = "Edit A:\n" ++ printEdit e_a ++ "\n"
                 bStr   = "Edit B:\n" ++ printEdit e_b ++ "\n"
                 mStr   = "Edit M:\n" ++ printEdit e_m ++ "\n"
             in error $ "diff4gen_meth: bug in the edit script generation\n" ++ prettyPrint fo ++ "\n" ++ prettyPrint o ++ oStr -- ++ aStr ++ bStr ++ mStr

gen_edit_member :: MemberDecl -> MemberDecl -> [Edit] -> (MemberDecl, [Edit])
gen_edit_member p1 p2 eis =
  let (p,e1,e2) = edit_member_gen p1 p2
      -- (p',e2,e1') = edit_gen p2 p1
      -- assertions: p == p', e1 == e1', e2 == e2', |e1| == |e2'|
      e = zip e1 e2
      -- need to update the others
      (_,eis') = foldl (gen_edit_aux eis) (0,[]) e
  in (p,eis' ++ [e2])
 
diff4gen :: Program -> Program -> Program -> Program -> (Program, Edit, Edit, Edit, Edit)
diff4gen o a b m = 
  let (no, eo, ea) = edit_gen o a 
      (nno, eab)   = gen_edit no b [eo,ea]
  in case gen_edit nno m eab of
      (fo,[[]]) -> (fo, [], [], [], [])
      (fo, [e_o,e_a,e_b,e_m]) -> (fo, e_o, e_a, e_b, e_m)
      (fo, es ) -> error $ show es

gen_edit :: Program -> Program -> [Edit] -> (Program, [Edit])
gen_edit p1 p2 eis =
  let (p,e1,e2) = edit_gen p1 p2
      -- (p',e2,e1') = edit_gen p2 p1
      -- assertions: p == p', e1 == e1', e2 == e2', |e1| == |e2'|
      e = zip e1 e2
      -- need to update the others
      (_,eis') = foldl (gen_edit_aux eis) (0,[]) e
  in (p,eis' ++ [e2])

gen_edit_aux :: [Edit] -> (Int,[Edit]) -> (BlockStmt,BlockStmt) -> (Int,[Edit])
gen_edit_aux eis (i,eis') (e1,e2)
 | e1 == hole = -- T.trace ("gen_edit:\neis = " ++ show eis ++ "\neis' = " ++ show eis' ++ "\n(e1,e2) = " ++ show (e1,e2)) $  
    (i+1,push (map (\e ->  e!!i) eis) eis') 
 | otherwise = (i, push (replicate (length eis) e1) eis')

-- add each x to the ei
push :: [BlockStmt] -> [Edit] -> [Edit]
push xs [] = map (:[]) xs
push xs eis =
  let a = zip xs eis
  in map (\(x,ei) -> ei++[x]) a

-- | Check the soundness of the edit script 
check_edit_soundness :: (Program, Program, Edit) -> Bool
check_edit_soundness (original, holes, edit) = 
  original == (apply_edit holes edit)


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
            in (ph',nh',bs++[b],es')
    _ -> (False,nh,bs++[b],_es) 

opt_holes_stmt ::[Edit] -> (Bool,Int,[Edit]) -> Stmt -> (Bool,Int,Maybe Stmt,[Edit])
opt_holes_stmt es (ph,nh,_es) stmt =
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
      n_stmts = map (\(e,o) -> combineStmt e o) $ zip e_stmts o_stmts
      p_es = map init _es
  in map (\(l,s) -> l ++ [s]) $ zip p_es n_stmts

combineStmt :: BlockStmt -> BlockStmt -> BlockStmt
combineStmt p_bstmt c_bstmt = 
  case p_bstmt of
    BlockStmt p_stmt ->
      case p_stmt of
        StmtBlock (Block l) -> BlockStmt $ StmtBlock $ Block $ l ++ [c_bstmt]
        _ ->  BlockStmt $ StmtBlock $ Block [p_bstmt,c_bstmt] 
    _ -> BlockStmt $ StmtBlock $ Block [p_bstmt,c_bstmt] 
