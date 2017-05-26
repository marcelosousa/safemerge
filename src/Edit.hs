{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Edit 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit where

-- import Analysis.Java.Liff
import Analysis.Java.ClassInfo
import Data.Map (Map)
import Edit.Apply
import Edit.Diff
import Edit.Gen
import Edit.Normalize
import Edit.Opt
import Edit.Print
import Edit.Types
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Debug.Trace as T

type MethInst = (MIdent, MemberDecl, Edit, Edit, Edit, Edit)
type DiffInst = PMergeInst MethInst 
type MergeInst = PMergeInst MIdent

data PMergeInst a = MInst
 {
   _o_info :: ClassInfo
 , _a_info :: ClassInfo
 , _b_info :: ClassInfo
 , _m_info :: ClassInfo
 , _merges :: [a]
 }
 deriving (Ord,Eq)

instance Show a => Show (PMergeInst a) where
  show m@MInst{..} = "Merges: " ++show _merges

printMethInsts :: [MethInst] -> String
printMethInsts [] = ""
printMethInsts ((mid,mdecl,o,a,b,m):rest) = 
  let header  = "Merge Instance for method " ++ show mid ++ "\n"
      mDStr   = prettyPrint mdecl ++ "\n"
      oStr    = "Edit Base:\n" ++ printEdit o ++ "\n"
      aStr    = "Edit A:\n"    ++ printEdit a ++ "\n"
      bStr    = "Edit B:\n"    ++ printEdit b ++ "\n"
      mStr    = "Edit M:\n"    ++ printEdit m ++ "\n"
      restStr = printMethInsts rest
  in header++mDStr++oStr++aStr++bStr++mStr++restStr

-- | Computes a 4-way diff of Programs
diff4gen :: Program -> Program -> Program -> Program -> (Program, Edit, Edit, Edit, Edit)
diff4gen o a b m = 
  let (no, eo, ea) = edit_gen o a 
      (nno, eab)   = gen_edit edit_gen no b [eo,ea]
  in case gen_edit edit_gen nno m eab of
      (fo,[[]]) -> (fo, [], [], [], [])
      (fo, [e_o,e_a,e_b,e_m]) -> (fo, e_o, e_a, e_b, e_m)
      (fo, es ) -> error $ show es

-- | Converts a MergeInstance into a DiffInstance;
--   One of them needs to go!
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

-- | Computes a 4-way diff of Member Declarations (Methods)
diff4gen_meth :: MIdent -> MemberDecl -> MemberDecl -> MemberDecl -> MemberDecl -> MethInst
diff4gen_meth ident o a b m =
  let (no, eo, ea) = edit_member_gen o a 
      (nno, eab)   = -- T.trace ("edit_member:\n" ++ prettyPrint no ++ "\n" ++ printEdit eo ++ "\n" ++ printEdit ea) $ 
        gen_edit edit_member_gen no b [eo,ea]
  in -- T.trace ("edit_member II:\n" ++ prettyPrint nno ++ "\n" ++ unlines (map printEdit eab)) $ 
     case gen_edit edit_member_gen nno m eab of
      (fo,[[]]) -> (ident, fo, [], [], [], [])
      (fo, [e_o,e_a,e_b,e_m]) ->
        let -- (n_fo,[n_e_o,n_e_a,n_e_b,n_e_m]) = opt_holes fo [e_o,e_a,e_b,e_m] 
            _o = fst $ apply_edit_member fo e_o
            _a = fst $ apply_edit_member fo e_a
            _b = fst $ apply_edit_member fo e_b
            _m = fst $ apply_edit_member fo e_m
            -- nres = (ident,n_fo, n_e_o, n_e_a, n_e_b, n_e_m)
            nres = (ident,fo, e_o, e_a, e_b, e_m)
            checks = [o == _o, a == _a, b == _b, m == _m] 
        in if all id checks 
           then nres 
           else 
             let oStr = "Edit Base:\n" ++ printEdit e_o ++ "\n"
                 aStr = "Edit A:\n"    ++ printEdit e_a ++ "\n"
                 bStr = "Edit B:\n"    ++ printEdit e_b ++ "\n"
                 mStr = "Edit M:\n"    ++ printEdit e_m ++ "\n"
              -- in T.trace  ("diff4gen_meth: bug in the edit script generation\n" ++ prettyPrint fo ++ "\n" ++ prettyPrint o ++ oStr ++ aStr ++ bStr ++ mStr) $ (ident,fo, [],[],[],[])
              -- in error ("diff4gen_meth: bug in the edit script generation\n" ++ show checks ++ "\n" ++ prettyPrint o ++ "\n" ++ prettyPrint fo ++ "\n" ++ prettyPrint _o ++ "\n" ++ oStr ++ "\n" ++ show o ++ "\n" ++ show _o) 
              in T.trace ("diff4gen_meth: unusable edit scripts " ++ show checks)  $ (ident,fo, [],[],[],[])
      (fo, xs) -> T.trace ("diff4: strange result: " ++ show (length xs)) $ (ident,fo, [],[],[],[])

gen_edit :: (a -> a -> (a,Edit,Edit)) -> a -> a -> [Edit] -> (a, [Edit])
gen_edit f p1 p2 eis =
  let (p,e1,e2) = f p1 p2
      -- (p',e2,e1') = edit_gen p2 p1
      -- assertions: p == p', e1 == e1', e2 == e2', |e1| == |e2'|
      e = zip e1 e2
      -- need to update the others
      (_,eis') = foldl (gen_edit_aux eis) (0,[]) e
  in (p,eis' ++ [e2])
 
gen_edit_aux :: [Edit] -> (Int,[Edit]) -> (SEdit,SEdit) -> (Int,[Edit])
gen_edit_aux eis (i,eis') ((e1,s1),(e2,s2))
 | e1 == hole = (i+1,push (map (\e ->  e!!i) eis) eis') 
 | otherwise  = (i, push (replicate (length eis) (e1,s1)) eis')

-- | Check the soundness of the edit script 
check_edit_soundness :: (Program, Program, Edit) -> Bool
check_edit_soundness (original, holes, edit) = 
  original == (apply_edit holes edit)

