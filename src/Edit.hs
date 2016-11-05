{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Edit 
-- Copyright :  (c) 2016 Marcelo Sousa
-------------------------------------------------------------------------------
module Edit where

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
 | e2 == skip || e1 == hole = (i+1,push (map (\e -> e!!i) eis) eis') 
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
