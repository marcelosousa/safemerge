{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.API
-- Copyright :  (c) 2017 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.API where

import Analysis.Debug
import Analysis.Dependence
import Analysis.Java.AST
import Analysis.Java.ClassInfo
import Analysis.Pretty
import Analysis.Types
import Analysis.Util
import Control.Monad.ST
import Control.Monad.State.Strict hiding (join)
import Data.List
import Data.Map (Map)
import Edit.Types
import Language.Java.Syntax
import Z3.Monad hiding (Params)
import qualified Data.Map as M

-- | Initial Function Map
initial_FuncMap :: Z3 FunctMap
initial_FuncMap = do
  iSort  <- mkIntSort
  iArray <- mkArraySort iSort iSort
  fn     <- mkFreshFuncDecl "size" [iArray] iSort
  return $ M.singleton (Ident "size",0) (fn, M.empty)

-- This function does the heavylifting in the join of the ssamap
--   It receives the original environment, one identifier that is 
--   shared in the new 2 environments and checks whether their 
--   keys is the same in which case it generates Nothing
--       otherwise it reports which versions need to updated
type DiffSSA = [(VId,Int,Sort,VarType,[(String,Int,Sort)])]

diffSSAMap :: SSAMap -> Ident -> SSAVer -> SSAVer -> DiffSSA 
diffSSAMap orig m e1 e2 = 
  case M.lookup m orig of
    Nothing -> [] -- error $ "diffSSAMap: scoping issue for " ++ show m 
    Just v  -> 
      if e1 == v && e2 == v
      then [] 
      else M.foldWithKey (diffSSAVar e1 e2) [] v 
 where
  diffSSAVar :: SSAVer -> SSAVer -> VId -> SSAVar -> DiffSSA -> DiffSSA 
  diffSSAVar e1 e2 vId v_o rest = 
    case (M.lookup vId e1, M.lookup vId e2) of
      (Just v1, Just v2) ->
        let mod = M.foldWithKey (diffSSAVarModel (_v_mod v1) (_v_mod v2)) [] (_v_mod v_o)
            k_o = _v_cnt v_o
            k_1 = _v_cnt v1
            k_2 = _v_cnt v2
        in if k_o == k_1 && k_o == k_2 && null mod 
           then rest
           else (vId,maximum [k_1,k_2] + 1,_v_typ v_o,_v_mty v_o,mod):rest 
      _ -> error $ "diffSSAVarModel: version " ++ show vId
  
  diffSSAVarModel :: SSAVarModel -> SSAVarModel -> String -> (AST,Sort,Int) -> [(String,Int,Sort)] -> [(String,Int,Sort)]
  diffSSAVarModel m1 m2 i (ast,sort,k0) rest = 
    case (M.lookup i m1, M.lookup i m2) of
      (Just (_,_,v1), Just (_,_,v2)) ->
        if k0 == v1 && k0 == v2
        then rest
        else ((i,maximum [v1,v2] + 1,sort):rest)
      _ -> error $ "diffVarModel: ident " ++ i

-- | Converts the result of DiffSSA to a partial SSA Map
toSSAMap :: Map Ident DiffSSA -> EnvOp SSAMap
toSSAMap m = do 
  let m' = M.toList m 
  foldM toSSAVer M.empty m'
 where
   toSSAVer :: SSAMap -> (Ident,DiffSSA) -> EnvOp SSAMap 
   toSSAVer r (Ident str,[]) = return r
   toSSAVer r (Ident str,xs) = do
     el <- foldM (toSSAVar str) M.empty xs
     return $ M.insert (Ident str) el r

   toSSAVar::String -> SSAVer -> (VId,Int,Sort,VarType,[(String,Int,Sort)]) -> EnvOp SSAVer
   toSSAVar ident ver (vid,k,ty,mty,mod) = do 
     ast <- lift $ encIdentWithVId vid ident k ty
     mod <- foldM (toSSAModel vid) M.empty mod 
     let var = SSAVar ast ty k mod mty
     return $ M.insert vid var ver
   
   toSSAModel :: VId -> SSAVarModel -> (String,Int,Sort) -> EnvOp SSAVarModel
   toSSAModel vid mod (ident,k,ty) = do
     ast <- lift $ encIdentWithVId vid ident k ty
     return $ M.insert ident (ast,ty,k) mod

   encIdentWithVId :: Int -> String -> Int -> Sort -> Z3 AST
   encIdentWithVId j str i sort = do 
     let nstr = str ++ "_" ++ show j ++ "_" ++ show i
     sym <- mkStringSymbol nstr
     mkVar sym sort

-- | Join SSAVar
joinSSAVar :: SSAVar -> SSAVar -> SSAVar
joinSSAVar v1 v2 = 
  let mod = M.union (_v_mod v1) (_v_mod v2)
  in v1 { _v_mod = mod }

-- | Join Pre-conditions
joinPres :: SSAMap -> SSAMap -> SSAMap -> AST -> AST -> EnvOp AST
joinPres nmap m1 m2 pre1 pre2 = do
  -- join the new map with the old left map
  npre1 <- joinPre nmap m1 pre1
  -- join the new map with the old right map
  npre2 <- joinPre nmap m2 pre2
  -- apply the disjunction
  lift $ mkOr [npre1,npre2] 
 where
  -- | Update the precondition according to a new ssa map
  joinPre :: SSAMap -> SSAMap -> AST -> EnvOp AST
  joinPre new_map old_map old_pre = do
    -- get the ASTs of the intersection of the variables
    -- Map Ident (Map VId [(AST,AST)])
    let vars = M.intersectionWith (M.intersectionWith pairSSAVar) new_map old_map 
        pars = concatMap M.elems $ M.elems vars 
    eqs <- lift $ mapM (\(a,b) -> mkEq a b) $ concat pars 
    lift $ mkAnd (old_pre:eqs)  
  -- | Pair the AST of two SSA Variables
  pairSSAVar :: SSAVar -> SSAVar -> [(AST,AST)]
  pairSSAVar v1 v2 = 
    let mvars = M.intersectionWith pairFst (_v_mod v1) (_v_mod v2)  
    in (_v_ast v1,_v_ast v2):(M.elems mvars) 

-- | Join of Environments 
--   Joinable fields are
--     _ssamap, functmap, pre, edits, numret, anonym
joinEnv :: Env -> Env -> Env -> EnvOp Env
joinEnv orig e1 e2 = do
  let ssa_orig = _e_ssamap orig
      ssa_e1   = _e_ssamap e1
      ssa_e2   = _e_ssamap e2
      -- Identifies the 
      vs       = M.intersectionWithKey (diffSSAMap ssa_orig) ssa_e1 ssa_e2 
  new_ssa <- toSSAMap vs 
  pre     <- joinPres new_ssa ssa_e1 ssa_e2 (_e_pre e1) (_e_pre e2)
  let ssa      = M.unionsWith (M.unionWith joinSSAVar) [new_ssa,ssa_e1,ssa_e2] 
      fnm      = _e_fnmap   e1 `M.union` _e_fnmap  e2
      classes  = _e_classes e2
      eds      = _e_edits   e2 
      debug    = _e_debug   e2
      numret   = if _e_numret e1 /= _e_numret e2 
                 then error "joinEnv: different # ret" 
                 else _e_numret e1
      vids     = _e_vids    e2
      anonym   = _e_anonym  e1 `max` _e_anonym e2
      mode     = _e_mode    e2
      rety     = _e_rety    e1
      consts   = _e_consts  e2
  --wizPrint "join_env: original " 
  --printSSA ssa_orig
  --wizPrint "join_env: then branch" 
  --printSSA ssa_e1 
  --wizPrint "join_env: else branch" 
  --printSSA ssa_e2 
  --wizPrint "join_env: result"
  --printSSA ssa
  return $ Env ssa fnm pre classes eds debug numret vids anonym mode rety consts 

-- | Replace Version Identifiers 
updatePid :: [VId] -> EnvOp ()
updatePid vIds = do
  s@Env{..} <- get
  put s{ _e_vids = vIds }

-- | Replace Pre-Condition 
updatePre :: AST -> EnvOp ()
updatePre pre = do
  s@Env{..} <- get
  put s{ _e_pre = pre }

-- | Replace Edit Scripts 
updateEdits :: [AnnEdit] -> EnvOp ()
updateEdits edits = do
  s@Env{..} <- get
  put s{ _e_edits = edits }

-- | Pop Edits
popEdits :: EnvOp [AnnBlockStmt]
popEdits = do
  s@Env{..} <- get
  let (res,edits) = unzip $ map (\e -> (head e, tail e)) _e_edits 
  put s { _e_edits = edits }
  return res 

-- | Increment Return Counter
updateNumRet :: EnvOp ()
updateNumRet = do
  s@Env{..} <- get
  let numret = _e_numret + 1
  put s{ _e_numret = numret }
  
-- @ API for SSAMap

-- | Replace SSA Map 
updateSSAMap :: SSAMap -> EnvOp ()
updateSSAMap ssamap = do
  s@Env{..} <- get
  put s{ _e_ssamap = ssamap}

-- | Get AST from SSAMap 
--   Since there might be several auxiliary variables related
--   to an identifier/variable, we need to return a list 
--   where at the head we have the AST of the main variable
getASTSSAVar :: SSAVar -> [AST]
getASTSSAVar v = (_v_ast v):(map fst3 $ M.elems $ _v_mod v)

getASTSSAMap :: String -> VId -> Ident -> SSAMap -> [AST]
getASTSSAMap err vid ident ssamap = 
  case M.lookup ident ssamap of
    Nothing -> error $ "getASTSSAMap: " ++ err
    Just l  -> case M.lookup vid l of
      Nothing -> error $ "getASTSSAMap vid: " ++ err
      Just v  -> getASTSSAVar v

getVarSSAMap :: String -> VId -> Ident -> SSAMap -> SSAVar
getVarSSAMap err vid ident ssamap = 
  case M.lookup ident ssamap of
    Nothing -> error $ "getVarSSAMap: " ++ err
    Just l  -> case M.lookup vid l of
      Nothing -> error $ "getVarSSAMap vid: " ++ err
      Just v  -> v

-- | Generate a new AST and increment the counter
updateVariable :: VId -> Ident -> SSAVar -> Z3 SSAVar
updateVariable vId (Ident id) v@SSAVar{..} = do 
 let cnt  = _v_cnt + 1
     name = id ++ "_" ++ show vId ++ "_" ++ show cnt 
 ast <- mkFreshConst name _v_typ
 let var = SSAVar ast _v_typ cnt _v_mod _v_mty 
 return var

-- | Insert SSAVar to SSAMap 
insertSSAVar :: VId -> Ident -> SSAVar -> SSAMap -> SSAMap
insertSSAVar vId ident el ssamap =
  case M.lookup ident ssamap of
    Nothing -> M.insert ident (M.singleton vId el) ssamap
    Just l  -> let l' = M.insert vId el l
               in M.insert ident l' ssamap

-- | Update Function Map
updateFunctMap :: FunctMap -> EnvOp ()
updateFunctMap fnmap = do
  s@Env{..} <- get
  put s{ _e_fnmap = fnmap }

-- | Increment Anonymous Function Counter 
incAnonym :: EnvOp Int
incAnonym = do
  s@Env{..} <- get
  let anonym = _e_anonym 
  put s{ _e_anonym =  anonym + 1}
  return anonym

-- | Insert constant
insertConst :: Literal -> AST -> EnvOp ()
insertConst l a = do
  s@Env{..} <- get
  let c = M.insert l a _e_consts
  put s{ _e_consts = c }

-- | Update Constants
updateConsts :: ConstMap -> EnvOp ()
updateConsts c = do
  s@Env{..} <- get
  put s{ _e_consts = c }
