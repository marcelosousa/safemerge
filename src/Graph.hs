{-# LANGUAGE RecordWildCards #-}

{-
 Control Flow Graph Representation 
-}
module Graph where

import Control.Monad.State.Lazy
import Data.List 
import Data.Map (Map)
import Edit.Types
import Language.Java.Syntax
import qualified Data.Map as M
import qualified Debug.Trace as T

type NodeId = Int

data EdgeTag = LoopHead | IfJoin | Entry | Exit | CondTag
  deriving (Show, Eq)

data EdgeInfo 
 = EdgeInfo 
   {
     edge_tags :: [EdgeTag]
   , edge_code :: Stmt 
   }
  deriving Show

instance Eq EdgeInfo where
  (==) n1 n2 = edge_tags n1 == edge_tags n2

type EdgeId = Int
type Graphs st = Map MIdent (Graph st)

data Graph st
 = Graph
   {
     entry_node :: NodeId                       -- entry point
   , graph      :: Map NodeId [(EdgeId,NodeId)] -- successors
   , edge_table :: Map EdgeId EdgeInfo          -- information
   , node_table :: Map NodeId [st]              -- ^ states per thread 
   }
  deriving Show

succs :: Graph st -> NodeId -> [(EdgeId,NodeId)]
succs cfg@Graph{..} node = case M.lookup node graph of
  Nothing -> [] -- error "succs: no successors for node"
  Just s  -> s

get_edge_info :: Graph st -> EdgeId -> EdgeInfo 
get_edge_info cfg@Graph{..} edge = case M.lookup edge edge_table of
  Nothing -> error "get_edge_info: no info for edge id"
  Just s  -> s

get_node_info :: Graph st -> NodeId -> [st]
get_node_info cfg@Graph{..} node = case M.lookup node node_table of
  Nothing -> [] 
  Just s  -> s
 
is_cond :: [EdgeTag] -> Bool
is_cond = any (== CondTag)  

is_exit :: [EdgeTag] -> Bool
is_exit = any (== Exit)  

is_join :: [EdgeTag] -> Bool
is_join = any (== IfJoin)  

init_graph :: NodeId -> Graph st
init_graph e = Graph e M.empty M.empty M.empty
