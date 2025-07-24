{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module AIAgent.Core.Graph
  ( -- * Graph Types
    Graph(..)
  , Edge(..)
  , EdgeCondition(..)
  , GraphConfig(..)
  , ExecutionStrategy(..)
  
    -- * Graph Construction
  , emptyGraph
  , addNode
  , addEdge
  , addConditionalEdge
  , removeNode
  , removeEdge
  
    -- * Graph Queries
  , getNode
  , getNodes
  , getEdges
  , getSuccessors
  , getPredecessors
  , getStartNodes
  , getEndNodes
  
    -- * Graph Validation
  , validateGraph
  , GraphValidationError(..)
  , hasCircularDependency
  , isAcyclic
  
    -- * Graph Traversal
  , topologicalSort
  , breadthFirstTraversal
  , depthFirstTraversal
  
    -- * Lenses
  , graphNodes
  , graphEdges
  , graphConfig
  , edgeFrom
  , edgeTo
  , edgeCondition
  , edgeMetadata
  
    -- * Utilities
  , graphSize
  , edgeCount
  , visualizeGraph
  ) where

import Control.Lens
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (nub)

import AIAgent.Core.Node
import AIAgent.Core.State

-- | Execution strategy for the graph
data ExecutionStrategy
  = Sequential    -- ^ Execute nodes one by one in topological order
  | Parallel      -- ^ Execute independent nodes in parallel
  | Conditional   -- ^ Execute based on edge conditions
  | Custom        -- ^ Custom execution logic
  deriving (Show, Eq)

-- | Configuration for graph execution
data GraphConfig = GraphConfig
  { _configStrategy     :: ExecutionStrategy
  , _configMaxParallel  :: Int
  , _configTimeout      :: Maybe Int  -- ^ Timeout in seconds
  , _configRetryCount   :: Int
  , _configFailFast     :: Bool       -- ^ Stop on first failure
  } deriving (Show, Eq)

-- | Edge condition that determines if an edge should be traversed
data EdgeCondition
  = Always                                    -- ^ Always traverse this edge
  | Never                                     -- ^ Never traverse this edge
  | OnSuccess                                 -- ^ Only if previous node succeeded
  | OnFailure                                 -- ^ Only if previous node failed
  | OnCondition (HashMap Text Value -> Bool)  -- ^ Custom condition based on state
  | OnValue Text Value                        -- ^ If state key equals value

-- | An edge connecting two nodes with optional conditions
data Edge = Edge
  { _edgeFrom      :: NodeId
  , _edgeTo        :: NodeId
  , _edgeCondition :: EdgeCondition
  , _edgeMetadata  :: HashMap Text Value
  } deriving (Show)

-- | Validation errors for graphs
data GraphValidationError
  = NodeNotFound NodeId
  | CircularDependency [NodeId]
  | DisconnectedNode NodeId
  | InvalidEdge NodeId NodeId Text
  | EmptyGraph
  deriving (Show, Eq)

-- | The main graph structure containing nodes and edges
data Graph = Graph
  { _graphNodes  :: HashMap NodeId Node
  , _graphEdges  :: [Edge]
  , _graphConfig :: GraphConfig
  } deriving (Show)

-- Generate lenses
makeLenses ''GraphConfig
makeLenses ''Edge
makeLenses ''Graph

instance Show EdgeCondition where
  show Always = "Always"
  show Never = "Never"
  show OnSuccess = "OnSuccess"
  show OnFailure = "OnFailure"
  show (OnCondition _) = "OnCondition(...)"
  show (OnValue key val) = "OnValue(" ++ Text.unpack key ++ ", " ++ show val ++ ")"

-- | Create an empty graph with default configuration
emptyGraph :: Graph
emptyGraph = Graph
  { _graphNodes = HM.empty
  , _graphEdges = []
  , _graphConfig = defaultConfig
  }
  where
    defaultConfig = GraphConfig Sequential 4 Nothing 0 True

-- | Add a node to the graph
addNode :: Node -> Graph -> Graph
addNode node graph = graph & graphNodes %~ HM.insert (_nodeId node) node

-- | Add an edge to the graph
addEdge :: NodeId -> NodeId -> Graph -> Graph
addEdge fromId toId graph = addConditionalEdge fromId toId Always HM.empty graph

-- | Add a conditional edge to the graph
addConditionalEdge :: NodeId -> NodeId -> EdgeCondition -> HashMap Text Value -> Graph -> Graph
addConditionalEdge fromId toId condition metadata graph =
  let edge = Edge fromId toId condition metadata
  in graph & graphEdges %~ (edge :)

-- | Remove a node from the graph (and all associated edges)
removeNode :: NodeId -> Graph -> Graph
removeNode nodeId graph =
  graph & graphNodes %~ HM.delete nodeId
        & graphEdges %~ filter (\edge -> _edgeFrom edge /= nodeId && _edgeTo edge /= nodeId)

-- | Remove an edge from the graph
removeEdge :: NodeId -> NodeId -> Graph -> Graph
removeEdge fromId toId graph =
  graph & graphEdges %~ filter (\edge -> not (_edgeFrom edge == fromId && _edgeTo edge == toId))

-- | Get a node by ID
getNode :: NodeId -> Graph -> Maybe Node
getNode nodeId graph = HM.lookup nodeId (_graphNodes graph)

-- | Get all nodes in the graph
getNodes :: Graph -> [Node]
getNodes graph = HM.elems (_graphNodes graph)

-- | Get all edges in the graph
getEdges :: Graph -> [Edge]
getEdges graph = _graphEdges graph

-- | Get successor nodes of a given node
getSuccessors :: NodeId -> Graph -> [NodeId]
getSuccessors nodeId graph = 
  [_edgeTo edge | edge <- _graphEdges graph, _edgeFrom edge == nodeId]

-- | Get predecessor nodes of a given node
getPredecessors :: NodeId -> Graph -> [NodeId]
getPredecessors nodeId graph = 
  [_edgeFrom edge | edge <- _graphEdges graph, _edgeTo edge == nodeId]

-- | Get nodes with no predecessors (start nodes)
getStartNodes :: Graph -> [NodeId]
getStartNodes graph =
  let allNodes = HM.keys (_graphNodes graph)
      nodesWithPredecessors = HS.fromList [_edgeTo edge | edge <- _graphEdges graph]
  in filter (`HS.notMember` nodesWithPredecessors) allNodes

-- | Get nodes with no successors (end nodes)
getEndNodes :: Graph -> [NodeId]
getEndNodes graph =
  let allNodes = HM.keys (_graphNodes graph)
      nodesWithSuccessors = HS.fromList [_edgeFrom edge | edge <- _graphEdges graph]
  in filter (`HS.notMember` nodesWithSuccessors) allNodes

-- | Validate the graph structure
validateGraph :: Graph -> Either [GraphValidationError] ()
validateGraph graph = do
  let errors = concat
        [ validateNodesExist graph
        , validateNoCircularDependencies graph
        , validateNotEmpty graph
        ]
  if null errors
    then Right ()
    else Left errors

-- | Check if all edge references point to existing nodes
validateNodesExist :: Graph -> [GraphValidationError]
validateNodesExist graph =
  let nodeIds = HS.fromList $ HM.keys (_graphNodes graph)
      edges = _graphEdges graph
      invalidEdges = filter (\edge -> 
        _edgeFrom edge `HS.notMember` nodeIds || _edgeTo edge `HS.notMember` nodeIds) edges
  in concatMap (\edge -> 
      [ NodeNotFound (_edgeFrom edge) | _edgeFrom edge `HS.notMember` nodeIds ] ++
      [ NodeNotFound (_edgeTo edge) | _edgeTo edge `HS.notMember` nodeIds ]) invalidEdges

-- | Check for circular dependencies
validateNoCircularDependencies :: Graph -> [GraphValidationError]
validateNoCircularDependencies graph =
  case hasCircularDependency graph of
    Just cycle -> [CircularDependency cycle]
    Nothing -> []

-- | Check if graph is not empty
validateNotEmpty :: Graph -> [GraphValidationError]
validateNotEmpty graph =
  if HM.null (_graphNodes graph)
    then [EmptyGraph]
    else []

-- | Check if the graph has circular dependencies
hasCircularDependency :: Graph -> Maybe [NodeId]
hasCircularDependency graph = detectCycle (HM.keys $ _graphNodes graph) HS.empty []
  where
    detectCycle :: [NodeId] -> HashSet NodeId -> [NodeId] -> Maybe [NodeId]
    detectCycle [] _ _ = Nothing
    detectCycle (node:rest) visited path
      | node `HS.member` visited = 
          case dropWhile (/= node) path of
            [] -> detectCycle rest visited path
            cycle -> Just (node : cycle)
      | otherwise = 
          let successors = getSuccessors node graph
              newVisited = HS.insert node visited
              newPath = node : path
          in case detectCycle successors newVisited newPath of
               Just cycle -> Just cycle
               Nothing -> detectCycle rest (HS.delete node newVisited) (tail path)

-- | Check if the graph is acyclic (DAG)
isAcyclic :: Graph -> Bool
isAcyclic graph = case hasCircularDependency graph of
  Nothing -> True
  Just _ -> False

-- | Perform topological sort of the graph
topologicalSort :: Graph -> Either [NodeId] [NodeId]
topologicalSort graph
  | not (isAcyclic graph) = Left $ fromMaybe [] (hasCircularDependency graph)
  | otherwise = Right $ kahn graph
  where
    kahn :: Graph -> [NodeId]
    kahn g = 
      let startNodes = getStartNodes g
      in topSort startNodes [] g
    
    topSort :: [NodeId] -> [NodeId] -> Graph -> [NodeId]
    topSort [] result _ = reverse result
    topSort (node:queue) result g =
      let successors = getSuccessors node g
          g' = removeNode node g
          newStarts = filter (\n -> null (getPredecessors n g')) successors
          newQueue = queue ++ newStarts
      in topSort newQueue (node:result) g'

-- | Breadth-first traversal of the graph
breadthFirstTraversal :: NodeId -> Graph -> [NodeId]
breadthFirstTraversal startNode graph = bfs [startNode] HS.empty []
  where
    bfs :: [NodeId] -> HashSet NodeId -> [NodeId] -> [NodeId]
    bfs [] _ result = reverse result
    bfs (node:queue) visited result
      | node `HS.member` visited = bfs queue visited result
      | otherwise =
          let successors = getSuccessors node graph
              newQueue = queue ++ successors
              newVisited = HS.insert node visited
          in bfs newQueue newVisited (node:result)

-- | Depth-first traversal of the graph
depthFirstTraversal :: NodeId -> Graph -> [NodeId]
depthFirstTraversal startNode graph = dfs [startNode] HS.empty []
  where
    dfs :: [NodeId] -> HashSet NodeId -> [NodeId] -> [NodeId]
    dfs [] _ result = reverse result
    dfs (node:stack) visited result
      | node `HS.member` visited = dfs stack visited result
      | otherwise =
          let successors = getSuccessors node graph
              newStack = successors ++ stack
              newVisited = HS.insert node visited
          in dfs newStack newVisited (node:result)

-- | Get the number of nodes in the graph
graphSize :: Graph -> Int
graphSize graph = HM.size (_graphNodes graph)

-- | Get the number of edges in the graph
edgeCount :: Graph -> Int
edgeCount graph = length (_graphEdges graph)

-- | Generate a simple text visualization of the graph
visualizeGraph :: Graph -> Text
visualizeGraph graph = 
  let nodes = HM.keys (_graphNodes graph)
      edges = _graphEdges graph
      nodeLines = map (\nid -> "Node: " <> nid) nodes
      edgeLines = map (\edge -> _edgeFrom edge <> " -> " <> _edgeTo edge) edges
  in Text.unlines $ nodeLines ++ edgeLines