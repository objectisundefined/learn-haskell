{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AIAgent.Core.Executor
  ( -- * Executor Types
    ExecutionResult(..)
  , ExecutionError(..)
  , ExecutionMetrics(..)
  , GraphExecutor(..)
  
    -- * Executor Creation
  , newExecutor
  , defaultExecutor
  
    -- * Graph Execution
  , executeGraph
  , executeFromNode
  , executeSubgraph
  , executeWithStrategy
  
    -- * Execution Control
  , pauseExecution
  , resumeExecution
  , cancelExecution
  , getExecutionStatus
  
    -- * Monitoring and Metrics
  , getExecutionMetrics
  , getNodeExecutionHistory
  , visualizeExecution
  
    -- * Lenses
  , resultNodes
  , resultState
  , resultMetrics
  , resultErrors
  , executorGraph
  , executorState
  , executorConfig
  
    -- * Utilities
  , isExecutionComplete
  , hasExecutionErrors
  , getFailedNodes
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.List (sortBy)
import Data.Ord (comparing)

import AIAgent.Core.State
import AIAgent.Core.Node
import AIAgent.Core.Graph

-- | Errors that can occur during graph execution
data ExecutionError = ExecutionError
  { _execErrorNode    :: NodeId
  , _execErrorMessage :: Text
  , _execErrorTime    :: UTCTime
  , _execErrorDetails :: Maybe Value
  } deriving (Show, Eq)

-- | Metrics collected during graph execution
data ExecutionMetrics = ExecutionMetrics
  { _metricsStartTime     :: UTCTime
  , _metricsEndTime       :: Maybe UTCTime
  , _metricsNodesExecuted :: Int
  , _metricsNodesFailed   :: Int
  , _metricsNodesSkipped  :: Int
  , _metricsTotalDuration :: Maybe NominalDiffTime
  , _metricsNodeDurations :: HashMap NodeId NominalDiffTime
  } deriving (Show, Eq)

-- | Result of graph execution
data ExecutionResult = ExecutionResult
  { _resultNodes   :: HashMap NodeId NodeResult
  , _resultState   :: AgentState
  , _resultMetrics :: ExecutionMetrics
  , _resultErrors  :: [ExecutionError]
  } deriving (Show)

-- | The graph executor manages execution state and provides control
data GraphExecutor = GraphExecutor
  { _executorGraph   :: TVar Graph
  , _executorState   :: AgentState
  , _executorConfig  :: TVar GraphConfig
  , _executorStatus  :: TVar ExecutionStatus
  , _executorResults :: TVar (HashMap NodeId NodeResult)
  , _executorMetrics :: TVar ExecutionMetrics
  , _executorErrors  :: TVar [ExecutionError]
  }

-- | Status of graph execution
data ExecutionStatus
  = NotStarted
  | Running
  | Paused
  | Completed
  | Failed
  | Cancelled
  deriving (Show, Eq)

-- Generate lenses
makeLenses ''ExecutionError
makeLenses ''ExecutionMetrics
makeLenses ''ExecutionResult
makeLenses ''GraphExecutor

instance Show GraphExecutor where
  show _ = "GraphExecutor{...}"

-- | Create a new graph executor
newExecutor :: MonadIO m => Graph -> AgentState -> m GraphExecutor
newExecutor graph agentState = liftIO $ do
  startTime <- getCurrentTime
  let initialMetrics = ExecutionMetrics startTime Nothing 0 0 0 Nothing HM.empty
  
  graphVar <- newTVarIO graph
  configVar <- newTVarIO (_graphConfig graph)
  statusVar <- newTVarIO NotStarted
  resultsVar <- newTVarIO HM.empty
  metricsVar <- newTVarIO initialMetrics
  errorsVar <- newTVarIO []
  
  return $ GraphExecutor graphVar agentState configVar statusVar resultsVar metricsVar errorsVar

-- | Create a default executor with empty graph
defaultExecutor :: MonadIO m => m GraphExecutor
defaultExecutor = do
  state <- newAgentState Nothing
  newExecutor emptyGraph state

-- | Execute the entire graph
executeGraph :: MonadIO m => GraphExecutor -> m ExecutionResult
executeGraph executor = liftIO $ do
  -- Validate graph before execution
  graph <- readTVarIO (_executorGraph executor)
  case validateGraph graph of
    Left errors -> do
      let execErrors = map (\err -> ExecutionError "validation" (Text.pack $ show err) 
                                     <$> getCurrentTime <*> pure Nothing) errors
      execErrorsList <- sequence execErrors
      atomically $ writeTVar (_executorErrors executor) execErrorsList
      buildResult executor
    Right () -> do
      -- Set status to running
      atomically $ writeTVar (_executorStatus executor) Running
      
      -- Get execution strategy
      config <- readTVarIO (_executorConfig executor)
      
      -- Execute based on strategy
      case _configStrategy config of
        Sequential -> executeSequential executor
        Parallel -> executeParallel executor
        Conditional -> executeConditional executor
        Custom -> executeCustom executor
      
      -- Set completion status
      errors <- readTVarIO (_executorErrors executor)
      let finalStatus = if null errors then Completed else Failed
      atomically $ writeTVar (_executorStatus executor) finalStatus
      
      -- Update metrics
      updateFinalMetrics executor
      
      buildResult executor

-- | Execute graph sequentially in topological order
executeSequential :: GraphExecutor -> IO ()
executeSequential executor = do
  graph <- readTVarIO (_executorGraph executor)
  case topologicalSort graph of
    Left cycle -> do
      timestamp <- getCurrentTime
      let cycleError = ExecutionError "cycle_detection" 
                         ("Circular dependency detected: " <> Text.intercalate " -> " cycle)
                         timestamp Nothing
      atomically $ modifyTVar (_executorErrors executor) (cycleError :)
    Right sortedNodes -> do
      mapM_ (executeNode executor) sortedNodes

-- | Execute graph with maximum parallelism
executeParallel :: GraphExecutor -> IO ()
executeParallel executor = do
  graph <- readTVarIO (_executorGraph executor)
  config <- readTVarIO (_executorConfig executor)
  
  let startNodes = getStartNodes graph
      maxParallel = _configMaxParallel config
  
  -- Use a semaphore to limit parallelism
  sem <- newTVarIO maxParallel
  executeParallelNodes executor sem (HS.fromList startNodes) HS.empty

-- | Execute nodes in parallel with concurrency control
executeParallelNodes :: GraphExecutor -> TVar Int -> HashSet NodeId -> HashSet NodeId -> IO ()
executeParallelNodes executor sem ready executed = do
  unless (HS.null ready) $ do
    graph <- readTVarIO (_executorGraph executor)
    
    -- Convert ready set to list and execute in parallel
    let readyList = HS.toList ready
    asyncActions <- mapM (\nodeId -> async $ do
      -- Wait for semaphore
      atomically $ do
        count <- readTVar sem
        if count > 0
          then writeTVar sem (count - 1)
          else retry
      
      -- Execute node
      result <- executeNode executor nodeId
      
      -- Release semaphore
      atomically $ modifyTVar sem (+1)
      
      return (nodeId, result)
    ) readyList
    
    -- Wait for all to complete
    results <- mapM wait asyncActions
    
    -- Update executed set
    let newExecuted = HS.union executed (HS.fromList $ map fst results)
    
    -- Find next ready nodes
    let allNodes = HS.fromList $ HM.keys (_graphNodes graph)
        remainingNodes = HS.difference allNodes newExecuted
        nextReady = HS.filter (\nodeId -> 
          let predecessors = HS.fromList $ getPredecessors nodeId graph
          in HS.isSubsetOf predecessors newExecuted) remainingNodes
    
    -- Continue with next batch
    executeParallelNodes executor sem nextReady newExecuted

-- | Execute graph conditionally based on edge conditions
executeConditional :: GraphExecutor -> IO ()
executeConditional executor = do
  graph <- readTVarIO (_executorGraph executor)
  let startNodes = getStartNodes graph
  mapM_ (executeConditionalFrom executor HS.empty) startNodes

-- | Execute conditionally from a specific node
executeConditionalFrom :: GraphExecutor -> HashSet NodeId -> NodeId -> IO ()
executeConditionalFrom executor visited nodeId = do
  unless (nodeId `HS.member` visited) $ do
    -- Execute current node
    result <- executeNode executor nodeId
    
    -- Check edges and execute successors based on conditions
    graph <- readTVarIO (_executorGraph executor)
    agentState <- return $ _executorState executor
    currentState <- getState agentState
    
    let edges = filter (\edge -> _edgeFrom edge == nodeId) (_graphEdges graph)
    validEdges <- filterM (evaluateEdgeCondition currentState result) edges
    
    let newVisited = HS.insert nodeId visited
        successors = map _edgeTo validEdges
    
    mapM_ (executeConditionalFrom executor newVisited) successors

-- | Evaluate if an edge condition is met
evaluateEdgeCondition :: HashMap Text Value -> NodeResult -> Edge -> IO Bool
evaluateEdgeCondition state result edge = 
  case _edgeCondition edge of
    Always -> return True
    Never -> return False
    OnSuccess -> return $ _resultStatus result == Success
    OnFailure -> return $ _resultStatus result == Failed
    OnCondition predicate -> return $ predicate state
    OnValue key expectedValue -> 
      return $ HM.lookup key state == Just expectedValue

-- | Custom execution strategy (placeholder)
executeCustom :: GraphExecutor -> IO ()
executeCustom executor = executeSequential executor  -- Default to sequential

-- | Execute a single node
executeNode :: GraphExecutor -> NodeId -> IO NodeResult
executeNode executor nodeId = do
  graph <- readTVarIO (_executorGraph executor)
  case getNode nodeId graph of
    Nothing -> do
      timestamp <- getCurrentTime
      let nodeError = NodeError ("Node not found: " <> nodeId) Nothing Nothing timestamp
      return $ NodeResult Failed Nothing (Just nodeError) HM.empty
    Just node -> do
      startTime <- getCurrentTime
      result <- runNode node (_executorState executor)
      endTime <- getCurrentTime
      
      let duration = diffUTCTime endTime startTime
      
      -- Update results
      atomically $ modifyTVar (_executorResults executor) (HM.insert nodeId result)
      
      -- Update metrics
      atomically $ modifyTVar (_executorMetrics executor) $ \metrics ->
        metrics & metricsNodesExecuted %~ (+1)
                & metricsNodeDurations %~ HM.insert nodeId duration
                & (if _resultStatus result == Failed then metricsNodesFailed else id) %~ (+1)
      
      -- Add error if failed
      when (_resultStatus result == Failed) $ do
        case _resultError result of
          Just nodeErr -> do
            let execError = ExecutionError nodeId (_errorMessage nodeErr) 
                                         (_errorTimestamp nodeErr) (_errorDetails nodeErr)
            atomically $ modifyTVar (_executorErrors executor) (execError :)
          Nothing -> do
            timestamp <- getCurrentTime
            let execError = ExecutionError nodeId "Unknown error" timestamp Nothing
            atomically $ modifyTVar (_executorErrors executor) (execError :)
      
      return result

-- | Execute from a specific starting node
executeFromNode :: MonadIO m => GraphExecutor -> NodeId -> m ExecutionResult
executeFromNode executor startNodeId = liftIO $ do
  atomically $ writeTVar (_executorStatus executor) Running
  _ <- executeNode executor startNodeId
  atomically $ writeTVar (_executorStatus executor) Completed
  updateFinalMetrics executor
  buildResult executor

-- | Execute a subgraph containing only specified nodes
executeSubgraph :: MonadIO m => GraphExecutor -> [NodeId] -> m ExecutionResult
executeSubgraph executor nodeIds = liftIO $ do
  graph <- readTVarIO (_executorGraph executor)
  
  -- Filter graph to only include specified nodes
  let filteredNodes = HM.filterWithKey (\k _ -> k `elem` nodeIds) (_graphNodes graph)
      filteredEdges = filter (\edge -> _edgeFrom edge `elem` nodeIds && 
                                       _edgeTo edge `elem` nodeIds) (_graphEdges graph)
      subgraph = graph & graphNodes .~ filteredNodes
                       & graphEdges .~ filteredEdges
  
  -- Create temporary executor for subgraph
  tempExecutor <- newExecutor subgraph (_executorState executor)
  executeGraph tempExecutor

-- | Execute with a specific strategy, temporarily overriding the config
executeWithStrategy :: MonadIO m => GraphExecutor -> ExecutionStrategy -> m ExecutionResult
executeWithStrategy executor strategy = liftIO $ do
  -- Save original config
  originalConfig <- readTVarIO (_executorConfig executor)
  
  -- Set new strategy
  let newConfig = originalConfig & configStrategy .~ strategy
  atomically $ writeTVar (_executorConfig executor) newConfig
  
  -- Execute
  result <- executeGraph executor
  
  -- Restore original config
  atomically $ writeTVar (_executorConfig executor) originalConfig
  
  return result

-- | Pause execution (placeholder - would need more sophisticated control)
pauseExecution :: MonadIO m => GraphExecutor -> m ()
pauseExecution executor = liftIO $ 
  atomically $ writeTVar (_executorStatus executor) Paused

-- | Resume execution (placeholder)
resumeExecution :: MonadIO m => GraphExecutor -> m ()
resumeExecution executor = liftIO $ 
  atomically $ writeTVar (_executorStatus executor) Running

-- | Cancel execution
cancelExecution :: MonadIO m => GraphExecutor -> m ()
cancelExecution executor = liftIO $ 
  atomically $ writeTVar (_executorStatus executor) Cancelled

-- | Get current execution status
getExecutionStatus :: MonadIO m => GraphExecutor -> m ExecutionStatus
getExecutionStatus executor = liftIO $ readTVarIO (_executorStatus executor)

-- | Get execution metrics
getExecutionMetrics :: MonadIO m => GraphExecutor -> m ExecutionMetrics
getExecutionMetrics executor = liftIO $ readTVarIO (_executorMetrics executor)

-- | Get node execution history
getNodeExecutionHistory :: MonadIO m => GraphExecutor -> m [(NodeId, NodeResult)]
getNodeExecutionHistory executor = liftIO $ do
  results <- readTVarIO (_executorResults executor)
  return $ HM.toList results

-- | Visualize execution results
visualizeExecution :: MonadIO m => GraphExecutor -> m Text
visualizeExecution executor = liftIO $ do
  results <- readTVarIO (_executorResults executor)
  metrics <- readTVarIO (_executorMetrics executor)
  errors <- readTVarIO (_executorErrors executor)
  
  let resultLines = map (\(nodeId, result) -> 
        nodeId <> ": " <> Text.pack (show (_resultStatus result))) (HM.toList results)
      errorLines = map (\err -> "ERROR " <> _execErrorNode err <> ": " <> _execErrorMessage err) errors
      metricsLines = 
        [ "Nodes Executed: " <> Text.pack (show (_metricsNodesExecuted metrics))
        , "Nodes Failed: " <> Text.pack (show (_metricsNodesFailed metrics))
        ]
  
  return $ Text.unlines $ resultLines ++ errorLines ++ metricsLines

-- | Update final metrics when execution completes
updateFinalMetrics :: GraphExecutor -> IO ()
updateFinalMetrics executor = do
  endTime <- getCurrentTime
  atomically $ modifyTVar (_executorMetrics executor) $ \metrics ->
    let duration = diffUTCTime endTime (_metricsStartTime metrics)
    in metrics & metricsEndTime .~ Just endTime
               & metricsTotalDuration .~ Just duration

-- | Build the final execution result
buildResult :: GraphExecutor -> IO ExecutionResult
buildResult executor = do
  results <- readTVarIO (_executorResults executor)
  metrics <- readTVarIO (_executorMetrics executor)
  errors <- readTVarIO (_executorErrors executor)
  
  return $ ExecutionResult results (_executorState executor) metrics errors

-- | Check if execution is complete
isExecutionComplete :: MonadIO m => GraphExecutor -> m Bool
isExecutionComplete executor = liftIO $ do
  status <- readTVarIO (_executorStatus executor)
  return $ status `elem` [Completed, Failed, Cancelled]

-- | Check if execution has errors
hasExecutionErrors :: MonadIO m => GraphExecutor -> m Bool
hasExecutionErrors executor = liftIO $ do
  errors <- readTVarIO (_executorErrors executor)
  return $ not (null errors)

-- | Get list of failed nodes
getFailedNodes :: MonadIO m => GraphExecutor -> m [NodeId]
getFailedNodes executor = liftIO $ do
  results <- readTVarIO (_executorResults executor)
  return $ HM.keys $ HM.filter (\result -> _resultStatus result == Failed) results