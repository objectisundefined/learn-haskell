{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module AIAgent.Core.Node
  ( -- * Node Types
    NodeId
  , NodeStatus(..)
  , NodeResult(..)
  , NodeError(..)
  , Node(..)
  , NodeContext(..)
  
    -- * Node Creation
  , mkNode
  , mkAsyncNode
  , mkPureNode
  , mkIONode
  
    -- * Node Execution
  , runNode
  , runNodeWithContext
  
    -- * Node Combinators
  , sequenceNodes
  , parallelNodes
  , conditionalNode
  , retryNode
  
    -- * Lenses
  , nodeId
  , nodeName
  , nodeDescription
  , nodeAction
  , nodeMetadata
  , resultStatus
  , resultData
  , resultError
  , resultMetadata
  
    -- * Utilities
  , isSuccess
  , isFailure
  , isRunning
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad (when)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

import AIAgent.Core.State

-- | Unique identifier for a node
type NodeId = Text

-- | Status of a node execution
data NodeStatus
  = Pending    -- ^ Node is waiting to be executed
  | Running    -- ^ Node is currently executing
  | Success    -- ^ Node completed successfully
  | Failed     -- ^ Node failed with an error
  | Cancelled  -- ^ Node execution was cancelled
  deriving (Show, Eq, Ord)

-- | Error information for failed nodes
data NodeError = NodeError
  { _errorMessage   :: Text
  , _errorCode      :: Maybe Text
  , _errorDetails   :: Maybe Value
  , _errorTimestamp :: UTCTime
  } deriving (Show, Eq)

-- | Result of node execution
data NodeResult = NodeResult
  { _resultStatus   :: NodeStatus
  , _resultData     :: Maybe Value
  , _resultError    :: Maybe NodeError
  , _resultMetadata :: HashMap Text Value
  } deriving (Show, Eq)

-- | Context available to nodes during execution
data NodeContext = NodeContext
  { _contextState     :: AgentState
  , _contextNodeId    :: NodeId
  , _contextMetadata  :: HashMap Text Value
  , _contextStartTime :: UTCTime
  } deriving (Show)

-- | A computational node in the agent graph
data Node = Node
  { _nodeId          :: NodeId
  , _nodeName        :: Text
  , _nodeDescription :: Maybe Text
  , _nodeAction      :: NodeContext -> IO NodeResult
  , _nodeMetadata    :: HashMap Text Value
  }

-- Generate lenses
makeLenses ''NodeError
makeLenses ''NodeResult
makeLenses ''NodeContext
makeLenses ''Node

instance Show Node where
  show node = "Node { id = " ++ show (_nodeId node) ++ 
              ", name = " ++ show (_nodeName node) ++ " }"

-- | Create a new node with the given action
mkNode :: NodeId -> Text -> (NodeContext -> IO NodeResult) -> Node
mkNode nid name action = Node
  { _nodeId = nid
  , _nodeName = name
  , _nodeDescription = Nothing
  , _nodeAction = action
  , _nodeMetadata = HM.empty
  }

-- | Create an asynchronous node that can run concurrently
mkAsyncNode :: NodeId -> Text -> (NodeContext -> IO NodeResult) -> Node
mkAsyncNode nid name action = mkNode nid name $ \ctx -> do
  asyncAction <- async (action ctx)
  result <- wait asyncAction
  return result

-- | Create a pure node from a pure function
mkPureNode :: NodeId -> Text -> (HashMap Text Value -> Either Text Value) -> Node
mkPureNode nid name pureFunc = mkNode nid name $ \ctx -> do
  currentState <- getState (_contextState ctx)
  case pureFunc currentState of
    Left err -> do
      timestamp <- getCurrentTime
      let nodeError = NodeError err Nothing Nothing timestamp
      return $ NodeResult Failed Nothing (Just nodeError) HM.empty
    Right result -> 
      return $ NodeResult Success (Just result) Nothing HM.empty

-- | Create an IO node from an IO action
mkIONode :: NodeId -> Text -> (HashMap Text Value -> IO (Either Text Value)) -> Node
mkIONode nid name ioAction = mkNode nid name $ \ctx -> do
  currentState <- getState (_contextState ctx)
  result <- try (ioAction currentState)
  case result of
    Left (ex :: SomeException) -> do
      timestamp <- getCurrentTime
      let nodeError = NodeError (Text.pack $ show ex) Nothing Nothing timestamp
      return $ NodeResult Failed Nothing (Just nodeError) HM.empty
    Right (Left err) -> do
      timestamp <- getCurrentTime
      let nodeError = NodeError err Nothing Nothing timestamp
      return $ NodeResult Failed Nothing (Just nodeError) HM.empty
    Right (Right value) ->
      return $ NodeResult Success (Just value) Nothing HM.empty

-- | Run a node with the given state
runNode :: MonadIO m => Node -> AgentState -> m NodeResult
runNode node agentState = liftIO $ do
  startTime <- getCurrentTime
  let context = NodeContext agentState (_nodeId node) (_nodeMetadata node) startTime
  runNodeWithContext node context

-- | Run a node with a specific context
runNodeWithContext :: MonadIO m => Node -> NodeContext -> m NodeResult
runNodeWithContext node context = liftIO $ do
  result <- _nodeAction node context
  
  -- Update state with node execution info
  let executionInfo = HM.fromList
        [ ("last_executed_node", String (_nodeId node))
        , ("execution_timestamp", String (Text.pack $ show (_contextStartTime context)))
        , ("execution_status", String (Text.pack $ show (_resultStatus result)))
        ]
  
  updateState (_contextState context) executionInfo Replace (Just (_nodeId node))
  return result

-- | Execute nodes in sequence
sequenceNodes :: MonadIO m => [Node] -> AgentState -> m [NodeResult]
sequenceNodes nodes agentState = liftIO $ do
  results <- mapM (`runNode` agentState) nodes
  return results

-- | Execute nodes in parallel
parallelNodes :: MonadIO m => [Node] -> AgentState -> m [NodeResult]
parallelNodes nodes agentState = liftIO $ do
  asyncActions <- mapM (\node -> async (runNode node agentState)) nodes
  results <- mapM wait asyncActions
  return results

-- | Create a conditional node that executes based on a predicate
conditionalNode :: NodeId 
                -> Text 
                -> (HashMap Text Value -> Bool)  -- ^ Condition predicate
                -> Node                          -- ^ Node to execute if true
                -> Maybe Node                    -- ^ Optional node to execute if false
                -> Node
conditionalNode nid name predicate trueNode maybeFalseNode = 
  mkNode nid name $ \ctx -> do
    currentState <- getState (_contextState ctx)
    if predicate currentState
      then runNodeWithContext trueNode ctx
      else case maybeFalseNode of
        Just falseNode -> runNodeWithContext falseNode ctx
        Nothing -> return $ NodeResult Success Nothing Nothing HM.empty

-- | Create a node that retries on failure
retryNode :: NodeId -> Text -> Int -> Node -> Node
retryNode nid name maxRetries originalNode = mkNode nid name $ \ctx -> do
  retryAction 0
  where
    retryAction :: Int -> IO NodeResult
    retryAction attempt
      | attempt >= maxRetries = do
          timestamp <- getCurrentTime
          let nodeError = NodeError 
                "Maximum retries exceeded" 
                (Just "MAX_RETRIES") 
                (Just $ Number $ fromIntegral maxRetries)
                timestamp
          return $ NodeResult Failed Nothing (Just nodeError) HM.empty
      | otherwise = do
          result <- runNodeWithContext originalNode ctx
          case _resultStatus result of
            Success -> return result
            _ -> retryAction (attempt + 1)

-- | Check if a result represents success
isSuccess :: NodeResult -> Bool
isSuccess result = _resultStatus result == Success

-- | Check if a result represents failure
isFailure :: NodeResult -> Bool
isFailure result = _resultStatus result == Failed

-- | Check if a result represents running state
isRunning :: NodeResult -> Bool
isRunning result = _resultStatus result == Running

