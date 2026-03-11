{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, try)
import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)

import AIAgent.Core.State

type NodeId = Text

data NodeStatus
  = Pending
  | Running
  | Success
  | Failed
  | Cancelled
  deriving (Show, Eq, Ord)

data NodeError = NodeError
  { _errorMessage  :: Text
  , _errorCode     :: Maybe Text
  , _errorDetails  :: Maybe Value
  , _errorTimestamp :: UTCTime
  } deriving (Show, Eq)

data NodeResult = NodeResult
  { _resultStatus   :: NodeStatus
  , _resultData     :: Maybe Value
  , _resultError    :: Maybe NodeError
  , _resultMetadata :: HashMap Text Value
  } deriving (Show, Eq)

data NodeContext = NodeContext
  { _contextState     :: AgentState
  , _contextNodeId    :: NodeId
  , _contextMetadata  :: HashMap Text Value
  , _contextStartTime :: UTCTime
  }

-- AgentState contains TVars which cannot derive Show
instance Show NodeContext where
  show ctx = "NodeContext { nodeId = " ++ show (_contextNodeId ctx)
          ++ ", metadata = " ++ show (_contextMetadata ctx)
          ++ ", startTime = " ++ show (_contextStartTime ctx) ++ " }"

data Node = Node
  { _nodeId          :: NodeId
  , _nodeName        :: Text
  , _nodeDescription :: Maybe Text
  , _nodeAction      :: NodeContext -> IO NodeResult
  , _nodeMetadata    :: HashMap Text Value
  }

instance Show Node where
  show node = "Node { id = " ++ show (_nodeId node)
           ++ ", name = " ++ show (_nodeName node) ++ " }"

makeLenses ''NodeError
makeLenses ''NodeResult
makeLenses ''NodeContext
makeLenses ''Node

mkNode :: NodeId -> Text -> (NodeContext -> IO NodeResult) -> Node
mkNode nid name action = Node
  { _nodeId          = nid
  , _nodeName        = name
  , _nodeDescription = Nothing
  , _nodeAction      = action
  , _nodeMetadata    = HM.empty
  }

mkAsyncNode :: NodeId -> Text -> (NodeContext -> IO NodeResult) -> Node
mkAsyncNode nid name action = mkNode nid name $ \ctx -> do
  a <- async (action ctx)
  wait a

mkPureNode :: NodeId -> Text -> (HashMap Text Value -> Either Text Value) -> Node
mkPureNode nid name pureFunc = mkNode nid name $ \ctx -> do
  currentState <- getState (_contextState ctx)
  case pureFunc currentState of
    Left err -> do
      timestamp <- getCurrentTime
      let nodeError = NodeError err Nothing Nothing timestamp
      return $ NodeResult Failed Nothing (Just nodeError) HM.empty
    Right val ->
      return $ NodeResult Success (Just val) Nothing HM.empty

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

runNode :: MonadIO m => Node -> AgentState -> m NodeResult
runNode node agentState = liftIO $ do
  startTime <- getCurrentTime
  let context = NodeContext agentState (_nodeId node) (_nodeMetadata node) startTime
  runNodeWithContext node context

runNodeWithContext :: MonadIO m => Node -> NodeContext -> m NodeResult
runNodeWithContext node context = liftIO $ do
  result <- _nodeAction node context
  let executionInfo = HM.fromList
        [ ("last_executed_node",    String (_nodeId node))
        , ("execution_timestamp",   String (Text.pack $ show (_contextStartTime context)))
        , ("execution_status",      String (Text.pack $ show (_resultStatus result)))
        ]
  updateState (_contextState context) executionInfo Replace (Just (_nodeId node))
  return result

sequenceNodes :: MonadIO m => [Node] -> AgentState -> m [NodeResult]
sequenceNodes nodes agentState = liftIO $ mapM (`runNode` agentState) nodes

parallelNodes :: MonadIO m => [Node] -> AgentState -> m [NodeResult]
parallelNodes nodes agentState = liftIO $ do
  asyncActions <- mapM (\node -> async (runNode node agentState)) nodes
  mapM wait asyncActions

conditionalNode :: NodeId
                -> Text
                -> (HashMap Text Value -> Bool)
                -> Node
                -> Maybe Node
                -> Node
conditionalNode nid name predicate trueNode maybeFalseNode =
  mkNode nid name $ \ctx -> do
    currentState <- getState (_contextState ctx)
    if predicate currentState
      then runNodeWithContext trueNode ctx
      else case maybeFalseNode of
        Just falseNode -> runNodeWithContext falseNode ctx
        Nothing        -> return $ NodeResult Success Nothing Nothing HM.empty

retryNode :: NodeId -> Text -> Int -> Node -> Node
retryNode nid name maxRetries originalNode = mkNode nid name $ \ctx ->
  retryAction ctx 0
  where
    retryAction :: NodeContext -> Int -> IO NodeResult
    retryAction ctx' attempt
      | attempt >= maxRetries = do
          timestamp <- getCurrentTime
          let nodeError = NodeError
                "Maximum retries exceeded"
                (Just "MAX_RETRIES")
                (Just $ Number $ fromIntegral maxRetries)
                timestamp
          return $ NodeResult Failed Nothing (Just nodeError) HM.empty
      | otherwise = do
          result <- runNodeWithContext originalNode ctx'
          case _resultStatus result of
            Success -> return result
            _       -> retryAction ctx' (attempt + 1)

isSuccess :: NodeResult -> Bool
isSuccess = (== Success) . _resultStatus

isFailure :: NodeResult -> Bool
isFailure = (== Failed) . _resultStatus

isRunning :: NodeResult -> Bool
isRunning = (== Running) . _resultStatus

