{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AIAgent.Core.State
  ( -- * State Types
    AgentState(..)
  , StateSnapshot(..)
  , StateHistory
  , StateUpdateMode(..)
  
    -- * State Operations
  , newAgentState
  , getState
  , setState
  , updateState
  , mergeState
  , appendState
  
    -- * History and Snapshots
  , takeSnapshot
  , rollbackToSnapshot
  , getHistory
  , clearHistory
  
    -- * Lenses
  , stateData
  , stateHistory
  , snapshotId
  , snapshotTimestamp
  , snapshotData
  , snapshotNodeId
  
    -- * JSON Serialization
  , stateToJSON
  , stateFromJSON
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (unless)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

-- | Defines how state updates should be handled
data StateUpdateMode
  = Merge     -- ^ Merge new values with existing state
  | Replace   -- ^ Replace entire state
  | Append    -- ^ Append to existing values (for lists/collections)
  deriving (Show, Eq)

-- | A snapshot of the state at a specific point in time
data StateSnapshot = StateSnapshot
  { _snapshotId        :: UUID
  , _snapshotTimestamp :: UTCTime
  , _snapshotData      :: HashMap Text Value
  , _snapshotNodeId    :: Maybe Text
  } deriving (Show, Eq)

-- | Complete state history as a list of snapshots
type StateHistory = [StateSnapshot]

-- | The main agent state container with STM support for concurrency
data AgentState = AgentState
  { _stateData    :: TVar (HashMap Text Value)
  , _stateHistory :: TVar StateHistory
  }

-- Generate lenses
makeLenses ''StateSnapshot
makeLenses ''AgentState

-- | Create a new agent state with optional initial data
newAgentState :: MonadIO m => Maybe (HashMap Text Value) -> m AgentState
newAgentState initialData = liftIO $ do
  let initData = maybe HM.empty id initialData
  dataVar <- newTVarIO initData
  historyVar <- newTVarIO []
  let state = AgentState dataVar historyVar
  
  -- Take initial snapshot if we have data
  unless (HM.null initData) $ do
    _ <- takeSnapshot state Nothing
    return ()
  
  return state

-- | Get current state data
getState :: MonadIO m => AgentState -> m (HashMap Text Value)
getState agentState = liftIO $ readTVarIO (_stateData agentState)

-- | Set a single key-value pair in the state
setState :: MonadIO m => AgentState -> Text -> Value -> StateUpdateMode -> Maybe Text -> m ()
setState agentState key value mode nodeId = liftIO $ atomically $ do
  currentData <- readTVar (_stateData agentState)
  
  let newData = case mode of
        Replace -> HM.insert key value currentData
        Merge -> case (HM.lookup key currentData, value) of
          (Just (Object existing), Object new) -> 
            HM.insert key (Object $ existing <> new) currentData
          _ -> HM.insert key value currentData
        Append -> case HM.lookup key currentData of
          Just (Array existing) -> case value of
            Array new -> HM.insert key (Array $ existing <> new) currentData
            single -> HM.insert key (Array $ existing <> [single]) currentData
          Just existing -> HM.insert key (Array [existing, value]) currentData
          Nothing -> HM.insert key (Array [value]) currentData
  
  writeTVar (_stateData agentState) newData

-- | Update state with multiple key-value pairs
updateState :: MonadIO m => AgentState -> HashMap Text Value -> StateUpdateMode -> Maybe Text -> m ()
updateState agentState updates mode nodeId = liftIO $ do
  mapM_ (\(k, v) -> setState agentState k v mode nodeId) (HM.toList updates)
  _ <- takeSnapshot agentState nodeId
  return ()

-- | Merge another state into the current state
mergeState :: MonadIO m => AgentState -> HashMap Text Value -> Maybe Text -> m ()
mergeState agentState otherState nodeId = 
  updateState agentState otherState Merge nodeId

-- | Append values to the current state
appendState :: MonadIO m => AgentState -> HashMap Text Value -> Maybe Text -> m ()
appendState agentState newValues nodeId = 
  updateState agentState newValues Append nodeId

-- | Take a snapshot of the current state
takeSnapshot :: MonadIO m => AgentState -> Maybe Text -> m StateSnapshot
takeSnapshot agentState nodeId = liftIO $ do
  snapshotId <- UUID.nextRandom
  timestamp <- getCurrentTime
  currentData <- readTVarIO (_stateData agentState)
  
  let snapshot = StateSnapshot snapshotId timestamp currentData nodeId
  
  atomically $ do
    history <- readTVar (_stateHistory agentState)
    writeTVar (_stateHistory agentState) (snapshot : history)
  
  return snapshot

-- | Rollback to a specific snapshot
rollbackToSnapshot :: MonadIO m => AgentState -> UUID -> m Bool
rollbackToSnapshot agentState targetId = liftIO $ atomically $ do
  history <- readTVar (_stateHistory agentState)
  case findSnapshot targetId history of
    Just snapshot -> do
      writeTVar (_stateData agentState) (_snapshotData snapshot)
      -- Remove all snapshots after this one
      let newHistory = takeWhile ((/= targetId) . _snapshotId) history ++ [snapshot]
      writeTVar (_stateHistory agentState) newHistory
      return True
    Nothing -> return False
  where
    findSnapshot :: UUID -> [StateSnapshot] -> Maybe StateSnapshot
    findSnapshot sid = find (\s -> _snapshotId s == sid)

-- | Get the complete state history
getHistory :: MonadIO m => AgentState -> m StateHistory
getHistory agentState = liftIO $ readTVarIO (_stateHistory agentState)

-- | Clear the state history
clearHistory :: MonadIO m => AgentState -> m ()
clearHistory agentState = liftIO $ atomically $ do
  writeTVar (_stateHistory agentState) []

-- | Convert state to JSON
stateToJSON :: MonadIO m => AgentState -> m Value
stateToJSON agentState = do
  currentData <- getState agentState
  return $ Object currentData

-- | Load state from JSON
stateFromJSON :: MonadIO m => AgentState -> Value -> m Bool
stateFromJSON agentState (Object obj) = do
  liftIO $ atomically $ writeTVar (_stateData agentState) obj
  _ <- takeSnapshot agentState (Just "loaded_from_json")
  return True
stateFromJSON _ _ = return False

-- Utility function for finding elements in lists
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x then Just x else find p xs