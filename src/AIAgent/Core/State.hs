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

    -- * Conversion Helpers
  , hmToObject
  , objectToHm
  ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.Text (Text)
import Data.Time
import Data.Unique (newUnique, hashUnique)
import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- Conversion helpers between HashMap Text Value and aeson's Object (KeyMap)
-- ---------------------------------------------------------------------------

-- | Convert a 'HashMap Text Value' to an aeson 'Object' ('KeyMap Value').
hmToObject :: HashMap Text Value -> KM.KeyMap Value
hmToObject = KM.fromList . map (\(k, v) -> (fromText k, v)) . HM.toList

-- | Convert an aeson 'Object' ('KeyMap Value') back to a 'HashMap Text Value'.
objectToHm :: KM.KeyMap Value -> HashMap Text Value
objectToHm = HM.fromList . map (\(k, v) -> (toText k, v)) . KM.toList

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Defines how state updates should be handled
data StateUpdateMode
  = Merge     -- ^ Merge new values with existing state
  | Replace   -- ^ Replace entire state
  | Append    -- ^ Append to existing values (for lists/collections)
  deriving (Show, Eq)

-- | A snapshot of the state at a specific point in time
data StateSnapshot = StateSnapshot
  { _snapshotId        :: Int
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

-- ---------------------------------------------------------------------------
-- State Operations
-- ---------------------------------------------------------------------------

-- | Create a new agent state with optional initial data
newAgentState :: MonadIO m => Maybe (HashMap Text Value) -> m AgentState
newAgentState initialData = liftIO $ do
  let initData = maybe HM.empty id initialData
  dataVar    <- newTVarIO initData
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
setState agentState key value mode _nodeId = liftIO $ atomically $ do
  currentData <- readTVar (_stateData agentState)

  let newData = case mode of
        Replace -> HM.insert key value currentData
        Merge   -> case (HM.lookup key currentData, value) of
          (Just (Object existing), Object incoming) ->
            HM.insert key (Object $ existing <> incoming) currentData
          _ -> HM.insert key value currentData
        Append  -> case HM.lookup key currentData of
          Just (Array existing) -> case value of
            Array incoming -> HM.insert key (Array $ existing <> incoming) currentData
            single     -> HM.insert key (Array $ existing <> V.singleton single) currentData
          Just existing -> HM.insert key (Array $ V.fromList [existing, value]) currentData
          Nothing       -> HM.insert key (Array $ V.singleton value) currentData

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

-- ---------------------------------------------------------------------------
-- History and Snapshots
-- ---------------------------------------------------------------------------

-- | Take a snapshot of the current state
takeSnapshot :: MonadIO m => AgentState -> Maybe Text -> m StateSnapshot
takeSnapshot agentState nodeId = liftIO $ do
  uid       <- newUnique
  timestamp <- getCurrentTime
  currentData <- readTVarIO (_stateData agentState)

  let snapshot = StateSnapshot (hashUnique uid) timestamp currentData nodeId

  atomically $ do
    history <- readTVar (_stateHistory agentState)
    writeTVar (_stateHistory agentState) (snapshot : history)

  return snapshot

-- | Rollback to a specific snapshot by its Int identifier
rollbackToSnapshot :: MonadIO m => AgentState -> Int -> m Bool
rollbackToSnapshot agentState targetId = liftIO $ atomically $ do
  history <- readTVar (_stateHistory agentState)
  case find (\s -> _snapshotId s == targetId) history of
    Just snapshot -> do
      writeTVar (_stateData agentState) (_snapshotData snapshot)
      -- Keep only the target snapshot and everything before it
      let newHistory = takeWhile ((/= targetId) . _snapshotId) history ++ [snapshot]
      writeTVar (_stateHistory agentState) newHistory
      return True
    Nothing -> return False

-- | Get the complete state history
getHistory :: MonadIO m => AgentState -> m StateHistory
getHistory agentState = liftIO $ readTVarIO (_stateHistory agentState)

-- | Clear the state history
clearHistory :: MonadIO m => AgentState -> m ()
clearHistory agentState = liftIO $ atomically $
  writeTVar (_stateHistory agentState) []

-- ---------------------------------------------------------------------------
-- JSON Serialization
-- ---------------------------------------------------------------------------

-- | Convert state to a JSON 'Value'
stateToJSON :: MonadIO m => AgentState -> m Value
stateToJSON agentState = do
  currentData <- getState agentState
  return $ Object (hmToObject currentData)

-- | Load state from a JSON 'Value'. Returns 'True' on success.
stateFromJSON :: MonadIO m => AgentState -> Value -> m Bool
stateFromJSON agentState (Object obj) = do
  liftIO $ atomically $ writeTVar (_stateData agentState) (objectToHm obj)
  _ <- takeSnapshot agentState (Just "loaded_from_json")
  return True
stateFromJSON _ _ = return False