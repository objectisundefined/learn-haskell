{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AIAgent.Agents.Base
  ( -- * Agent Types
    Agent(..)
  , AgentConfig(..)
  , AgentCapability(..)
  , AgentMemory(..)
  
    -- * Agent Creation
  , mkAgent
  , mkStatelessAgent
  , mkStatefulAgent
  
    -- * Agent Operations
  , runAgent
  , runAgentWithMemory
  , chainAgents
  , parallelAgents
  
    -- * Agent Combinators
  , retryAgent
  , timeoutAgent
  , fallbackAgent
  , conditionalAgent
  
    -- * Memory Management
  , newAgentMemory
  , addToMemory
  , getFromMemory
  , clearMemory
  
    -- * Lenses
  , agentName
  , agentDescription
  , agentCapabilities
  , agentConfig
  , agentAction
  , memoryShortTerm
  , memoryLongTerm
  , memoryWorkingSet
  
    -- * Utilities
  , hasCapability
  , getAgentInfo
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

import AIAgent.Core.State
import AIAgent.Core.Node

-- | Agent capabilities that describe what an agent can do
data AgentCapability
  = TextGeneration    -- ^ Can generate text/responses
  | DataRetrieval     -- ^ Can retrieve and search data
  | Computation       -- ^ Can perform calculations
  | DecisionMaking    -- ^ Can make decisions based on criteria
  | Planning          -- ^ Can create plans and strategies
  | Learning          -- ^ Can learn from interactions
  | ToolUsage         -- ^ Can use external tools
  | Communication     -- ^ Can communicate with other agents
  | Reasoning         -- ^ Can perform logical reasoning
  | CreativeThinking  -- ^ Can think creatively and generate ideas
  deriving (Show, Eq, Ord, Enum)

-- | Configuration for agent behavior
data AgentConfig = AgentConfig
  { _configMaxRetries    :: Int
  , _configTimeout       :: Maybe Int  -- ^ Timeout in seconds
  , _configVerbose       :: Bool
  , _configMemoryEnabled :: Bool
  , _configTemperature   :: Maybe Double  -- ^ For LLM agents
  , _configMaxTokens     :: Maybe Int     -- ^ For LLM agents
  } deriving (Show, Eq)

-- | Agent memory system for maintaining context and learning
data AgentMemory = AgentMemory
  { _memoryShortTerm  :: TVar [Value]         -- ^ Recent interactions
  , _memoryLongTerm   :: TVar (HashMap Text Value)  -- ^ Persistent knowledge
  , _memoryWorkingSet :: TVar (HashMap Text Value)  -- ^ Current working context
  } deriving (Show)

-- | The main agent interface
data Agent = Agent
  { _agentName         :: Text
  , _agentDescription  :: Maybe Text
  , _agentCapabilities :: [AgentCapability]
  , _agentConfig       :: AgentConfig
  , _agentAction       :: AgentMemory -> HashMap Text Value -> IO (Either Text Value)
  }

-- Generate lenses
makeLenses ''AgentConfig
makeLenses ''AgentMemory
makeLenses ''Agent

instance Show Agent where
  show agent = "Agent { name = " ++ Text.unpack (_agentName agent) ++ 
               ", capabilities = " ++ show (_agentCapabilities agent) ++ " }"

-- | Default agent configuration
defaultAgentConfig :: AgentConfig
defaultAgentConfig = AgentConfig
  { _configMaxRetries = 3
  , _configTimeout = Just 30
  , _configVerbose = False
  , _configMemoryEnabled = True
  , _configTemperature = Nothing
  , _configMaxTokens = Nothing
  }

-- | Create a new agent with the given action
mkAgent :: Text 
        -> [AgentCapability] 
        -> (AgentMemory -> HashMap Text Value -> IO (Either Text Value))
        -> Agent
mkAgent name capabilities action = Agent
  { _agentName = name
  , _agentDescription = Nothing
  , _agentCapabilities = capabilities
  , _agentConfig = defaultAgentConfig
  , _agentAction = action
  }

-- | Create a stateless agent (ignores memory)
mkStatelessAgent :: Text 
                 -> [AgentCapability]
                 -> (HashMap Text Value -> IO (Either Text Value))
                 -> Agent
mkStatelessAgent name capabilities action = 
  mkAgent name capabilities (\_ input -> action input)

-- | Create a stateful agent that uses memory
mkStatefulAgent :: Text
                -> [AgentCapability]
                -> (AgentMemory -> HashMap Text Value -> IO (Either Text Value))
                -> Agent
mkStatefulAgent = mkAgent

-- | Create new agent memory
newAgentMemory :: MonadIO m => m AgentMemory
newAgentMemory = liftIO $ do
  shortTerm <- newTVarIO []
  longTerm <- newTVarIO HM.empty
  workingSet <- newTVarIO HM.empty
  return $ AgentMemory shortTerm longTerm workingSet

-- | Add information to short-term memory
addToMemory :: MonadIO m => AgentMemory -> Value -> m ()
addToMemory memory value = liftIO $ atomically $ do
  current <- readTVar (_memoryShortTerm memory)
  let updated = take 100 (value : current)  -- Keep last 100 items
  writeTVar (_memoryShortTerm memory) updated

-- | Get value from long-term memory
getFromMemory :: MonadIO m => AgentMemory -> Text -> m (Maybe Value)
getFromMemory memory key = liftIO $ do
  longTerm <- readTVarIO (_memoryLongTerm memory)
  return $ HM.lookup key longTerm

-- | Clear all memory
clearMemory :: MonadIO m => AgentMemory -> m ()
clearMemory memory = liftIO $ atomically $ do
  writeTVar (_memoryShortTerm memory) []
  writeTVar (_memoryLongTerm memory) HM.empty
  writeTVar (_memoryWorkingSet memory) HM.empty

-- | Run an agent with the given input
runAgent :: MonadIO m => Agent -> HashMap Text Value -> m (Either Text Value)
runAgent agent input = liftIO $ do
  memory <- newAgentMemory
  runAgentWithMemory agent memory input

-- | Run an agent with existing memory
runAgentWithMemory :: MonadIO m => Agent -> AgentMemory -> HashMap Text Value -> m (Either Text Value)
runAgentWithMemory agent memory input = liftIO $ do
  let config = _agentConfig agent
      action = _agentAction agent
      maxRetries = _configMaxRetries config
  
  -- Add input to working set
  atomically $ modifyTVar (_memoryWorkingSet memory) (HM.union input)
  
  -- Execute with retries
  executeWithRetries action memory input maxRetries
  where
    executeWithRetries :: (AgentMemory -> HashMap Text Value -> IO (Either Text Value))
                       -> AgentMemory
                       -> HashMap Text Value
                       -> Int
                       -> IO (Either Text Value)
    executeWithRetries action memory input retries
      | retries <= 0 = return $ Left "Maximum retries exceeded"
      | otherwise = do
          result <- try (action memory input)
          case result of
            Left (ex :: SomeException) -> 
              if retries > 1
                then executeWithRetries action memory input (retries - 1)
                else return $ Left $ Text.pack $ show ex
            Right (Left err) ->
              if retries > 1
                then executeWithRetries action memory input (retries - 1)
                else return $ Left err
            Right success -> do
              -- Add result to memory if successful
              case success of
                Right value -> addToMemory memory value
                _ -> return ()
              return success

-- | Chain multiple agents together (output of one becomes input of next)
chainAgents :: MonadIO m => [Agent] -> HashMap Text Value -> m (Either Text Value)
chainAgents [] input = return $ Right $ Object input
chainAgents (agent:rest) input = liftIO $ do
  result <- runAgent agent input
  case result of
    Left err -> return $ Left err
    Right (Object nextInput) -> chainAgents rest nextInput
    Right value -> 
      -- Convert single value to input for next agent
      let nextInput = HM.insert "input" value input
      in chainAgents rest nextInput

-- | Run multiple agents in parallel and combine results
parallelAgents :: MonadIO m => [Agent] -> HashMap Text Value -> m [Either Text Value]
parallelAgents agents input = liftIO $ do
  mapConcurrently (\agent -> runAgent agent input) agents

-- | Create a retry agent wrapper
retryAgent :: Int -> Agent -> Agent
retryAgent maxRetries agent = 
  agent & agentConfig . configMaxRetries .~ maxRetries

-- | Create a timeout agent wrapper (placeholder - would need async timeout)
timeoutAgent :: Int -> Agent -> Agent
timeoutAgent timeoutSeconds agent = 
  agent & agentConfig . configTimeout .~ Just timeoutSeconds

-- | Create a fallback agent that tries alternatives on failure
fallbackAgent :: Agent -> [Agent] -> Agent
fallbackAgent primaryAgent fallbackAgents = 
  mkAgent 
    ("Fallback(" <> _agentName primaryAgent <> ")")
    (_agentCapabilities primaryAgent)
    $ \memory input -> do
      result <- runAgentWithMemory primaryAgent memory input
      case result of
        Right value -> return $ Right value
        Left _ -> tryFallbacks fallbackAgents memory input
  where
    tryFallbacks :: [Agent] -> AgentMemory -> HashMap Text Value -> IO (Either Text Value)
    tryFallbacks [] _ _ = return $ Left "All fallback agents failed"
    tryFallbacks (fallback:rest) memory input = do
      result <- runAgentWithMemory fallback memory input
      case result of
        Right value -> return $ Right value
        Left _ -> tryFallbacks rest memory input

-- | Create a conditional agent that only runs if condition is met
conditionalAgent :: (HashMap Text Value -> Bool) -> Agent -> Agent
conditionalAgent condition agent = 
  mkAgent
    ("Conditional(" <> _agentName agent <> ")")
    (_agentCapabilities agent)
    $ \memory input -> 
      if condition input
        then runAgentWithMemory agent memory input
        else return $ Right $ String "Condition not met, skipping agent"

-- | Check if agent has a specific capability
hasCapability :: Agent -> AgentCapability -> Bool
hasCapability agent capability = capability `elem` _agentCapabilities agent

-- | Get agent information as JSON
getAgentInfo :: Agent -> Value
getAgentInfo agent = object
  [ "name" .= _agentName agent
  , "description" .= _agentDescription agent
  , "capabilities" .= map show (_agentCapabilities agent)
  , "config" .= object
      [ "maxRetries" .= _configMaxRetries (_agentConfig agent)
      , "timeout" .= _configTimeout (_agentConfig agent)
      , "verbose" .= _configVerbose (_agentConfig agent)
      ]
  ]

-- Utility import
import Control.Concurrent.Async (mapConcurrently)