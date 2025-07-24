{-# LANGUAGE OverloadedStrings #-}

module AIAgent.Agents.LLM
  ( -- * LLM Agent Types
    LLMAgent(..)
  , LLMConfig(..)
  , LLMProvider(..)
  
    -- * LLM Agent Creation
  , mkLLMAgent
  , mkOpenAIAgent
  , mkLocalLLMAgent
  
    -- * LLM Operations
  , generateText
  , chatCompletion
  , embedding
  
    -- * Utilities
  , defaultLLMConfig
  ) where

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text

import AIAgent.Agents.Base

-- | Supported LLM providers
data LLMProvider
  = OpenAI
  | Anthropic
  | Local Text  -- ^ Local model with name/path
  | Custom Text -- ^ Custom provider with identifier
  deriving (Show, Eq)

-- | Configuration for LLM agents
data LLMConfig = LLMConfig
  { llmProvider    :: LLMProvider
  , llmModel       :: Text
  , llmTemperature :: Double
  , llmMaxTokens   :: Int
  , llmApiKey      :: Maybe Text
  , llmBaseUrl     :: Maybe Text
  } deriving (Show, Eq)

-- | LLM Agent wrapper
data LLMAgent = LLMAgent
  { llmConfig :: LLMConfig
  , llmAgent  :: Agent
  } deriving (Show)

-- | Default LLM configuration
defaultLLMConfig :: LLMConfig
defaultLLMConfig = LLMConfig
  { llmProvider = OpenAI
  , llmModel = "gpt-3.5-turbo"
  , llmTemperature = 0.7
  , llmMaxTokens = 1000
  , llmApiKey = Nothing
  , llmBaseUrl = Nothing
  }

-- | Create a generic LLM agent
mkLLMAgent :: LLMConfig -> Text -> Agent
mkLLMAgent config name = 
  mkStatefulAgent name [TextGeneration, Reasoning, CreativeThinking] $ \memory input -> do
    -- This is a placeholder implementation
    -- In a real implementation, this would call the actual LLM API
    case HM.lookup "prompt" input of
      Just (String prompt) -> do
        let response = "LLM Response to: " <> prompt <> " (using " <> llmModel config <> ")"
        return $ Right $ object ["response" .= response]
      _ -> return $ Left "No prompt provided"

-- | Create an OpenAI agent
mkOpenAIAgent :: Maybe Text -> Text -> Agent
mkOpenAIAgent apiKey name = 
  let config = defaultLLMConfig { llmProvider = OpenAI, llmApiKey = apiKey }
  in mkLLMAgent config name

-- | Create a local LLM agent
mkLocalLLMAgent :: Text -> Text -> Agent
mkLocalLLMAgent modelPath name = 
  let config = defaultLLMConfig { llmProvider = Local modelPath }
  in mkLLMAgent config name

-- | Generate text using the LLM
generateText :: LLMAgent -> Text -> IO (Either Text Text)
generateText llmAgent prompt = do
  let input = HM.fromList [("prompt", String prompt)]
  result <- runAgent (llmAgent llmAgent) input
  case result of
    Right (Object obj) -> 
      case HM.lookup "response" obj of
        Just (String response) -> return $ Right response
        _ -> return $ Left "Invalid response format"
    Right _ -> return $ Left "Invalid response type"
    Left err -> return $ Left err

-- | Perform chat completion
chatCompletion :: LLMAgent -> [Text] -> IO (Either Text Text)
chatCompletion llmAgent messages = do
  let input = HM.fromList [("messages", Array $ map String messages)]
  result <- runAgent (llmAgent llmAgent) input
  case result of
    Right (Object obj) -> 
      case HM.lookup "response" obj of
        Just (String response) -> return $ Right response
        _ -> return $ Left "Invalid response format"
    Right _ -> return $ Left "Invalid response type"
    Left err -> return $ Left err

-- | Generate embeddings (placeholder)
embedding :: LLMAgent -> Text -> IO (Either Text [Double])
embedding _ text = do
  -- Placeholder implementation
  let fakeEmbedding = replicate 768 0.1  -- Typical embedding dimension
  return $ Right fakeEmbedding