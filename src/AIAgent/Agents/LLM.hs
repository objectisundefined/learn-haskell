{-# LANGUAGE OverloadedStrings #-}

module AIAgent.Agents.LLM
  ( -- * LLM Agent Types
    LLMAgent(..)
  , LLMConfig(..)
  , LLMProvider(..)
  , LLMHandler

    -- * LLM Agent Creation
  , mkLLMAgent
  , mkCustomLLMAgent

    -- * LLM Operations
  , generateText
  , chatCompletion
  , embedding

    -- * Handlers
  , noopLLMHandler

    -- * Utilities
  , defaultLLMConfig
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Vector as V

import AIAgent.Agents.Base

-- | A pluggable request handler that performs the actual LLM backend call.
-- Receives the agent configuration and request payload; returns either an
-- error message or the JSON response from the backend.
type LLMHandler = LLMConfig -> HashMap Text Value -> IO (Either Text Value)

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

-- | LLM Agent wrapper combining configuration, the base agent, and the
-- backend handler used for all LLM operations.
data LLMAgent = LLMAgent
  { _llmConfig    :: LLMConfig
  , _llmBaseAgent :: Agent
  , _llmHandler   :: LLMHandler
  }

instance Show LLMAgent where
  show agent = "LLMAgent { config = " ++ show (_llmConfig agent)
            ++ ", agent = " ++ show (_llmBaseAgent agent) ++ " }"

-- | Default handler that always returns an error instructing the caller to
-- supply a real backend handler via 'mkCustomLLMAgent'.
noopLLMHandler :: LLMHandler
noopLLMHandler _config _input =
  return $ Left "No LLM backend configured. Provide a custom handler via mkCustomLLMAgent."

-- | Default LLM configuration
defaultLLMConfig :: LLMConfig
defaultLLMConfig = LLMConfig
  { llmProvider    = OpenAI
  , llmModel       = "gpt-3.5-turbo"
  , llmTemperature = 0.7
  , llmMaxTokens   = 1000
  , llmApiKey      = Nothing
  , llmBaseUrl     = Nothing
  }

-- | Create an LLM agent with the given handler.
-- The handler is invoked for every request routed through the base 'Agent'.
mkLLMAgent :: LLMConfig -> Text -> LLMHandler -> LLMAgent
mkLLMAgent config name handler =
  let baseAgent = mkStatefulAgent name [TextGeneration, Reasoning, CreativeThinking] $
        \_memory input -> handler config input
  in LLMAgent
       { _llmConfig    = config
       , _llmBaseAgent = baseAgent
       , _llmHandler   = handler
       }

-- | Create an LLM agent backed by a caller-supplied handler.
-- This is the primary way to wire in a real API client (OpenAI, Anthropic,
-- a local model server, etc.).
mkCustomLLMAgent :: LLMConfig -> Text -> LLMHandler -> LLMAgent
mkCustomLLMAgent = mkLLMAgent

-- | Generate text using the LLM.
generateText :: LLMAgent -> Text -> IO (Either Text Text)
generateText agent prompt = do
  let input = HM.fromList [("prompt", String prompt)]
  result <- runAgent (_llmBaseAgent agent) input
  extractTextField "response" result

-- | Perform chat completion.
chatCompletion :: LLMAgent -> [Text] -> IO (Either Text Text)
chatCompletion agent messages = do
  let msgArray = Array $ V.fromList (map String messages)
      input    = HM.fromList [("messages", msgArray)]
  result <- runAgent (_llmBaseAgent agent) input
  extractTextField "response" result

-- | Generate embeddings via the configured handler.
-- The handler is expected to return a JSON object containing an
-- @\"embedding\"@ key whose value is an array of numbers.
embedding :: LLMAgent -> Text -> IO (Either Text [Double])
embedding agent text = do
  let input = HM.fromList [("text", String text), ("operation", String "embedding")]
  result <- (_llmHandler agent) (_llmConfig agent) input
  case result of
    Left err -> return $ Left err
    Right (Object obj) ->
      case KM.lookup (fromText "embedding") obj of
        Just (Array arr) -> return $ parseDoubles (V.toList arr)
        _ -> return $ Left "Response missing \"embedding\" array"
    Right _ -> return $ Left "Invalid response type from handler"

-- | Extract a text field from an agent result.
extractTextField :: Text -> Either Text Value -> IO (Either Text Text)
extractTextField key result = return $ case result of
  Right (Object obj) ->
    case KM.lookup (fromText key) (obj) of
      Just (String val) -> Right val
      _                 -> Left $ "Response missing \"" <> key <> "\" text field"
  Right _ -> Left "Invalid response type"
  Left err -> Left err

-- | Try to parse a list of JSON values as Doubles.
parseDoubles :: [Value] -> Either Text [Double]
parseDoubles = mapM parseOne
  where
    parseOne (Number n) = Right (realToFrac n)
    parseOne _          = Left "Embedding array contains non-numeric value"