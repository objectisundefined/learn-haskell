{-# LANGUAGE OverloadedStrings #-}

module AIAgent.Tools.Base
  ( -- * Tool Types
    Tool(..)
  , ToolConfig(..)
  , ToolResult(..)
  , ToolError(..)
  
    -- * Tool Creation
  , mkTool
  , mkSyncTool
  , mkAsyncTool
  
    -- * Tool Operations
  , executeTool
  , executeToolWithTimeout
  
    -- * Tool Utilities
  , toolToAgent
  , combineTools
  ) where

import Control.Concurrent.Async
import Control.Exception
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Time

import AIAgent.Agents.Base

-- | Configuration for tools
data ToolConfig = ToolConfig
  { toolTimeout     :: Maybe Int  -- ^ Timeout in seconds
  , toolRetries     :: Int
  , toolDescription :: Maybe Text
  } deriving (Show, Eq)

-- | Tool execution errors
data ToolError = ToolError
  { toolErrorMessage :: Text
  , toolErrorCode    :: Maybe Text
  , toolErrorTime    :: UTCTime
  } deriving (Show, Eq)

-- | Result of tool execution
data ToolResult = ToolResult
  { toolResultData   :: Value
  , toolResultStatus :: Text
  , toolResultTime   :: UTCTime
  } deriving (Show, Eq)

-- | A tool that can be executed with input parameters
data Tool = Tool
  { toolName   :: Text
  , toolConfig :: ToolConfig
  , toolAction :: HashMap Text Value -> IO (Either ToolError ToolResult)
  }

instance Show Tool where
  show tool = "Tool { name = " ++ show (toolName tool) ++ " }"

-- | Default tool configuration
defaultToolConfig :: ToolConfig
defaultToolConfig = ToolConfig
  { toolTimeout = Just 30
  , toolRetries = 3
  , toolDescription = Nothing
  }

-- | Create a new tool
mkTool :: Text -> (HashMap Text Value -> IO (Either ToolError ToolResult)) -> Tool
mkTool name action = Tool
  { toolName = name
  , toolConfig = defaultToolConfig
  , toolAction = action
  }

-- | Create a synchronous tool from a pure function
mkSyncTool :: Text -> (HashMap Text Value -> Either Text Value) -> Tool
mkSyncTool name func = mkTool name $ \input -> do
  timestamp <- getCurrentTime
  case func input of
    Left err -> return $ Left $ ToolError err Nothing timestamp
    Right result -> return $ Right $ ToolResult result "success" timestamp

-- | Create an asynchronous tool
mkAsyncTool :: Text -> (HashMap Text Value -> IO (Either Text Value)) -> Tool
mkAsyncTool name action = mkTool name $ \input -> do
  timestamp <- getCurrentTime
  result <- action input
  case result of
    Left err -> return $ Left $ ToolError err Nothing timestamp
    Right value -> return $ Right $ ToolResult value "success" timestamp

-- | Execute a tool with input parameters
executeTool :: Tool -> HashMap Text Value -> IO (Either ToolError ToolResult)
executeTool tool input = do
  let retries = toolRetries (toolConfig tool)
  executeWithRetries (toolAction tool) input retries
  where
    executeWithRetries action input retryCount
      | retryCount <= 0 = do
          timestamp <- getCurrentTime
          return $ Left $ ToolError "Maximum retries exceeded" (Just "MAX_RETRIES") timestamp
      | otherwise = do
          result <- try (action input)
          case result of
            Left (ex :: SomeException) -> do
              timestamp <- getCurrentTime
              if retryCount > 1
                then executeWithRetries action input (retryCount - 1)
                else return $ Left $ ToolError (Text.pack $ show ex) (Just "EXCEPTION") timestamp
            Right success -> return success

-- | Execute a tool with a timeout
executeToolWithTimeout :: Tool -> HashMap Text Value -> IO (Either ToolError ToolResult)
executeToolWithTimeout tool input = do
  case toolTimeout (toolConfig tool) of
    Nothing -> executeTool tool input
    Just timeoutSecs -> do
      result <- race (threadDelay (timeoutSecs * 1000000)) (executeTool tool input)
      case result of
        Left _ -> do
          timestamp <- getCurrentTime
          return $ Left $ ToolError "Tool execution timed out" (Just "TIMEOUT") timestamp
        Right toolResult -> return toolResult

-- | Convert a tool to an agent
toolToAgent :: Tool -> Agent
toolToAgent tool = mkStatelessAgent (toolName tool) [ToolUsage] $ \input -> do
  result <- executeTool tool input
  case result of
    Left err -> return $ Left (toolErrorMessage err)
    Right success -> return $ Right (toolResultData success)

-- | Combine multiple tools into a single agent that can route to them
combineTools :: [Tool] -> Agent
combineTools tools = mkStatelessAgent "CombinedTools" [ToolUsage] $ \input -> do
  case HM.lookup "tool" input of
    Just (String toolName) -> 
      case findTool toolName tools of
        Just tool -> do
          result <- executeTool tool input
          case result of
            Left err -> return $ Left (toolErrorMessage err)
            Right success -> return $ Right (toolResultData success)
        Nothing -> return $ Left $ "Tool not found: " <> toolName
    _ -> return $ Left "No tool specified in input"
  where
    findTool :: Text -> [Tool] -> Maybe Tool
    findTool name = find (\t -> toolName t == name)
    
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs

-- Required imports
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)