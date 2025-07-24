{-# LANGUAGE OverloadedStrings #-}

module AIAgent.Examples.ChatBot
  ( -- * ChatBot Types
    ChatBot(..)
  , ChatSession(..)
  , ChatMessage(..)
  
    -- * ChatBot Creation
  , mkChatBot
  , mkSimpleChatBot
  , mkToolEnabledChatBot
  
    -- * Chat Operations
  , startChatSession
  , sendMessage
  , getChatHistory
  , clearChatHistory
  
    -- * Example ChatBots
  , simpleChatBot
  , calculatorChatBot
  , fileAssistantBot
  
    -- * Demo Functions
  , runSimpleChatDemo
  , runCalculatorChatDemo
  , runFileAssistantDemo
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time

import AIAgent.Core.State
import AIAgent.Agents.Base
import AIAgent.Tools.Base
import AIAgent.Tools.Function

-- | A chat message with metadata
data ChatMessage = ChatMessage
  { msgContent   :: Text
  , msgRole      :: Text  -- "user" or "assistant"
  , msgTimestamp :: UTCTime
  , msgMetadata  :: HashMap Text Value
  } deriving (Show, Eq)

-- | A chat session with history and state
data ChatSession = ChatSession
  { sessionId      :: Text
  , sessionHistory :: [ChatMessage]
  , sessionMemory  :: AgentMemory
  , sessionActive  :: Bool
  } deriving (Show)

-- | A chatbot with conversation capabilities
data ChatBot = ChatBot
  { botAgent       :: Agent
  , botTools       :: [Tool]
  , botPersonality :: HashMap Text Value
  , botSystemPrompt :: Maybe Text
  } deriving (Show)

-- | Create a new chatbot
mkChatBot :: Agent -> [Tool] -> HashMap Text Value -> Maybe Text -> ChatBot
mkChatBot agent tools personality systemPrompt = ChatBot
  { botAgent = agent
  , botTools = tools
  , botPersonality = personality
  , botSystemPrompt = systemPrompt
  }

-- | Create a simple chatbot without tools
mkSimpleChatBot :: Text -> Text -> ChatBot
mkSimpleChatBot name description = 
  let agent = mkStatefulAgent name [TextGeneration, Communication] $ \memory input -> do
        case HM.lookup "message" input of
          Just (String userMessage) -> do
            -- Simple echo bot with some personality
            let response = "I received your message: \"" <> userMessage <> 
                          "\". How can I help you further?"
            return $ Right $ object ["response" .= response]
          _ -> return $ Left "No message provided"
      personality = HM.fromList 
        [ ("name", String name)
        , ("description", String description)
        , ("friendly", Bool True)
        ]
  in mkChatBot agent [] personality Nothing

-- | Create a tool-enabled chatbot
mkToolEnabledChatBot :: Text -> [Tool] -> ChatBot
mkToolEnabledChatBot name tools = 
  let agent = mkStatefulAgent name [TextGeneration, Communication, ToolUsage] $ \memory input -> do
        case HM.lookup "message" input of
          Just (String userMessage) -> do
            -- Simple tool routing based on message content
            if "calculate" `Text.isInfixOf` Text.toLower userMessage || 
               "math" `Text.isInfixOf` Text.toLower userMessage
              then handleMathRequest userMessage
              else do
                let response = "I understand you said: " <> userMessage <> 
                              ". I have " <> Text.pack (show (length tools)) <> 
                              " tools available to help you."
                return $ Right $ object ["response" .= response]
          _ -> return $ Left "No message provided"
      personality = HM.fromList [("name", String name)]
  in mkChatBot agent tools personality Nothing
  where
    handleMathRequest :: Text -> IO (Either Text Value)
    handleMathRequest msg = do
      -- Simple parsing for math operations
      if "+" `Text.isInfixOf` msg
        then return $ Right $ object ["response" .= ("I can help with addition! " <> msg)]
        else return $ Right $ object ["response" .= ("I can help with math, but I need clearer instructions.")]

-- | Start a new chat session
startChatSession :: ChatBot -> IO ChatSession
startChatSession bot = do
  sessionId <- fmap (Text.pack . show) getCurrentTime
  memory <- newAgentMemory
  
  -- Add system prompt to memory if available
  case botSystemPrompt bot of
    Just prompt -> do
      timestamp <- getCurrentTime
      let systemMsg = ChatMessage prompt "system" timestamp HM.empty
      addToMemory memory (toJSON systemMsg)
    Nothing -> return ()
  
  return $ ChatSession sessionId [] memory True

-- | Send a message to the chatbot
sendMessage :: ChatBot -> ChatSession -> Text -> IO (Either Text (Text, ChatSession))
sendMessage bot session userMessage = do
  if not (sessionActive session)
    then return $ Left "Chat session is not active"
    else do
      timestamp <- getCurrentTime
      let userMsg = ChatMessage userMessage "user" timestamp HM.empty
      
      -- Add user message to memory
      addToMemory (sessionMemory session) (toJSON userMsg)
      
      -- Prepare input for agent
      let input = HM.fromList 
            [ ("message", String userMessage)
            , ("session_id", String (sessionId session))
            , ("history_length", Number $ fromIntegral $ length $ sessionHistory session)
            ]
      
      -- Get response from agent
      result <- runAgentWithMemory (botAgent bot) (sessionMemory session) input
      case result of
        Left err -> return $ Left err
        Right responseValue -> do
          case responseValue of
            Object obj -> case HM.lookup "response" obj of
              Just (String responseText) -> do
                responseTimestamp <- getCurrentTime
                let assistantMsg = ChatMessage responseText "assistant" responseTimestamp HM.empty
                
                -- Add assistant message to memory
                addToMemory (sessionMemory session) (toJSON assistantMsg)
                
                -- Update session history
                let newHistory = sessionHistory session ++ [userMsg, assistantMsg]
                    newSession = session { sessionHistory = newHistory }
                
                return $ Right (responseText, newSession)
              _ -> return $ Left "Invalid response format from agent"
            _ -> return $ Left "Agent returned non-object response"

-- | Get chat history
getChatHistory :: ChatSession -> [ChatMessage]
getChatHistory = sessionHistory

-- | Clear chat history
clearChatHistory :: ChatSession -> IO ChatSession
clearChatHistory session = do
  clearMemory (sessionMemory session)
  return $ session { sessionHistory = [] }

-- | Simple chatbot example
simpleChatBot :: ChatBot
simpleChatBot = mkSimpleChatBot "SimpleBot" "A friendly assistant that echoes your messages"

-- | Calculator chatbot with math tools
calculatorChatBot :: ChatBot
calculatorChatBot = 
  let mathToolList = map funcTool mathTools
      agent = mkStatefulAgent "CalculatorBot" [TextGeneration, Computation, ToolUsage] $ \memory input -> do
        case HM.lookup "message" input of
          Just (String userMessage) -> do
            let lowerMsg = Text.toLower userMessage
            if any (`Text.isInfixOf` lowerMsg) ["add", "plus", "+", "sum"]
              then handleMathOperation "add" userMessage
              else if any (`Text.isInfixOf` lowerMsg) ["subtract", "minus", "-"]
                then handleMathOperation "subtract" userMessage
                else if any (`Text.isInfixOf` lowerMsg) ["multiply", "times", "*"]
                  then handleMathOperation "multiply" userMessage
                  else if any (`Text.isInfixOf` lowerMsg) ["divide", "/"]
                    then handleMathOperation "divide" userMessage
                    else return $ Right $ object 
                      ["response" .= ("I'm a calculator bot! Try asking me to add, subtract, multiply, or divide numbers." :: Text)]
          _ -> return $ Left "No message provided"
  in mkChatBot agent mathToolList HM.empty (Just "You are a helpful calculator assistant.")
  where
    handleMathOperation :: Text -> Text -> IO (Either Text Value)
    handleMathOperation op msg = do
      -- Simple number extraction (this is very basic)
      let numbers = extractNumbers msg
      case numbers of
        [a, b] -> do
          let result = case op of
                "add" -> a + b
                "subtract" -> a - b
                "multiply" -> a * b
                "divide" -> if b /= 0 then a / b else 0
                _ -> 0
          return $ Right $ object 
            ["response" .= (Text.pack (show a) <> " " <> op <> " " <> Text.pack (show b) <> " = " <> Text.pack (show result))]
        _ -> return $ Right $ object 
          ["response" .= ("I need two numbers to perform " <> op <> ". Please provide them in your message." :: Text)]
    
    extractNumbers :: Text -> [Double]
    extractNumbers text = 
      let words = Text.words text
          numberWords = filter (Text.all (\c -> c `elem` ("0123456789.-" :: String))) words
      in [read (Text.unpack w) | w <- numberWords, not (Text.null w)]

-- | File assistant chatbot with file tools
fileAssistantBot :: ChatBot
fileAssistantBot = 
  let fileToolList = map funcTool fileTools
      agent = mkStatefulAgent "FileAssistant" [TextGeneration, DataRetrieval, ToolUsage] $ \memory input -> do
        case HM.lookup "message" input of
          Just (String userMessage) -> do
            let lowerMsg = Text.toLower userMessage
            if "read" `Text.isInfixOf` lowerMsg && "file" `Text.isInfixOf` lowerMsg
              then return $ Right $ object ["response" .= ("I can help you read files! Please specify the file path." :: Text)]
              else if "exists" `Text.isInfixOf` lowerMsg
                then return $ Right $ object ["response" .= ("I can check if files exist! What file would you like me to check?" :: Text)]
                else return $ Right $ object ["response" .= ("I'm a file assistant! I can read files, check if they exist, and more." :: Text)]
          _ -> return $ Left "No message provided"
  in mkChatBot agent fileToolList HM.empty (Just "You are a helpful file assistant.")

-- | Run simple chatbot demo
runSimpleChatDemo :: IO ()
runSimpleChatDemo = do
  putStrLn "=== Simple ChatBot Demo ==="
  session <- startChatSession simpleChatBot
  
  -- Simulate conversation
  putStrLn "User: Hello there!"
  result1 <- sendMessage simpleChatBot session "Hello there!"
  case result1 of
    Right (response, newSession) -> do
      putStrLn $ "Bot: " ++ Text.unpack response
      
      putStrLn "User: How are you today?"
      result2 <- sendMessage simpleChatBot newSession "How are you today?"
      case result2 of
        Right (response2, finalSession) -> do
          putStrLn $ "Bot: " ++ Text.unpack response2
          putStrLn $ "Total messages in history: " ++ show (length $ getChatHistory finalSession)
        Left err -> putStrLn $ "Error: " ++ Text.unpack err
    Left err -> putStrLn $ "Error: " ++ Text.unpack err

-- | Run calculator chatbot demo
runCalculatorChatDemo :: IO ()
runCalculatorChatDemo = do
  putStrLn "\n=== Calculator ChatBot Demo ==="
  session <- startChatSession calculatorChatBot
  
  -- Simulate math conversation
  let mathQuestions = 
        [ "Can you add 15 and 25?"
        , "What is 100 minus 37?"
        , "Multiply 7 and 8 please"
        , "Divide 144 by 12"
        ]
  
  finalSession <- foldM (\sess question -> do
    putStrLn $ "User: " ++ Text.unpack question
    result <- sendMessage calculatorChatBot sess question
    case result of
      Right (response, newSess) -> do
        putStrLn $ "Bot: " ++ Text.unpack response
        return newSess
      Left err -> do
        putStrLn $ "Error: " ++ Text.unpack err
        return sess
  ) session mathQuestions
  
  putStrLn $ "Calculator chat completed with " ++ 
             show (length $ getChatHistory finalSession) ++ " messages."

-- | Run file assistant demo
runFileAssistantDemo :: IO ()
runFileAssistantDemo = do
  putStrLn "\n=== File Assistant Demo ==="
  session <- startChatSession fileAssistantBot
  
  -- Simulate file-related conversation
  putStrLn "User: Can you help me with files?"
  result1 <- sendMessage fileAssistantBot session "Can you help me with files?"
  case result1 of
    Right (response, newSession) -> do
      putStrLn $ "Bot: " ++ Text.unpack response
      
      putStrLn "User: Does the file README.md exist?"
      result2 <- sendMessage fileAssistantBot newSession "Does the file README.md exist?"
      case result2 of
        Right (response2, _) -> putStrLn $ "Bot: " ++ Text.unpack response2
        Left err -> putStrLn $ "Error: " ++ Text.unpack err
    Left err -> putStrLn $ "Error: " ++ Text.unpack err

-- Required imports
import Control.Monad (foldM)