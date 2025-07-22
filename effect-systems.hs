{-
Effect Systems in Haskell
=========================

Demonstrates various approaches to handling side effects:
- Monad Transformers (MTL style)
- Exception handling with ExceptT
- Resource management patterns
- Effect composition and interpretation
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module EffectSystems where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map

-- =============================================================================
-- 1. Classic Monad Transformers (MTL Style)
-- =============================================================================

-- Application Configuration
data Config = Config
  { configDbUrl :: String
  , configApiKey :: String
  , configDebugMode :: Bool
  } deriving Show

-- Application State
data AppState = AppState
  { stateCache :: Map String String
  , stateRequestCount :: Int
  } deriving Show

-- Application Errors
data AppError
  = DatabaseError String
  | ValidationError String
  | NetworkError String
  deriving Show

-- Our main application monad stack
type App = ReaderT Config (StateT AppState (ExceptT AppError IO))

-- Helper to run our application
runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp config initialState action = 
  runExceptT $ runStateT (runReaderT action config) initialState

-- Basic operations in our effect stack
logMessage :: MonadIO m => String -> m ()
logMessage msg = liftIO $ putStrLn $ "[LOG] " ++ msg

incrementRequestCount :: (MonadState AppState m) => m ()
incrementRequestCount = modify $ \s -> s { stateRequestCount = stateRequestCount s + 1 }

getFromCache :: (MonadState AppState m) => String -> m (Maybe String)
getFromCache key = do
  cache <- gets stateCache
  return $ Map.lookup key cache

addToCache :: (MonadState AppState m) => String -> String -> m ()
addToCache key value = modify $ \s -> s { stateCache = Map.insert key value (stateCache s) }

isDebugMode :: (MonadReader Config m) => m Bool
isDebugMode = asks configDebugMode

-- Example business logic
fetchUserData :: String -> App String
fetchUserData userId = do
  logMessage $ "Fetching user data for: " ++ userId
  incrementRequestCount
  
  -- Check cache first
  cached <- getFromCache userId
  case cached of
    Just userData -> do
      logMessage "Found in cache"
      return userData
    Nothing -> do
      logMessage "Cache miss, fetching from DB"
      
      -- Simulate database call
      config <- ask
      debug <- isDebugMode
      when debug $ logMessage $ "Using DB URL: " ++ configDbUrl config
      
      -- Simulate potential failure
      if userId == "invalid"
        then throwError $ DatabaseError "Invalid user ID"
        else do
          let userData = "User data for " ++ userId
          addToCache userId userData
          return userData

-- =============================================================================
-- 2. Writer Monad for Logging
-- =============================================================================

type Logger = Writer [String]

-- Logging operations
logInfo :: String -> Logger ()
logInfo msg = tell ["INFO: " ++ msg]

logError :: String -> Logger ()
logError msg = tell ["ERROR: " ++ msg]

logDebug :: String -> Logger ()
logDebug msg = tell ["DEBUG: " ++ msg]

-- Example computation with logging
computeWithLogging :: Int -> Logger Int
computeWithLogging x = do
  logInfo $ "Starting computation with input: " ++ show x
  
  when (x < 0) $ logError "Negative input detected"
  
  let result = x * x + 10
  logDebug $ "Intermediate result: " ++ show result
  
  let finalResult = result `div` 2
  logInfo $ "Final result: " ++ show finalResult
  
  return finalResult

-- =============================================================================
-- 3. Exception Handling with ExceptT
-- =============================================================================

data ValidationError = InvalidEmail | TooShort | TooLong deriving Show

validateEmail :: String -> Either ValidationError String
validateEmail email
  | length email < 3 = Left TooShort
  | length email > 50 = Left TooLong
  | '@' `notElem` email = Left InvalidEmail
  | otherwise = Right email

validatePassword :: String -> Either ValidationError String
validatePassword pwd
  | length pwd < 8 = Left TooShort
  | length pwd > 100 = Left TooLong
  | otherwise = Right pwd

-- Combine validations using ExceptT
validateUser :: String -> String -> ExceptT ValidationError IO (String, String)
validateUser email password = do
  validEmail <- ExceptT $ return $ validateEmail email
  validPassword <- ExceptT $ return $ validatePassword password
  liftIO $ putStrLn "User validation successful"
  return (validEmail, validPassword)

main :: IO ()
main = do
  putStrLn "Effect Systems in Haskell"
  putStrLn "========================="
  
  -- Demo monad transformers
  putStrLn "=== Monad Transformers Demo ==="
  let config = Config "postgres://localhost" "secret-key" True
      initialState = AppState Map.empty 0
  
  result <- runApp config initialState (fetchUserData "user123")
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (value, finalState) -> do
      putStrLn $ "Result: " ++ value
      putStrLn $ "Final state: " ++ show finalState
  
  -- Demo logging
  putStrLn "\n=== Writer Monad Logging Demo ==="
  let (result', logs) = runWriter (computeWithLogging 5)
  putStrLn $ "Computation result: " ++ show result'
  putStrLn "Logs:"
  mapM_ putStrLn logs
  
  -- Demo validation
  putStrLn "\n=== Validation with ExceptT Demo ==="
  result1 <- runExceptT $ validateUser "user@example.com" "password123"
  case result1 of
    Left err -> putStrLn $ "Validation failed: " ++ show err
    Right (email, pwd) -> putStrLn $ "Valid user: " ++ email
  
  putStrLn "\nHaskell's effect systems provide composable, type-safe side effect handling!"