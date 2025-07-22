{-
Exception Handling Patterns in Haskell
======================================

Demonstrates various approaches to error handling:
- Maybe and Either types for pure error handling
- ExceptT for composable errors
- IO exceptions with Control.Exception
- Custom error types and recovery strategies
- Safe resource management with bracket
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ExceptionHandling where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Typeable
import GHC.Generics
import System.IO
import System.IO.Error

-- =============================================================================
-- 1. Pure Error Handling with Maybe and Either
-- =============================================================================

-- Safe division using Maybe
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Safe list operations
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- Chaining Maybe operations
chainedOperations :: [Double] -> Maybe Double
chainedOperations xs = do
  first <- safeHead xs
  rest <- safeTail xs
  second <- safeHead rest
  safeDivide first second

-- =============================================================================
-- 2. Either for Detailed Error Information
-- =============================================================================

data MathError
  = DivisionByZero
  | NegativeSquareRoot
  | EmptyList
  deriving (Show, Eq)

safeDivideEither :: Double -> Double -> Either MathError Double
safeDivideEither _ 0 = Left DivisionByZero
safeDivideEither x y = Right (x / y)

safeSqrt :: Double -> Either MathError Double
safeSqrt x
  | x < 0 = Left NegativeSquareRoot
  | otherwise = Right (sqrt x)

safeHeadEither :: [a] -> Either MathError a
safeHeadEither [] = Left EmptyList
safeHeadEither (x:_) = Right x

-- Complex computation with error handling
complexMath :: [Double] -> Either MathError Double
complexMath xs = do
  first <- safeHeadEither xs
  sqrtFirst <- safeSqrt first
  rest <- if length xs > 1 then Right (tail xs) else Left EmptyList
  second <- safeHeadEither rest
  result <- safeDivideEither sqrtFirst second
  return result

-- =============================================================================
-- 3. Custom Exception Types
-- =============================================================================

-- Custom exception for business logic
data DatabaseException
  = ConnectionFailed String
  | QueryTimeout
  | RecordNotFound String
  deriving (Show, Typeable)

instance Exception DatabaseException

data ValidationException
  = InvalidEmail String
  | PasswordTooWeak
  | UsernameTaken String
  deriving (Show, Typeable)

instance Exception ValidationException

-- =============================================================================
-- 4. Safe IO Operations with Exception Handling
-- =============================================================================

-- Safe file reading with proper error handling
safeReadFile :: FilePath -> IO (Either IOError String)
safeReadFile path = do
  result <- try $ readFile path
  return $ case result of
    Left err -> Left err
    Right content -> Right content

-- Safe file operations with resource management
withFileHandle :: FilePath -> IOMode -> (Handle -> IO a) -> IO (Either IOError a)
withFileHandle path mode action = do
  result <- try $ bracket
    (openFile path mode)
    hClose
    action
  return $ case result of
    Left err -> Left err
    Right value -> Right value

-- =============================================================================
-- 5. ExceptT for Composable Error Handling
-- =============================================================================

type AppM = ExceptT String IO

-- Database simulation
fetchUser :: String -> AppM String
fetchUser userId = do
  liftIO $ putStrLn $ "Fetching user: " ++ userId
  if userId == "invalid"
    then throwError "User not found"
    else return $ "User data for " ++ userId

updateUser :: String -> String -> AppM ()
updateUser userId newData = do
  liftIO $ putStrLn $ "Updating user " ++ userId ++ " with: " ++ newData
  if "invalid" `elem` words newData
    then throwError "Invalid data format"
    else liftIO $ putStrLn "Update successful"

-- Composable operations
userWorkflow :: String -> AppM String
userWorkflow userId = do
  userData <- fetchUser userId
  updateUser userId "new data"
  return $ "Processed: " ++ userData

-- =============================================================================
-- 6. Exception Recovery and Cleanup
-- =============================================================================

-- Retry mechanism
retryWithBackoff :: IO a -> Int -> IO (Maybe a)
retryWithBackoff action 0 = return Nothing
retryWithBackoff action retries = do
  result <- try action
  case result of
    Right value -> return $ Just value
    Left (_ :: SomeException) -> do
      putStrLn $ "Attempt failed, retries left: " ++ show (retries - 1)
      retryWithBackoff action (retries - 1)

-- Resource cleanup with finally
withCleanup :: IO a -> IO b -> IO a
withCleanup action cleanup = do
  result <- action `finally` cleanup
  return result

-- Safe resource allocation
allocateResource :: String -> IO String
allocateResource name = do
  putStrLn $ "Allocating resource: " ++ name
  return name

releaseResource :: String -> IO ()
releaseResource name = putStrLn $ "Releasing resource: " ++ name

-- Use resource safely
useResourceSafely :: String -> IO ()
useResourceSafely name = bracket
  (allocateResource name)
  releaseResource
  (\resource -> do
    putStrLn $ "Using resource: " ++ resource
    -- Simulate work that might fail
    when (name == "failure") $ throwIO $ userError "Resource operation failed"
    putStrLn "Work completed")

-- =============================================================================
-- 7. Catching Specific Exceptions
-- =============================================================================

-- Handle specific exception types
handleDatabaseErrors :: IO a -> IO (Either DatabaseException a)
handleDatabaseErrors action = do
  result <- try action
  return $ case result of
    Left err -> Left err
    Right value -> Right value

-- Simulate database operations that might fail
simulateDbQuery :: String -> IO String
simulateDbQuery query
  | "timeout" `elem` words query = throwIO QueryTimeout
  | "missing" `elem` words query = throwIO $ RecordNotFound "User123"
  | "connection" `elem` words query = throwIO $ ConnectionFailed "Database unreachable"
  | otherwise = return $ "Result for: " ++ query

-- Multiple exception handlers
handleMultipleExceptions :: IO String -> IO String
handleMultipleExceptions action = 
  action `catches`
    [ Handler (\(ex :: DatabaseException) -> return $ "Database error: " ++ show ex)
    , Handler (\(ex :: ValidationException) -> return $ "Validation error: " ++ show ex)
    , Handler (\(ex :: IOError) -> return $ "IO error: " ++ show ex)
    ]

-- =============================================================================
-- Demonstration Functions
-- =============================================================================

demonstratePureErrors :: IO ()
demonstratePureErrors = do
  putStrLn "=== Pure Error Handling Demo ==="
  
  -- Maybe examples
  putStrLn $ "Safe division 10/2: " ++ show (safeDivide 10 2)
  putStrLn $ "Safe division 10/0: " ++ show (safeDivide 10 0)
  
  -- Either examples
  let numbers = [9.0, 3.0]
  putStrLn $ "Complex math [9,3]: " ++ show (complexMath numbers)
  
  let badNumbers = [-4.0, 2.0]
  putStrLn $ "Complex math [-4,2]: " ++ show (complexMath badNumbers)

demonstrateIOErrors :: IO ()
demonstrateIOErrors = do
  putStrLn "\n=== IO Error Handling Demo ==="
  
  -- Try to read a non-existent file
  result <- safeReadFile "nonexistent.txt"
  case result of
    Left err -> putStrLn $ "File read error: " ++ show err
    Right content -> putStrLn $ "File content: " ++ content
  
  -- Safe resource usage
  putStrLn "\nTesting resource management:"
  result' <- try $ useResourceSafely "test-resource"
  case result' of
    Left (ex :: SomeException) -> putStrLn $ "Resource error: " ++ show ex
    Right _ -> putStrLn "Resource operation completed"

demonstrateExceptT :: IO ()
demonstrateExceptT = do
  putStrLn "\n=== ExceptT Demo ==="
  
  result <- runExceptT $ userWorkflow "user123"
  case result of
    Left err -> putStrLn $ "Workflow error: " ++ err
    Right value -> putStrLn $ "Workflow result: " ++ value

demonstrateExceptionHandling :: IO ()
demonstrateExceptionHandling = do
  putStrLn "\n=== Exception Handling Demo ==="
  
  -- Test different types of database errors
  let queries = ["SELECT * FROM users", "timeout query", "missing user", "connection failed"]
  
  forM_ queries $ \query -> do
    putStrLn $ "\nExecuting: " ++ query
    result <- handleMultipleExceptions (simulateDbQuery query)
    putStrLn $ "Result: " ++ result

demonstrateRetry :: IO ()
demonstrateRetry = do
  putStrLn "\n=== Retry Mechanism Demo ==="
  
  let flakyAction = do
        putStrLn "Attempting flaky operation..."
        throwIO $ userError "Operation failed"
  
  result <- retryWithBackoff flakyAction 3
  case result of
    Nothing -> putStrLn "All retries exhausted"
    Just value -> putStrLn $ "Success: " ++ show value

main :: IO ()
main = do
  putStrLn "Exception Handling Patterns in Haskell"
  putStrLn "======================================"
  
  demonstratePureErrors
  demonstrateIOErrors
  demonstrateExceptT
  demonstrateExceptionHandling
  demonstrateRetry
  
  putStrLn "\nHaskell provides powerful tools for safe error handling!"