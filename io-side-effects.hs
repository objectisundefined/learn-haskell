{-
IO and Side Effects in Haskell
==============================

Demonstrates how Haskell handles side effects through the IO monad:
- Pure vs impure functions
- IO monad and do notation
- Lazy IO and strictness
- Mutable references (IORef, MVar, STM)
- File system operations
- Network programming
- Random number generation
- Time and concurrency
-}

{-# LANGUAGE OverloadedStrings #-}

module IOSideEffects where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
import Data.Time
import System.IO
import System.Random
import System.Environment
import System.Directory
import System.Process
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import qualified Data.ByteString.Lazy.Char8 as L8

-- =============================================================================
-- 1. Pure vs Impure Functions
-- =============================================================================

-- Pure function - always returns same output for same input
pureCalculation :: Int -> Int -> Int
pureCalculation x y = x * x + y * y

-- Impure function - interacts with the outside world
impureCalculation :: Int -> Int -> IO Int
impureCalculation x y = do
  putStrLn $ "Computing " ++ show x ++ "² + " ++ show y ++ "²"
  let result = x * x + y * y
  putStrLn $ "Result: " ++ show result
  return result

-- Function that uses both pure and impure operations
mixedCalculation :: [Int] -> IO [Int]
mixedCalculation numbers = do
  putStrLn "Processing numbers..."
  let pureResults = map pureCalculation numbers (repeat 1)
  putStrLn $ "Pure results: " ++ show pureResults
  
  -- Add some randomness (side effect)
  randomBonus <- randomRIO (1, 10)
  let finalResults = map (+ randomBonus) pureResults
  
  putStrLn $ "Final results with random bonus: " ++ show finalResults
  return finalResults

-- =============================================================================
-- 2. Basic IO Operations
-- =============================================================================

-- User interaction
interactWithUser :: IO ()
interactWithUser = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"
  
  putStrLn "What's your age?"
  ageStr <- getLine
  case reads ageStr of
    [(age, "")] -> do
      let yearBorn = 2024 - age
      putStrLn $ "You were born around " ++ show yearBorn
    _ -> putStrLn "Invalid age format"

-- File operations
fileOperations :: IO ()
fileOperations = do
  putStrLn "=== File Operations ==="
  
  -- Write to file
  let content = "Hello from Haskell!\nThis is a test file.\n"
  writeFile "test.txt" content
  putStrLn "File written"
  
  -- Read from file
  contents <- readFile "test.txt"
  putStrLn "File contents:"
  putStrLn contents
  
  -- Append to file
  appendFile "test.txt" "Appended line\n"
  
  -- Read again
  newContents <- readFile "test.txt"
  putStrLn "After appending:"
  putStrLn newContents

-- =============================================================================
-- 3. Mutable References
-- =============================================================================

-- IORef for mutable state
demonstrateIORef :: IO ()
demonstrateIORef = do
  putStrLn "=== IORef Demo ==="
  
  -- Create a mutable reference
  counter <- newIORef 0
  
  -- Read and modify
  forM_ [1..5] $ \i -> do
    current <- readIORef counter
    putStrLn $ "Iteration " ++ show i ++ ", counter: " ++ show current
    modifyIORef counter (+1)
  
  finalValue <- readIORef counter
  putStrLn $ "Final counter value: " ++ show finalValue

-- MVar for thread-safe mutable state
demonstrateMVar :: IO ()
demonstrateMVar = do
  putStrLn "\n=== MVar Demo ==="
  
  -- Create a shared counter
  sharedCounter <- newMVar 0
  
  -- Multiple threads modifying the counter
  threads <- forM [1..3] $ \threadId ->
    async $ do
      forM_ [1..10] $ \_ -> do
        modifyMVar_ sharedCounter $ \count -> do
          let newCount = count + 1
          putStrLn $ "Thread " ++ show threadId ++ " incremented to " ++ show newCount
          threadDelay 10000  -- Small delay
          return newCount
  
  -- Wait for all threads to complete
  mapM_ wait threads
  
  finalValue <- readMVar sharedCounter
  putStrLn $ "Final shared counter: " ++ show finalValue

-- =============================================================================
-- 4. Random Number Generation
-- =============================================================================

randomOperations :: IO ()
randomOperations = do
  putStrLn "\n=== Random Operations ==="
  
  -- Generate random integers
  randomInt <- randomRIO (1, 100)
  putStrLn $ "Random integer (1-100): " ++ show randomInt
  
  -- Generate random doubles
  randomDouble <- randomRIO (0.0, 1.0)
  putStrLn $ "Random double (0-1): " ++ show randomDouble
  
  -- Generate a list of random numbers
  randomList <- replicateM 10 (randomRIO (1, 6))
  putStrLn $ "10 random dice rolls: " ++ show randomList
  
  -- Shuffle a list
  let originalList = [1..10]
  shuffledList <- shuffle originalList
  putStrLn $ "Original: " ++ show originalList
  putStrLn $ "Shuffled: " ++ show shuffledList

-- Simple shuffle implementation
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  randomIndex <- randomRIO (0, length xs - 1)
  let (before, item:after) = splitAt randomIndex xs
  rest <- shuffle (before ++ after)
  return (item : rest)

-- =============================================================================
-- 5. Time Operations
-- =============================================================================

timeOperations :: IO ()
timeOperations = do
  putStrLn "\n=== Time Operations ==="
  
  -- Get current time
  currentTime <- getCurrentTime
  putStrLn $ "Current time: " ++ show currentTime
  
  -- Measure execution time
  putStrLn "Measuring execution time..."
  start <- getCurrentTime
  
  -- Simulate some work
  threadDelay 1000000  -- 1 second
  let result = sum [1..1000000]
  
  end <- getCurrentTime
  let duration = diffUTCTime end start
  
  putStrLn $ "Computation result: " ++ show result
  putStrLn $ "Execution time: " ++ show duration

-- =============================================================================
-- 6. Environment and System Operations
-- =============================================================================

systemOperations :: IO ()
systemOperations = do
  putStrLn "\n=== System Operations ==="
  
  -- Get environment variables
  home <- getEnv "HOME" `catch` \_ -> return "Not found"
  putStrLn $ "HOME directory: " ++ home
  
  -- Get current directory
  currentDir <- getCurrentDirectory
  putStrLn $ "Current directory: " ++ currentDir
  
  -- List directory contents
  contents <- listDirectory "."
  putStrLn $ "Directory contents: " ++ show (take 5 contents)
  
  -- Execute external command
  putStrLn "Executing 'date' command:"
  output <- readProcess "date" [] ""
  putStrLn $ "Date output: " ++ output

-- =============================================================================
-- 7. Network Operations (HTTP)
-- =============================================================================

networkOperations :: IO ()
networkOperations = do
  putStrLn "\n=== Network Operations ==="
  
  putStrLn "Making HTTP request to httpbin.org..."
  
  -- Simple HTTP GET request
  request <- parseRequest "http://httpbin.org/json"
  response <- httpLBS request
  
  let body = getResponseBody response
  putStrLn "Response received:"
  putStrLn $ take 200 $ L8.unpack body

-- =============================================================================
-- 8. Concurrent Programming
-- =============================================================================

concurrentOperations :: IO ()
concurrentOperations = do
  putStrLn "\n=== Concurrent Operations ==="
  
  -- Parallel computations
  putStrLn "Starting parallel computations..."
  
  async1 <- async $ do
    putStrLn "Task 1 starting"
    threadDelay 2000000  -- 2 seconds
    putStrLn "Task 1 completed"
    return "Result from task 1"
  
  async2 <- async $ do
    putStrLn "Task 2 starting"
    threadDelay 1500000  -- 1.5 seconds
    putStrLn "Task 2 completed"
    return "Result from task 2"
  
  -- Wait for both tasks
  result1 <- wait async1
  result2 <- wait async2
  
  putStrLn $ "Task 1 result: " ++ result1
  putStrLn $ "Task 2 result: " ++ result2

-- Producer-consumer pattern
producerConsumer :: IO ()
producerConsumer = do
  putStrLn "\n=== Producer-Consumer Pattern ==="
  
  -- Shared communication channel
  chan <- newChan
  
  -- Producer
  producer <- async $ do
    forM_ [1..10] $ \i -> do
      putStrLn $ "Producing item " ++ show i
      writeChan chan i
      threadDelay 200000  -- Small delay
    writeChan chan (-1)  -- Signal end
  
  -- Consumer
  consumer <- async $ do
    let consumeLoop = do
          item <- readChan chan
          if item == -1
            then putStrLn "Consumer finished"
            else do
              putStrLn $ "Consumed item " ++ show item
              threadDelay 300000  -- Processing time
              consumeLoop
    consumeLoop
  
  -- Wait for completion
  _ <- wait producer
  _ <- wait consumer
  putStrLn "Producer-Consumer demo completed"

-- =============================================================================
-- 9. Error Handling in IO
-- =============================================================================

errorHandlingIO :: IO ()
errorHandlingIO = do
  putStrLn "\n=== Error Handling in IO ==="
  
  -- Handle file not found
  putStrLn "Attempting to read non-existent file:"
  result <- readFile "nonexistent.txt" `catch` \e -> do
    putStrLn $ "Caught error: " ++ show (e :: IOError)
    return "Default content"
  
  putStrLn $ "Result: " ++ result
  
  -- Handle division by zero in IO context
  putStrLn "Safe division in IO:"
  safeDivisionResult <- safeDivisionIO 10 0
  putStrLn $ "Division result: " ++ show safeDivisionResult

safeDivisionIO :: Double -> Double -> IO (Maybe Double)
safeDivisionIO x y = do
  if y == 0
    then do
      putStrLn "Warning: Division by zero!"
      return Nothing
    else do
      let result = x / y
      putStrLn $ "Division successful: " ++ show result
      return $ Just result

-- =============================================================================
-- 10. Lazy IO Considerations
-- =============================================================================

lazyIODemo :: IO ()
lazyIODemo = do
  putStrLn "\n=== Lazy IO Demo ==="
  
  -- Create a large file
  writeFile "large.txt" $ unlines $ map show [1..100000]
  putStrLn "Created large file"
  
  -- Lazy reading (be careful with this!)
  putStrLn "Reading file lazily..."
  contents <- readFile "large.txt"
  let firstLine = head $ lines contents
  putStrLn $ "First line: " ++ firstLine
  
  -- The file is still open at this point due to lazy evaluation!
  -- This can cause resource leaks
  
  -- Strict reading alternative
  putStrLn "Reading file strictly..."
  strictContents <- readFile "large.txt"
  let lineCount = length $ lines strictContents
  seq lineCount $ putStrLn $ "Line count: " ++ show lineCount
  
  -- Clean up
  removeFile "large.txt"
  putStrLn "Cleaned up large file"

-- =============================================================================
-- Demonstration Main Function
-- =============================================================================

main :: IO ()
main = do
  putStrLn "IO and Side Effects in Haskell"
  putStrLn "=============================="
  
  -- Demonstrate different aspects of IO
  putStrLn "\n--- Pure vs Impure ---"
  result <- impureCalculation 3 4
  putStrLn $ "Impure calculation result: " ++ show result
  
  mixed <- mixedCalculation [1, 2, 3]
  putStrLn $ "Mixed calculation result: " ++ show mixed
  
  demonstrateIORef
  demonstrateMVar
  randomOperations
  timeOperations
  systemOperations
  
  -- Network operations (may fail if no internet)
  networkOperations `catch` \e -> 
    putStrLn $ "Network error: " ++ show (e :: HttpException)
  
  concurrentOperations
  producerConsumer
  errorHandlingIO
  lazyIODemo
  
  putStrLn "\nHaskell's IO system provides controlled access to side effects!"

-- Exception type for HTTP operations
data HttpException = HttpException String deriving Show
instance Exception HttpException