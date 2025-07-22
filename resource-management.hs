{-
Resource Management in Haskell
==============================

Demonstrates safe resource handling patterns:
- RAII (Resource Acquisition Is Initialization) patterns
- Bracket for exception-safe resource cleanup
- ResourceT for composable resource management
- Memory management and lazy evaluation
- File handles, network connections, and locks
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResourceManagement where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import Data.Time

-- =============================================================================
-- 1. Basic Resource Management with Bracket
-- =============================================================================

-- File resource management
data FileResource = FileResource Handle FilePath

openFileResource :: FilePath -> IO FileResource
openFileResource path = do
  putStrLn $ "Opening file: " ++ path
  handle <- openFile path ReadMode
  return $ FileResource handle path

closeFileResource :: FileResource -> IO ()
closeFileResource (FileResource handle path) = do
  putStrLn $ "Closing file: " ++ path
  hClose handle

-- Safe file operations using bracket
withFileResource :: FilePath -> (FileResource -> IO a) -> IO a
withFileResource path action = bracket
  (openFileResource path)
  closeFileResource
  action

-- Example usage
processFileContent :: FilePath -> IO String
processFileContent path = withFileResource path $ \(FileResource handle _) -> do
  content <- hGetContents handle
  putStrLn $ "Processing " ++ show (length content) ++ " characters"
  return $ "Processed: " ++ take 100 content

-- =============================================================================
-- 2. Network Resource Management
-- =============================================================================

data ConnectionResource = ConnectionResource Socket String Int

establishConnection :: String -> Int -> IO ConnectionResource
establishConnection host port = do
  putStrLn $ "Connecting to " ++ host ++ ":" ++ show port
  sock <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  -- Note: In real code, you'd resolve host and connect
  putStrLn "Connection established"
  return $ ConnectionResource sock host port

closeConnection :: ConnectionResource -> IO ()
closeConnection (ConnectionResource sock host port) = do
  putStrLn $ "Closing connection to " ++ host ++ ":" ++ show port
  Socket.close sock

-- Safe network operations
withConnection :: String -> Int -> (ConnectionResource -> IO a) -> IO a
withConnection host port action = bracket
  (establishConnection host port)
  closeConnection
  action

-- Example network operation
sendData :: String -> Int -> String -> IO ()
sendData host port message = withConnection host port $ \conn -> do
  putStrLn $ "Sending: " ++ message
  -- In real code: Socket.send (getSocket conn) message
  putStrLn "Data sent successfully"

-- =============================================================================
-- 3. Memory Resource Management
-- =============================================================================

-- Simulated memory allocation
data MemoryBlock = MemoryBlock Int String  -- size and identifier

allocateMemory :: Int -> String -> IO MemoryBlock
allocateMemory size identifier = do
  putStrLn $ "Allocating " ++ show size ++ " bytes for " ++ identifier
  -- Simulate allocation time
  threadDelay 10000
  return $ MemoryBlock size identifier

freeMemory :: MemoryBlock -> IO ()
freeMemory (MemoryBlock size identifier) = do
  putStrLn $ "Freeing " ++ show size ++ " bytes for " ++ identifier

-- Memory-safe operations
withMemory :: Int -> String -> (MemoryBlock -> IO a) -> IO a
withMemory size identifier action = bracket
  (allocateMemory size identifier)
  freeMemory
  action

-- Example memory operation
processInMemory :: String -> IO String
processInMemory dataName = withMemory 1024 dataName $ \(MemoryBlock size name) -> do
  putStrLn $ "Processing data in " ++ show size ++ " byte block: " ++ name
  -- Simulate processing
  threadDelay 50000
  return $ "Processed " ++ name

-- =============================================================================
-- 4. Lock Resource Management
-- =============================================================================

data LockResource = LockResource (MVar ()) String

acquireLock :: String -> IO LockResource
acquireLock name = do
  putStrLn $ "Acquiring lock: " ++ name
  lock <- newMVar ()
  takeMVar lock  -- Acquire the lock
  putStrLn $ "Lock acquired: " ++ name
  return $ LockResource lock name

releaseLock :: LockResource -> IO ()
releaseLock (LockResource lock name) = do
  putStrLn $ "Releasing lock: " ++ name
  putMVar lock ()
  putStrLn $ "Lock released: " ++ name

-- Safe critical section execution
withLock :: String -> IO a -> IO a
withLock name action = bracket
  (acquireLock name)
  releaseLock
  (const action)

-- Example critical section
criticalOperation :: String -> IO ()
criticalOperation operationName = withLock "global_lock" $ do
  putStrLn $ "Executing critical operation: " ++ operationName
  threadDelay 100000  -- Simulate work
  putStrLn $ "Critical operation completed: " ++ operationName

-- =============================================================================
-- 5. Composable Resource Management
-- =============================================================================

-- Multiple resources with proper cleanup order
data DatabaseConnection = DatabaseConnection String
data CacheConnection = CacheConnection String

connectDatabase :: String -> IO DatabaseConnection
connectDatabase url = do
  putStrLn $ "Connecting to database: " ++ url
  return $ DatabaseConnection url

disconnectDatabase :: DatabaseConnection -> IO ()
disconnectDatabase (DatabaseConnection url) = do
  putStrLn $ "Disconnecting from database: " ++ url

connectCache :: String -> IO CacheConnection
connectCache url = do
  putStrLn $ "Connecting to cache: " ++ url
  return $ CacheConnection url

disconnectCache :: CacheConnection -> IO ()
disconnectCache (CacheConnection url) = do
  putStrLn $ "Disconnecting from cache: " ++ url

-- Nested resource management
withDatabaseAndCache :: String -> String -> (DatabaseConnection -> CacheConnection -> IO a) -> IO a
withDatabaseAndCache dbUrl cacheUrl action = 
  bracket (connectDatabase dbUrl) disconnectDatabase $ \db ->
    bracket (connectCache cacheUrl) disconnectCache $ \cache ->
      action db cache

-- Example using multiple resources
performComplexOperation :: IO String
performComplexOperation = withDatabaseAndCache "postgres://localhost" "redis://localhost" $ \db cache -> do
  putStrLn "Performing complex operation with DB and Cache"
  -- Simulate work
  threadDelay 200000
  return "Operation completed successfully"

-- =============================================================================
-- 6. Exception-Safe Resource Pools
-- =============================================================================

data ResourcePool a = ResourcePool 
  { available :: TVar [a]
  , inUse :: TVar [a]
  , createResource :: IO a
  , destroyResource :: a -> IO ()
  , maxSize :: Int
  }

newResourcePool :: IO a -> (a -> IO ()) -> Int -> IO (ResourcePool a)
newResourcePool create destroy size = do
  availableVar <- newTVarIO []
  inUseVar <- newTVarIO []
  return $ ResourcePool availableVar inUseVar create destroy size

acquireFromPool :: ResourcePool a -> IO a
acquireFromPool pool = do
  maybeResource <- atomically $ do
    available <- readTVar (available pool)
    case available of
      [] -> return Nothing
      (r:rs) -> do
        writeTVar (available pool) rs
        inUse <- readTVar (inUse pool)
        writeTVar (inUse pool) (r : inUse)
        return $ Just r
  
  case maybeResource of
    Just resource -> return resource
    Nothing -> do
      -- Create new resource if pool not at capacity
      resource <- createResource pool
      atomically $ do
        inUse <- readTVar (inUse pool)
        writeTVar (inUse pool) (resource : inUse)
      return resource

releaseToPool :: ResourcePool a -> a -> IO ()
releaseToPool pool resource = atomically $ do
  inUse <- readTVar (inUse pool)
  available <- readTVar (available pool)
  writeTVar (inUse pool) (filter (/= resource) inUse)
  writeTVar (available pool) (resource : available)

-- Safe pool usage
withPoolResource :: ResourcePool a -> (a -> IO b) -> IO b
withPoolResource pool action = bracket
  (acquireFromPool pool)
  (releaseToPool pool)
  action

-- =============================================================================
-- 7. Monitoring Resource Usage
-- =============================================================================

data ResourceMonitor = ResourceMonitor
  { allocatedResources :: IORef [String]
  , startTime :: UTCTime
  }

newResourceMonitor :: IO ResourceMonitor
newResourceMonitor = do
  ref <- newIORef []
  time <- getCurrentTime
  return $ ResourceMonitor ref time

trackResource :: ResourceMonitor -> String -> IO ()
trackResource monitor resourceName = do
  atomicModifyIORef' (allocatedResources monitor) $ \resources ->
    (resourceName : resources, ())
  putStrLn $ "Tracking resource: " ++ resourceName

untrackResource :: ResourceMonitor -> String -> IO ()
untrackResource monitor resourceName = do
  atomicModifyIORef' (allocatedResources monitor) $ \resources ->
    (filter (/= resourceName) resources, ())
  putStrLn $ "Stopped tracking resource: " ++ resourceName

getResourceReport :: ResourceMonitor -> IO String
getResourceReport monitor = do
  resources <- readIORef (allocatedResources monitor)
  currentTime <- getCurrentTime
  let uptime = diffUTCTime currentTime (startTime monitor)
  return $ "Active resources: " ++ show (length resources) ++ 
           ", Uptime: " ++ show uptime

-- =============================================================================
-- Demonstration Functions
-- =============================================================================

demonstrateBasicResources :: IO ()
demonstrateBasicResources = do
  putStrLn "=== Basic Resource Management Demo ==="
  
  -- File resource demo
  putStrLn "\nFile operations:"
  result <- try $ processFileContent "README.md"
  case result of
    Left (ex :: SomeException) -> putStrLn $ "File error: " ++ show ex
    Right content -> putStrLn $ "File processed: " ++ take 50 content ++ "..."

demonstrateNetworkResources :: IO ()
demonstrateNetworkResources = do
  putStrLn "\n=== Network Resource Management Demo ==="
  
  result <- try $ sendData "localhost" 8080 "Hello, World!"
  case result of
    Left (ex :: SomeException) -> putStrLn $ "Network error: " ++ show ex
    Right _ -> putStrLn "Network operation completed"

demonstrateMemoryResources :: IO ()
demonstrateMemoryResources = do
  putStrLn "\n=== Memory Resource Management Demo ==="
  
  result <- processInMemory "user_data"
  putStrLn $ "Memory operation result: " ++ result

demonstrateLockResources :: IO ()
demonstrateLockResources = do
  putStrLn "\n=== Lock Resource Management Demo ==="
  
  -- Simulate concurrent access
  async1 <- forkIO $ criticalOperation "operation_1"
  async2 <- forkIO $ criticalOperation "operation_2"
  
  threadDelay 300000
  putStrLn "Lock demonstration completed"

demonstrateComplexResources :: IO ()
demonstrateComplexResources = do
  putStrLn "\n=== Complex Resource Management Demo ==="
  
  result <- try performComplexOperation
  case result of
    Left (ex :: SomeException) -> putStrLn $ "Complex operation error: " ++ show ex
    Right msg -> putStrLn $ "Complex operation result: " ++ msg

main :: IO ()
main = do
  putStrLn "Resource Management in Haskell"
  putStrLn "=============================="
  
  demonstrateBasicResources
  demonstrateNetworkResources
  demonstrateMemoryResources
  demonstrateLockResources
  demonstrateComplexResources
  
  putStrLn "\nHaskell provides excellent tools for safe resource management!"