{-
Demonstrates advanced concurrency patterns in Haskell
- Software Transactional Memory (STM)
- Async programming with parallel operations
- Lock-free data structures
- Actor-like patterns with message passing
- High-performance concurrent algorithms
-}

module AdvancedConcurrency where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import Data.IORef
import System.Random
import Data.Time

-- STM-based bank account example
data Account = Account
  { balance :: TVar Int
  , accountId :: Int
  } deriving Eq

newAccount :: Int -> Int -> IO Account
newAccount aid initialBalance = do
  bal <- newTVarIO initialBalance
  return $ Account bal aid

-- Atomic money transfer
transfer :: Account -> Account -> Int -> STM ()
transfer from to amount = do
  fromBal <- readTVar (balance from)
  toBal <- readTVar (balance to)
  
  if fromBal >= amount
    then do
      writeTVar (balance from) (fromBal - amount)
      writeTVar (balance to) (toBal + amount)
    else retry

-- Parallel computation with Async
parallelSum :: [Int] -> IO Int
parallelSum xs = do
  let chunks = splitIntoChunks 4 xs
  asyncActions <- mapM (async . return . sum) chunks
  results <- mapM wait asyncActions
  return $ sum results
  where
    splitIntoChunks n list = [take chunkSize $ drop (i * chunkSize) list | i <- [0..n-1]]
      where chunkSize = length list `div` n

-- Producer-Consumer with STM
data Buffer a = Buffer
  { items :: TVar [a]
  , capacity :: Int
  }

newBuffer :: Int -> IO (Buffer a)
newBuffer cap = do
  items <- newTVarIO []
  return $ Buffer items cap

produce :: Buffer a -> a -> STM ()
produce buffer item = do
  itemList <- readTVar (items buffer)
  if length itemList < capacity buffer
    then writeTVar (items buffer) (item : itemList)
    else retry

consume :: Buffer a -> STM a
consume buffer = do
  itemList <- readTVar (items buffer)
  case reverse itemList of
    [] -> retry
    (x:xs) -> do
      writeTVar (items buffer) (reverse xs)
      return x

-- Lock-free counter using IORef and CAS
data LockFreeCounter = LockFreeCounter (IORef Int)

newLockFreeCounter :: Int -> IO LockFreeCounter
newLockFreeCounter initial = LockFreeCounter <$> newIORef initial

incrementLF :: LockFreeCounter -> IO ()
incrementLF (LockFreeCounter ref) = do
  old <- readIORef ref
  success <- atomicModifyIORef' ref (\x -> if x == old then (x + 1, True) else (x, False))
  unless success $ incrementLF (LockFreeCounter ref)

-- Message passing with Chan
data Message = Message String Int deriving Show

-- Actor-like worker that processes messages
worker :: Chan Message -> IO ()
worker chan = forever $ do
  msg <- readChan chan
  putStrLn $ "Processing: " ++ show msg
  threadDelay 100000  -- Simulate work

-- Supervisor pattern
supervisedWorker :: Chan Message -> IO ()
supervisedWorker chan = do
  result <- try (worker chan)
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Worker crashed: " ++ show e
      putStrLn "Restarting worker..."
      supervisedWorker chan
    Right _ -> return ()

-- Parallel map using divide-and-conquer
parallelMap :: (a -> b) -> [a] -> IO [b]
parallelMap f xs
  | length xs <= 1 = return $ map f xs
  | otherwise = do
      let mid = length xs `div` 2
          (left, right) = splitAt mid xs
      
      leftAsync <- async (parallelMap f left)
      rightAsync <- async (parallelMap f right)
      
      leftResult <- wait leftAsync
      rightResult <- wait rightAsync
      
      return $ leftResult ++ rightResult

-- Rate limiting with STM
data RateLimiter = RateLimiter
  { tokens :: TVar Int
  , capacity :: Int
  , refillRate :: Int
  , lastRefill :: TVar UTCTime
  }

newRateLimiter :: Int -> Int -> IO RateLimiter
newRateLimiter cap rate = do
  tokensVar <- newTVarIO cap
  timeVar <- newTVarIO =<< getCurrentTime
  return $ RateLimiter tokensVar cap rate timeVar

-- Try to acquire a token
acquire :: RateLimiter -> STM Bool
acquire limiter = do
  currentTokens <- readTVar (tokens limiter)
  if currentTokens > 0
    then do
      writeTVar (tokens limiter) (currentTokens - 1)
      return True
    else return False

-- Background token refill
refillTokens :: RateLimiter -> IO ()
refillTokens limiter = forever $ do
  threadDelay 1000000  -- Wait 1 second
  now <- getCurrentTime
  atomically $ do
    currentTokens <- readTVar (tokens limiter)
    let newTokens = min (capacity limiter) (currentTokens + refillRate limiter)
    writeTVar (tokens limiter) newTokens
    writeTVar (lastRefill limiter) now

-- Parallel quicksort
parallelQuicksort :: (Ord a) => [a] -> IO [a]
parallelQuicksort [] = return []
parallelQuicksort [x] = return [x]
parallelQuicksort (pivot:xs) = do
  let smaller = [x | x <- xs, x <= pivot]
      larger = [x | x <- xs, x > pivot]
  
  if length xs < 1000  -- Sequential threshold
    then return $ quicksortSeq (pivot:xs)
    else do
      smallerAsync <- async (parallelQuicksort smaller)
      largerAsync <- async (parallelQuicksort larger)
      
      smallerSorted <- wait smallerAsync
      largerSorted <- wait largerAsync
      
      return $ smallerSorted ++ [pivot] ++ largerSorted
  where
    quicksortSeq [] = []
    quicksortSeq (p:ys) = 
      let smaller = [y | y <- ys, y <= p]
          larger = [y | y <- ys, y > p]
      in quicksortSeq smaller ++ [p] ++ quicksortSeq larger

-- Concurrent hash table using STM
data ConcurrentHashTable k v = ConcurrentHashTable [TVar [(k, v)]]

newConcurrentHashTable :: Int -> IO (ConcurrentHashTable k v)
newConcurrentHashTable buckets = 
  ConcurrentHashTable <$> replicateM buckets (newTVarIO [])

hashFunction :: Int -> Int -> Int
hashFunction buckets key = abs key `mod` buckets

insertCHT :: (Eq k) => ConcurrentHashTable Int v -> Int -> v -> STM ()
insertCHT (ConcurrentHashTable buckets) key value = do
  let bucketIndex = hashFunction (length buckets) key
      bucket = buckets !! bucketIndex
  kvPairs <- readTVar bucket
  let updatedPairs = (key, value) : filter ((/= key) . fst) kvPairs
  writeTVar bucket updatedPairs

lookupCHT :: (Eq k) => ConcurrentHashTable Int v -> Int -> STM (Maybe v)
lookupCHT (ConcurrentHashTable buckets) key = do
  let bucketIndex = hashFunction (length buckets) key
      bucket = buckets !! bucketIndex
  kvPairs <- readTVar bucket
  return $ lookup key kvPairs

-- Demonstration functions
demonstrateSTM :: IO ()
demonstrateSTM = do
  putStrLn "STM Bank Transfer Demo:"
  alice <- newAccount 1 1000
  bob <- newAccount 2 500
  
  putStrLn "Initial balances:"
  aliceBalance <- readTVarIO (balance alice)
  bobBalance <- readTVarIO (balance bob)
  putStrLn $ "Alice: " ++ show aliceBalance ++ ", Bob: " ++ show bobBalance
  
  -- Concurrent transfers
  forM_ [1..5] $ \i -> forkIO $ do
    atomically $ transfer alice bob 50
    putStrLn $ "Transfer " ++ show i ++ " completed"
  
  threadDelay 1000000
  
  putStrLn "Final balances:"
  aliceBalance' <- readTVarIO (balance alice)
  bobBalance' <- readTVarIO (balance bob)
  putStrLn $ "Alice: " ++ show aliceBalance' ++ ", Bob: " ++ show bobBalance'

demonstrateAsync :: IO ()
demonstrateAsync = do
  putStrLn "\nAsync Parallel Computation Demo:"
  let numbers = [1..1000000]
  
  start <- getCurrentTime
  result <- parallelSum numbers
  end <- getCurrentTime
  
  putStrLn $ "Sum: " ++ show result
  putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

demonstrateMessagePassing :: IO ()
demonstrateMessagePassing = do
  putStrLn "\nMessage Passing Demo:"
  chan <- newChan
  
  -- Start supervised worker
  _ <- forkIO $ supervisedWorker chan
  
  -- Send some messages
  forM_ [1..5] $ \i -> do
    writeChan chan (Message ("Task " ++ show i) i)
    threadDelay 50000
  
  threadDelay 1000000

main :: IO ()
main = do
  putStrLn "Advanced Concurrency Patterns in Haskell"
  putStrLn "========================================"
  demonstrateSTM
  demonstrateAsync
  demonstrateMessagePassing
  putStrLn "\nHaskell provides powerful abstractions for safe concurrent programming!"