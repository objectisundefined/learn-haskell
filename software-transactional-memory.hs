{-
Software Transactional Memory (STM) Examples
============================================

STM provides composable atomic operations without locks.
Key benefits:
- Composable atomic blocks
- No deadlocks or race conditions  
- Automatic retry on conflicts
- Clean separation of concerns
-}

{-# LANGUAGE NumericUnderscores #-}

module SoftwareTransactionalMemory where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

-- Basic STM Example: Shared Counter
data Counter = Counter (TVar Int)

newCounter :: Int -> IO Counter
newCounter initial = Counter <$> newTVarIO initial

increment :: Counter -> STM ()
increment (Counter tvar) = do
  current <- readTVar tvar
  writeTVar tvar (current + 1)

decrement :: Counter -> STM ()
decrement (Counter tvar) = do
  current <- readTVar tvar
  writeTVar tvar (current - 1)

getValue :: Counter -> STM Int
getValue (Counter tvar) = readTVar tvar

-- Bank Account Example with STM
data Account = Account 
  { accountId :: Int
  , balance :: TVar Int
  , transactions :: TVar [String]
  }

newAccount :: Int -> Int -> IO Account
newAccount aid initialBalance = do
  bal <- newTVarIO initialBalance
  trans <- newTVarIO []
  return $ Account aid bal trans

-- Safe money transfer using STM
transfer :: Account -> Account -> Int -> STM Bool
transfer from to amount = do
  fromBalance <- readTVar (balance from)
  if fromBalance >= amount
    then do
      -- Debit from source
      writeTVar (balance from) (fromBalance - amount)
      -- Credit to destination
      toBalance <- readTVar (balance to)
      writeTVar (balance to) (toBalance + amount)
      
      -- Log transactions
      fromTrans <- readTVar (transactions from)
      toTrans <- readTVar (transactions to)
      writeTVar (transactions from) 
        (("Transfer out: -" ++ show amount ++ " to " ++ show (accountId to)) : fromTrans)
      writeTVar (transactions to)
        (("Transfer in: +" ++ show amount ++ " from " ++ show (accountId from)) : toTrans)
      
      return True
    else return False

-- Conditional waiting with STM retry
waitForBalance :: Account -> Int -> STM ()
waitForBalance account minBalance = do
  current <- readTVar (balance account)
  if current >= minBalance
    then return ()
    else retry

-- STM with orElse for alternatives
withdrawOrWait :: Account -> Int -> STM Bool
withdrawOrWait account amount = 
  attemptWithdraw `orElse` waitAndWithdraw
  where
    attemptWithdraw = do
      current <- readTVar (balance account)
      if current >= amount
        then do
          writeTVar (balance account) (current - amount)
          return True
        else retry
    
    waitAndWithdraw = do
      waitForBalance account amount
      current <- readTVar (balance account)
      writeTVar (balance account) (current - amount)
      return True

-- Producer-Consumer with STM
data STMQueue a = STMQueue 
  { items :: TVar [a]
  , maxSize :: Int
  }

newSTMQueue :: Int -> IO (STMQueue a)
newSTMQueue size = do
  itemsVar <- newTVarIO []
  return $ STMQueue itemsVar size

enqueue :: STMQueue a -> a -> STM ()
enqueue queue item = do
  currentItems <- readTVar (items queue)
  if length currentItems < maxSize queue
    then writeTVar (items queue) (item : currentItems)
    else retry

dequeue :: STMQueue a -> STM a
dequeue queue = do
  currentItems <- readTVar (items queue)
  case reverse currentItems of
    [] -> retry
    (x:xs) -> do
      writeTVar (items queue) (reverse xs)
      return x

-- Demonstration functions
demonstrateBasicSTM :: IO ()
demonstrateBasicSTM = do
  putStrLn "=== Basic STM Counter ==="
  counter <- newCounter 0
  
  -- Spawn multiple threads incrementing concurrently
  asyncs <- replicateM 10 $ async $ do
    replicateM_ 1000 $ atomically $ increment counter
  
  -- Wait for all to complete
  mapM_ wait asyncs
  
  -- Read final value
  finalValue <- atomically $ getValue counter
  putStrLn $ "Final counter value: " ++ show finalValue ++ " (should be 10000)"

demonstrateBankTransfer :: IO ()
demonstrateBankTransfer = do
  putStrLn "\n=== Bank Transfer Demo ==="
  alice <- newAccount 1 1000
  bob <- newAccount 2 500
  
  -- Initial balances
  putStrLn "Initial balances:"
  aliceBal <- atomically $ readTVar (balance alice)
  bobBal <- atomically $ readTVar (balance bob)
  putStrLn $ "Alice: $" ++ show aliceBal ++ ", Bob: $" ++ show bobBal
  
  -- Concurrent transfers
  transfers <- sequence 
    [ async $ atomically $ transfer alice bob 100
    , async $ atomically $ transfer bob alice 50
    ]
  
  results <- mapM wait transfers
  putStrLn $ "Transfer results: " ++ show results
  
  -- Final balances
  putStrLn "Final balances:"
  aliceBal' <- atomically $ readTVar (balance alice)
  bobBal' <- atomically $ readTVar (balance bob)
  putStrLn $ "Alice: $" ++ show aliceBal' ++ ", Bob: $" ++ show bobBal'

main :: IO ()
main = do
  putStrLn "Software Transactional Memory Examples"
  putStrLn "====================================="
  
  demonstrateBasicSTM
  demonstrateBankTransfer
  
  putStrLn "\nSTM provides composable, deadlock-free concurrency!"