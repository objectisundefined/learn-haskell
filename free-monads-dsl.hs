{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

{-
Demonstrates Free Monads for building Domain Specific Languages (DSLs)
- Separating program structure from interpretation
- Multiple interpreters for the same DSL
- Composable and testable effect systems
- Real-world examples: file operations, web APIs, databases
-}

module FreeMonadsDSL where

import Control.Monad.Free
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

-- Basic Free Monad example: Simple console I/O DSL
data ConsoleF next
  = GetLine (String -> next)
  | PutLine String next
  deriving Functor

type Console = Free ConsoleF

-- Smart constructors
getLine' :: Console String
getLine' = liftF (GetLine id)

putLine' :: String -> Console ()
putLine' s = liftF (PutLine s ())

-- Example program using our DSL
echoProgram :: Console ()
echoProgram = do
  putLine' "What's your name?"
  name <- getLine'
  putLine' ("Hello, " ++ name ++ "!")

-- Pure interpreter (for testing)
runConsolePure :: [String] -> Console a -> (a, [String])
runConsolePure inputs program = runState (interpretPure program) inputs
  where
    interpretPure :: Console a -> State [String] a
    interpretPure (Pure x) = return x
    interpretPure (Free (GetLine cont)) = do
      inputs <- get
      case inputs of
        [] -> interpretPure (cont "")
        (i:is) -> do
          put is
          interpretPure (cont i)
    interpretPure (Free (PutLine s next)) = do
      -- In a real implementation, we'd accumulate output
      interpretPure next

-- IO interpreter
runConsoleIO :: Console a -> IO a
runConsoleIO (Pure x) = return x
runConsoleIO (Free (GetLine cont)) = do
  line <- getLine
  runConsoleIO (cont line)
runConsoleIO (Free (PutLine s next)) = do
  putStrLn s
  runConsoleIO next

-- File System DSL
data FileSystemF next
  = ReadFile FilePath (String -> next)
  | WriteFile FilePath String next
  | DeleteFile FilePath next
  | ListFiles FilePath ([FilePath] -> next)
  deriving Functor

type FileSystem = Free FileSystemF

-- Smart constructors
readFile' :: FilePath -> FileSystem String
readFile' path = liftF (ReadFile path id)

writeFile' :: FilePath -> String -> FileSystem ()
writeFile' path content = liftF (WriteFile path content ())

deleteFile' :: FilePath -> FileSystem ()
deleteFile' path = liftF (DeleteFile path ())

listFiles' :: FilePath -> FileSystem [FilePath]
listFiles' path = liftF (ListFiles path id)

-- Example file operations program
fileProgram :: FileSystem ()
fileProgram = do
  content <- readFile' "input.txt"
  let processed = map toUpper content
  writeFile' "output.txt" processed
  deleteFile' "temp.txt"
  files <- listFiles' "."
  writeFile' "files.txt" (unlines files)

-- Pure interpreter using Map for testing
type FileSystemState = Map FilePath String

runFileSystemPure :: FileSystemState -> FileSystem a -> (a, FileSystemState)
runFileSystemPure initial program = runState (interpretFS program) initial
  where
    interpretFS :: FileSystem a -> State FileSystemState a
    interpretFS (Pure x) = return x
    interpretFS (Free (ReadFile path cont)) = do
      fs <- get
      let content = Map.findWithDefault "" path fs
      interpretFS (cont content)
    interpretFS (Free (WriteFile path content next)) = do
      modify (Map.insert path content)
      interpretFS next
    interpretFS (Free (DeleteFile path next)) = do
      modify (Map.delete path)
      interpretFS next
    interpretFS (Free (ListFiles path cont)) = do
      fs <- get
      let files = Map.keys fs
      interpretFS (cont files)

-- Database DSL
data DatabaseF next
  = Query String ([Row] -> next)
  | Execute String next
  | Transaction (Free DatabaseF a) (a -> next)
  deriving Functor

type Database = Free DatabaseF
type Row = Map String String

-- Smart constructors
query :: String -> Database [Row]
query sql = liftF (Query sql id)

execute :: String -> Database ()
execute sql = liftF (Execute sql ())

transaction :: Database a -> Database a
transaction action = liftF (Transaction action id)

-- Example database program
dbProgram :: Database ()
dbProgram = do
  users <- query "SELECT * FROM users WHERE active = 1"
  execute "UPDATE users SET last_login = NOW() WHERE id = 1"
  transaction $ do
    execute "INSERT INTO logs (action) VALUES ('user_login')"
    execute "UPDATE stats SET login_count = login_count + 1"

-- Testing with Free Monads
testConsoleProgram :: IO ()
testConsoleProgram = do
  let inputs = ["Alice"]
      (result, remaining) = runConsolePure inputs echoProgram
  putStrLn "Testing console program with pure interpreter:"
  putStrLn $ "Remaining inputs: " ++ show remaining
  
  putStrLn "\nRunning with IO interpreter:"
  runConsoleIO echoProgram

-- Advantages of Free Monads:
-- 1. Separation of concerns - structure vs interpretation
-- 2. Multiple interpreters - testing, production, logging
-- 3. Composability - combine different effect systems
-- 4. Inspection - analyze programs before running them
-- 5. Optimization - transform programs before interpretation

main :: IO ()
main = do
  putStrLn "Free Monads DSL Demonstration"
  putStrLn "============================"
  testConsoleProgram
  putStrLn "\nFree Monads provide powerful abstraction for DSL creation!"