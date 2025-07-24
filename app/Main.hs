{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Text.IO as TIO
import System.Environment

import AIAgent.Examples.Simple

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["simple"] -> runSimpleExample
    ["data"] -> runDataProcessingExample
    ["conditional"] -> runConditionalExample
    ["parallel"] -> runParallelExample
    ["all"] -> runAllExamples
    _ -> do
      putStrLn "AI Agent Framework Demo"
      putStrLn "Usage: ai-agent-example [simple|data|conditional|parallel|all]"
      putStrLn ""
      putStrLn "Examples:"
      putStrLn "  simple      - Run simple calculator workflow"
      putStrLn "  data        - Run data processing workflow"
      putStrLn "  conditional - Run conditional routing workflow"
      putStrLn "  parallel    - Run parallel processing workflow"
      putStrLn "  all         - Run all examples"

runAllExamples :: IO ()
runAllExamples = do
  putStrLn "Running all AI Agent Framework examples...\n"
  
  runSimpleExample
  runDataProcessingExample
  runConditionalExample
  runParallelExample
  
  putStrLn "\n=== All Examples Completed ==="