{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import System.Environment

import AIAgent.Examples.Simple
import AIAgent.Examples.ChatBot

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["simple"]      -> runSimpleExample
    ["data"]        -> runDataProcessingExample
    ["conditional"] -> runConditionalExample
    ["parallel"]    -> runParallelExample
    ["chat"]        -> runSimpleChatDemo
    ["calculator"]  -> runCalculatorChatDemo
    ["file"]        -> runFileAssistantDemo
    ["all"]         -> runAllExamples
    _ -> do
      putStrLn "AI Agent Framework Demo"
      putStrLn "Usage: ai-agent-example [simple|data|conditional|parallel|chat|calculator|file|all]"
      putStrLn ""
      putStrLn "Examples:"
      putStrLn "  simple      - Run simple calculator workflow"
      putStrLn "  data        - Run data processing workflow"
      putStrLn "  conditional - Run conditional routing workflow"
      putStrLn "  parallel    - Run parallel processing workflow"
      putStrLn "  chat        - Run simple chatbot demo"
      putStrLn "  calculator  - Run calculator chatbot demo"
      putStrLn "  file        - Run file assistant demo"
      putStrLn "  all         - Run all examples"

runAllExamples :: IO ()
runAllExamples = do
  putStrLn "Running all AI Agent Framework examples...\n"

  runSimpleExample
  runDataProcessingExample
  runConditionalExample
  runParallelExample
  runSimpleChatDemo
  runCalculatorChatDemo
  runFileAssistantDemo

  putStrLn "\n=== All Examples Completed ==="