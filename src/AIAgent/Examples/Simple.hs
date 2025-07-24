{-# LANGUAGE OverloadedStrings #-}

module AIAgent.Examples.Simple
  ( -- * Example Workflows
    simpleCalculatorWorkflow
  , dataProcessingWorkflow
  , conditionalWorkflow
  , parallelWorkflow
  
    -- * Example Agents
  , calculatorAgent
  , dataFilterAgent
  , summaryAgent
  , validationAgent
  
    -- * Demo Functions
  , runSimpleExample
  , runDataProcessingExample
  , runConditionalExample
  , runParallelExample
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)

import AIAgent.Core.State
import AIAgent.Core.Node
import AIAgent.Core.Graph
import AIAgent.Core.Executor
import AIAgent.Agents.Base

-- | A simple calculator agent that performs basic arithmetic
calculatorAgent :: Agent
calculatorAgent = mkStatelessAgent "Calculator" [Computation] $ \input -> do
  case (HM.lookup "operation" input, HM.lookup "a" input, HM.lookup "b" input) of
    (Just (String op), Just (Number a), Just (Number b)) -> do
      let result = case op of
            "add" -> a + b
            "subtract" -> a - b
            "multiply" -> a * b
            "divide" -> if b == 0 then 0 else a / b
            _ -> 0
      return $ Right $ object ["result" .= result, "operation" .= op]
    _ -> return $ Left "Invalid input: need operation, a, and b"

-- | A data filter agent that filters and processes data
dataFilterAgent :: Agent
dataFilterAgent = mkStatelessAgent "DataFilter" [DataRetrieval, Computation] $ \input -> do
  case HM.lookup "data" input of
    Just (Array items) -> do
      case HM.lookup "filter" input of
        Just (String "positive") -> do
          let positiveNumbers = filter isPositiveNumber items
          return $ Right $ object ["filtered_data" .= positiveNumbers]
        Just (String "negative") -> do
          let negativeNumbers = filter isNegativeNumber items
          return $ Right $ object ["filtered_data" .= negativeNumbers]
        _ -> return $ Right $ object ["filtered_data" .= items]
    _ -> return $ Left "Invalid input: need array data"
  where
    isPositiveNumber (Number n) = n > 0
    isPositiveNumber _ = False
    
    isNegativeNumber (Number n) = n < 0
    isNegativeNumber _ = False

-- | A summary agent that creates summaries of data
summaryAgent :: Agent
summaryAgent = mkStatelessAgent "Summary" [TextGeneration, Computation] $ \input -> do
  case HM.lookup "data" input of
    Just (Array items) -> do
      let numbers = [n | Number n <- items]
          count = length numbers
          total = sum numbers
          average = if count > 0 then total / fromIntegral count else 0
      return $ Right $ object 
        [ "summary" .= object
            [ "count" .= count
            , "total" .= total
            , "average" .= average
            ]
        ]
    _ -> return $ Left "Invalid input: need array data"

-- | A validation agent that checks data integrity
validationAgent :: Agent
validationAgent = mkStatelessAgent "Validator" [DecisionMaking] $ \input -> do
  case HM.lookup "data" input of
    Just (Array items) -> do
      let allNumbers = all isNumber items
          nonEmpty = not (null items)
          valid = allNumbers && nonEmpty
      return $ Right $ object 
        [ "valid" .= valid
        , "all_numbers" .= allNumbers
        , "non_empty" .= nonEmpty
        ]
    _ -> return $ Left "Invalid input: need array data"
  where
    isNumber (Number _) = True
    isNumber _ = False

-- | Create a simple calculator workflow
simpleCalculatorWorkflow :: IO Graph
simpleCalculatorWorkflow = do
  -- Create nodes from agents
  let calcNode = mkIONode "calculator" "Perform calculation" $ \state -> do
        result <- runAgent calculatorAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  -- Create a simple graph with one node
  let graph = emptyGraph
              & addNode calcNode
  
  return graph

-- | Create a data processing workflow
dataProcessingWorkflow :: IO Graph
dataProcessingWorkflow = do
  -- Create nodes
  let validatorNode = mkIONode "validator" "Validate data" $ \state -> do
        result <- runAgent validationAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  let filterNode = mkIONode "filter" "Filter data" $ \state -> do
        result <- runAgent dataFilterAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  let summaryNode = mkIONode "summary" "Summarize data" $ \state -> do
        result <- runAgent summaryAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  -- Create graph with sequential processing
  let graph = emptyGraph
              & addNode validatorNode
              & addNode filterNode
              & addNode summaryNode
              & addEdge "validator" "filter"
              & addEdge "filter" "summary"
  
  return graph

-- | Create a conditional workflow that routes based on validation
conditionalWorkflow :: IO Graph
conditionalWorkflow = do
  let validatorNode = mkIONode "validator" "Validate data" $ \state -> do
        result <- runAgent validationAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  let filterNode = mkIONode "filter" "Filter data" $ \state -> do
        result <- runAgent dataFilterAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  let errorNode = mkPureNode "error" "Handle error" $ \_ -> 
        Right $ object ["error" .= ("Data validation failed" :: Text)]
  
  let summaryNode = mkIONode "summary" "Summarize data" $ \state -> do
        result <- runAgent summaryAgent state
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  -- Create conditional edges
  let validCondition = OnValue "valid" (Bool True)
      invalidCondition = OnValue "valid" (Bool False)
  
  let graph = emptyGraph
              & addNode validatorNode
              & addNode filterNode
              & addNode errorNode
              & addNode summaryNode
              & addConditionalEdge "validator" "filter" validCondition HM.empty
              & addConditionalEdge "validator" "error" invalidCondition HM.empty
              & addEdge "filter" "summary"
  
  return graph

-- | Create a parallel workflow
parallelWorkflow :: IO Graph
parallelWorkflow = do
  let filterPositiveNode = mkIONode "filter_positive" "Filter positive numbers" $ \state -> do
        let input = HM.insert "filter" (String "positive") state
        result <- runAgent dataFilterAgent input
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  let filterNegativeNode = mkIONode "filter_negative" "Filter negative numbers" $ \state -> do
        let input = HM.insert "filter" (String "negative") state
        result <- runAgent dataFilterAgent input
        case result of
          Right value -> return $ Right value
          Left err -> return $ Left err
  
  let combineNode = mkPureNode "combine" "Combine results" $ \state -> do
        case (HM.lookup "positive_result" state, HM.lookup "negative_result" state) of
          (Just pos, Just neg) -> 
            Right $ object ["combined" .= object ["positive" .= pos, "negative" .= neg]]
          _ -> Left "Missing results from parallel processing"
  
  let graph = emptyGraph
              & addNode filterPositiveNode
              & addNode filterNegativeNode
              & addNode combineNode
              & addEdge "filter_positive" "combine"
              & addEdge "filter_negative" "combine"
              & graphConfig . configStrategy .~ Parallel
  
  return graph

-- | Run a simple calculator example
runSimpleExample :: IO ()
runSimpleExample = do
  putStrLn "=== Simple Calculator Example ==="
  
  -- Create initial state
  let initialData = HM.fromList 
        [ ("operation", String "add")
        , ("a", Number 10)
        , ("b", Number 5)
        ]
  
  state <- newAgentState (Just initialData)
  graph <- simpleCalculatorWorkflow
  executor <- newExecutor graph state
  
  -- Execute the workflow
  result <- executeGraph executor
  
  -- Display results
  putStrLn $ "Execution completed with " ++ show (length $ _resultErrors result) ++ " errors"
  putStrLn $ "Results: " ++ show (_resultNodes result)
  
  -- Show final state
  finalState <- getState (_resultState result)
  putStrLn $ "Final state: " ++ show finalState

-- | Run a data processing example
runDataProcessingExample :: IO ()
runDataProcessingExample = do
  putStrLn "\n=== Data Processing Example ==="
  
  -- Create initial state with sample data
  let sampleData = Array [Number 1, Number (-2), Number 3, Number (-4), Number 5]
      initialData = HM.fromList [("data", sampleData)]
  
  state <- newAgentState (Just initialData)
  graph <- dataProcessingWorkflow
  executor <- newExecutor graph state
  
  -- Execute the workflow
  result <- executeGraph executor
  
  -- Display results
  putStrLn $ "Execution completed with " ++ show (length $ _resultErrors result) ++ " errors"
  
  -- Show execution visualization
  visualization <- visualizeExecution executor
  putStrLn $ "Execution visualization:\n" ++ Text.unpack visualization

-- | Run a conditional workflow example
runConditionalExample :: IO ()
runConditionalExample = do
  putStrLn "\n=== Conditional Workflow Example ==="
  
  -- Test with valid data
  putStrLn "Testing with valid data:"
  let validData = Array [Number 1, Number 2, Number 3]
      initialData = HM.fromList [("data", validData)]
  
  state <- newAgentState (Just initialData)
  graph <- conditionalWorkflow
  executor <- newExecutor graph state
  
  result <- executeGraph executor
  visualization <- visualizeExecution executor
  putStrLn $ Text.unpack visualization
  
  -- Test with invalid data
  putStrLn "\nTesting with invalid data:"
  let invalidData = Array [String "not", String "numbers"]
      invalidInitialData = HM.fromList [("data", invalidData)]
  
  state2 <- newAgentState (Just invalidInitialData)
  executor2 <- newExecutor graph state2
  
  result2 <- executeGraph executor2
  visualization2 <- visualizeExecution executor2
  putStrLn $ Text.unpack visualization2

-- | Run a parallel processing example
runParallelExample :: IO ()
runParallelExample = do
  putStrLn "\n=== Parallel Processing Example ==="
  
  let mixedData = Array [Number 1, Number (-2), Number 3, Number (-4), Number 5]
      initialData = HM.fromList [("data", mixedData)]
  
  state <- newAgentState (Just initialData)
  graph <- parallelWorkflow
  executor <- newExecutor graph state
  
  -- Execute with parallel strategy
  result <- executeWithStrategy executor Parallel
  
  -- Display results
  metrics <- getExecutionMetrics executor
  putStrLn $ "Parallel execution completed in: " ++ 
    show (_metricsTotalDuration metrics) ++ " seconds"
  
  visualization <- visualizeExecution executor
  putStrLn $ Text.unpack visualization