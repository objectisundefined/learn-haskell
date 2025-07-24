# AI Agent Framework - A LangGraph-like Framework in Haskell

A functional, type-safe AI agent framework inspired by LangGraph, built in Haskell. This framework provides a powerful foundation for building AI agent workflows with graph-based execution, state management, and composable agents.

## Features

- **🔄 Graph-Based Execution**: Build complex workflows as directed graphs with conditional routing
- **🏠 State Management**: Thread-safe state management with STM, history tracking, and rollback capabilities
- **🤖 Composable Agents**: Modular agents with different capabilities that can be chained and combined
- **🛠️ Tool Integration**: Easy integration of external tools and functions
- **⚡ Concurrent Execution**: Support for sequential, parallel, and conditional execution strategies
- **💾 Memory System**: Built-in memory management for agent conversations and learning
- **🔍 Monitoring & Metrics**: Comprehensive execution metrics and visualization
- **🎯 Type Safety**: Leverages Haskell's type system for reliable, compile-time guarantees

## Quick Start

### Prerequisites

- GHC 9.0+ 
- Cabal 3.0+

### Installation

```bash
git clone <repository-url>
cd ai-agent-framework
cabal update
cabal build
```

### Running Examples

```bash
# Run all examples
cabal run ai-agent-example all

# Run specific examples
cabal run ai-agent-example simple
cabal run ai-agent-example data
cabal run ai-agent-example conditional
cabal run ai-agent-example parallel
```

## Architecture Overview

The framework consists of several core components:

### Core Components

1. **State Management** (`AIAgent.Core.State`)
   - Thread-safe state with STM
   - History tracking and snapshots
   - Rollback capabilities

2. **Nodes** (`AIAgent.Core.Node`) 
   - Computational units in the graph
   - Support for pure, IO, and async operations
   - Error handling and retries

3. **Graphs** (`AIAgent.Core.Graph`)
   - Directed graph structure
   - Conditional edges and routing
   - Validation and cycle detection

4. **Executor** (`AIAgent.Core.Executor`)
   - Graph execution engine
   - Multiple execution strategies
   - Monitoring and metrics

### Agent System

5. **Base Agents** (`AIAgent.Agents.Base`)
   - Agent interface and capabilities
   - Memory management
   - Agent combinators

6. **LLM Agents** (`AIAgent.Agents.LLM`)
   - Language model integration
   - Placeholder for future LLM providers

### Tool System

7. **Tools** (`AIAgent.Tools.Base`, `AIAgent.Tools.Function`)
   - External tool integration
   - Function wrapping with schema validation
   - Predefined tool collections

## Core Concepts

### Agents

Agents are the primary computational units with specific capabilities:

```haskell
-- Create a simple calculation agent
calculatorAgent :: Agent
calculatorAgent = mkStatelessAgent "Calculator" [Computation] $ \input -> do
  case (HM.lookup "operation" input, HM.lookup "a" input, HM.lookup "b" input) of
    (Just (String op), Just (Number a), Just (Number b)) -> do
      let result = case op of
            "add" -> a + b
            "subtract" -> a - b
            "multiply" -> a * b
            "divide" -> if b == 0 then 0 else a / b
      return $ Right $ object ["result" .= result]
    _ -> return $ Left "Invalid input"
```

### Graphs

Build workflows as graphs with conditional routing:

```haskell
-- Create a data processing workflow
dataProcessingWorkflow :: IO Graph
dataProcessingWorkflow = do
  let validatorNode = mkIONode "validator" "Validate data" validateAction
      filterNode = mkIONode "filter" "Filter data" filterAction
      summaryNode = mkIONode "summary" "Summarize data" summaryAction
  
  let graph = emptyGraph
              & addNode validatorNode
              & addNode filterNode 
              & addNode summaryNode
              & addEdge "validator" "filter"
              & addEdge "filter" "summary"
  
  return graph
```

### Conditional Routing

Add conditional edges based on state:

```haskell
-- Route based on validation results
let validCondition = OnValue "valid" (Bool True)
    invalidCondition = OnValue "valid" (Bool False)

let graph = emptyGraph
            & addNode validatorNode
            & addNode processNode
            & addNode errorNode
            & addConditionalEdge "validator" "process" validCondition HM.empty
            & addConditionalEdge "validator" "error" invalidCondition HM.empty
```

### Execution

Execute graphs with different strategies:

```haskell
-- Execute with parallel strategy
state <- newAgentState (Just initialData)
executor <- newExecutor graph state
result <- executeWithStrategy executor Parallel

-- Monitor execution
metrics <- getExecutionMetrics executor
visualization <- visualizeExecution executor
```

## Examples

### 1. Simple Calculator

A basic workflow that performs arithmetic operations:

```haskell
-- Input: {"operation": "add", "a": 10, "b": 5}
-- Output: {"result": 15, "operation": "add"}
cabal run ai-agent-example simple
```

### 2. Data Processing Pipeline

Sequential processing with validation, filtering, and summarization:

```haskell
-- Input: {"data": [1, -2, 3, -4, 5]}
-- Validates → Filters → Summarizes data
cabal run ai-agent-example data
```

### 3. Conditional Workflow

Demonstrates conditional routing based on validation results:

```haskell
-- Routes to different paths based on data validity
cabal run ai-agent-example conditional
```

### 4. Parallel Processing

Concurrent execution of independent operations:

```haskell
-- Processes positive and negative numbers in parallel
cabal run ai-agent-example parallel
```

## Advanced Features

### Agent Combinators

Chain and combine agents:

```haskell
-- Chain agents sequentially
chainAgents [agent1, agent2, agent3] input

-- Run agents in parallel
parallelAgents [agent1, agent2, agent3] input

-- Add retry capability
retryAgent 3 originalAgent

-- Create fallback chain
fallbackAgent primaryAgent [fallback1, fallback2]
```

### Memory Management

Agents can maintain memory across interactions:

```haskell
-- Create agent with memory
agent = mkStatefulAgent "ChatBot" [TextGeneration] $ \memory input -> do
  -- Access previous interactions
  history <- readTVarIO (memory ^. memoryShortTerm)
  -- Store new information
  addToMemory memory newValue
  -- Return response
  return $ Right response
```

### Tool Integration

Wrap functions as tools:

```haskell
-- Mathematical tools
mathTools :: [FunctionTool]
mathTools = [mkMathTool "add" (+), mkMathTool "multiply" (*)]

-- String manipulation tools  
stringTools :: [FunctionTool]
stringTools = [mkStringTool "uppercase" Text.toUpper]

-- Convert tools to agents
toolAgent = combineTools (map funcTool mathTools)
```

### State Management

Advanced state operations:

```haskell
-- Create state with initial data
state <- newAgentState (Just initialData)

-- Update with different modes
updateState state updates Merge (Just "node_id")
appendState state newValues (Just "node_id")

-- Take snapshots and rollback
snapshot <- takeSnapshot state (Just "checkpoint")
success <- rollbackToSnapshot state snapshotId

-- Serialize state
json <- stateToJSON state
```

## Configuration

### Graph Configuration

```haskell
-- Customize execution behavior
let config = GraphConfig
      { _configStrategy = Parallel
      , _configMaxParallel = 4
      , _configTimeout = Just 30
      , _configRetryCount = 3
      , _configFailFast = True
      }

let graph = emptyGraph & graphConfig .~ config
```

### Agent Configuration

```haskell
-- Configure agent behavior
let config = AgentConfig
      { _configMaxRetries = 3
      , _configTimeout = Just 30
      , _configVerbose = True
      , _configMemoryEnabled = True
      }

let agent = baseAgent & agentConfig .~ config
```

## Error Handling

The framework provides comprehensive error handling:

```haskell
-- Node execution errors
data NodeError = NodeError
  { _errorMessage :: Text
  , _errorCode :: Maybe Text
  , _errorDetails :: Maybe Value
  , _errorTimestamp :: UTCTime
  }

-- Execution errors
data ExecutionError = ExecutionError
  { _execErrorNode :: NodeId
  , _execErrorMessage :: Text
  , _execErrorTime :: UTCTime
  }

-- Check for errors
hasErrors <- hasExecutionErrors executor
failedNodes <- getFailedNodes executor
```

## Monitoring & Metrics

Track execution performance:

```haskell
-- Execution metrics
data ExecutionMetrics = ExecutionMetrics
  { _metricsStartTime :: UTCTime
  , _metricsNodesExecuted :: Int
  , _metricsNodesFailed :: Int
  , _metricsTotalDuration :: Maybe NominalDiffTime
  , _metricsNodeDurations :: HashMap NodeId NominalDiffTime
  }

-- Get metrics
metrics <- getExecutionMetrics executor
history <- getNodeExecutionHistory executor
visualization <- visualizeExecution executor
```

## Framework Statistics

**Total Implementation**: 2,699 lines of Haskell code across 10 modules

### Module Breakdown:
- **Core/Executor.hs**: 462 lines - Graph execution engine
- **Core/Graph.hs**: 335 lines - Graph structure and algorithms  
- **Examples/Simple.hs**: 329 lines - Basic workflow examples
- **Examples/ChatBot.hs**: 324 lines - Conversational agent examples
- **Agents/Base.hs**: 308 lines - Agent interface and combinators
- **Core/Node.hs**: 255 lines - Computational nodes
- **Tools/Function.hs**: 206 lines - Function tool wrapping
- **Core/State.hs**: 197 lines - STM-based state management
- **Tools/Base.hs**: 162 lines - Tool integration interface
- **Agents/LLM.hs**: 121 lines - LLM integration placeholder

## Testing

Run the validation script:

```bash
python3 validate_framework.py
```

This will check:
- ✅ Project structure completeness
- ✅ Module declarations and exports  
- ✅ Key functionality availability
- ✅ Code statistics and metrics

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Future Roadmap

- [ ] **LLM Integration**: Real OpenAI/Anthropic API integration
- [ ] **Persistence**: Database backends for state and memory
- [ ] **Distributed Execution**: Remote agent execution
- [ ] **Web Interface**: Browser-based workflow designer
- [ ] **Streaming**: Support for streaming responses
- [ ] **Plugin System**: Dynamic loading of agents and tools
- [ ] **Debugging Tools**: Step-through debugging and visualization
- [ ] **Performance Optimization**: Compilation to efficient execution graphs

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Inspired by [LangGraph](https://github.com/langchain-ai/langgraph) from LangChain
- Built with Haskell's powerful type system and STM concurrency
- Uses lens for elegant data manipulation
- Leverages Aeson for JSON serialization

## Contact

For questions, suggestions, or contributions, please open an issue on GitHub.

---

*Built with ❤️ in Haskell*