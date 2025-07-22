# Learn Haskell - Comprehensive Examples Collection

This repository showcases the **powerful features and capabilities of Haskell** through practical, well-documented examples. From basic concepts to advanced programming techniques, these examples demonstrate why Haskell is one of the most expressive and mathematically elegant programming languages.

## 🚀 New Advanced Examples (Recently Added)

### **Side Effects & IO Management** (🆕 **Comprehensive Collection**)

#### **Software Transactional Memory** (`software-transactional-memory.hs`)
- Composable atomic operations without locks
- Bank transfer examples with automatic retry
- Producer-consumer patterns with STM queues
- Deadlock-free concurrent programming
- STM alternatives with `orElse` for fallback strategies

#### **Effect Systems** (`effect-systems.hs`)
- Monad Transformer stacks (ReaderT + StateT + ExceptT + IO)
- Writer monad for logging and audit trails
- Exception handling with ExceptT composition
- Resource management patterns with bracket
- Custom effect interpretations and DSLs

#### **Exception Handling** (`exception-handling.hs`)
- Pure error handling with Maybe and Either types
- Custom exception types for domain-specific errors
- Safe resource management with bracket patterns
- Retry mechanisms and graceful error recovery
- Multiple exception handlers and error composition

#### **Resource Management** (`resource-management.hs`)
- RAII patterns for automatic cleanup
- File, network, and memory resource safety
- Lock management and critical sections
- Resource pools for connection management
- Monitoring and tracking resource usage

#### **IO and Side Effects** (`io-side-effects.hs`)
- Pure vs impure function distinctions
- Mutable references (IORef, MVar, STM)
- Random number generation and shuffling
- Time operations and performance measurement
- Network programming and HTTP requests
- Concurrent programming patterns
- Lazy IO considerations and pitfalls

### **Lazy Evaluation & Infinite Data Structures** (`lazy-infinite-structures.hs`)
- Infinite lists and streams with constant memory usage
- Fibonacci sequences, prime sieves, and Hamming numbers
- Lazy evaluation examples and space-efficient algorithms
- Pascal's triangle and infinite trees

### **Type-Level Programming** (`type-level-programming.hs`) 
- Compile-time computation with type families
- Length-indexed vectors for array safety
- Type-level arithmetic and logic
- Phantom types for units of measure
- GADTs for expressing complex constraints

### **Free Monads & DSLs** (`free-monads-dsl.hs`)
- Domain Specific Language creation
- Separation of program structure from interpretation
- Multiple interpreters (testing, production, logging)
- Console I/O, file system, and database DSLs

### **Category Theory in Practice** (`category-theory-practice.hs`)
- Functors, Applicatives, and Monads explained
- Kleisli composition and arrows
- Natural transformations and Yoneda lemma
- Comonads and mathematical abstractions
- Real-world applications of abstract mathematics

### **Advanced Concurrency** (`advanced-concurrency.hs`)
- Software Transactional Memory (STM) for lock-free programming
- Parallel algorithms with `async`
- Message passing and actor patterns
- Lock-free data structures and rate limiting
- High-performance concurrent programming

### **Metaprogramming & Generics** (`metaprogramming-generics.hs`)
- Template Haskell for compile-time code generation
- Generic programming with GHC.Generics
- Automatic lens generation
- Reflective programming with Data.Data
- Type-safe metaprogramming techniques

## 🎯 Existing Examples (Foundational Concepts)

### **Core Language Features**
- **Monads**: Reader, Writer, State transformers (`five-fundamental-monads.hs`)
- **Lenses**: Simple and advanced lens libraries (`simple-lens.hs`, `advanced-lens.hs`)
- **JSON Parsing**: Full parser combinator implementation (`json-parser.hs`)
- **Template Haskell**: Metaprogramming examples (`template-programming.hs`)

### **Data Structures & Algorithms**
- **Red-Black Trees**: Self-balancing binary search trees (`red-black-tree.hs`)
- **Binary Trees**: Tree operations and traversals (`binary-tree.hs`) 
- **Hash Maps/Sets**: Functional hash table implementations (`hash-map.hs`, `hash-set.hs`)
- **HList**: Heterogeneous lists with type safety (`hlist.hs`)

### **Advanced Type System**
- **GADTs**: Generalized Algebraic Data Types (`learn-gadt.hs`)
- **Dependent Types**: Type-level programming (`learn-dependent-type.hs`)
- **Zipper Data Structures**: Functional navigation (`zipper-data-structures.hs`)

### **Practical Applications**
- **Argument Parsing**: Command-line interface building (`arg-parser.hs`)
- **Web Scraping**: Git repository parsing (`git-parser.hs`)
- **Mathematical Algorithms**: Complex number theory (`last-digit.hs`, `narcissistic-number.hs`)
- **Game Logic**: N-Queens problem, Josephus permutation (`n-queens-problem.hs`)

### **Concurrent Programming**
- **STM Examples**: Software Transactional Memory (`skip-chan.hs`)
- **State Management**: IO state handling (`state-in-io.hs`)
- **Parallel Processing**: Concurrent data structures

## 🔥 Why These Examples Showcase Haskell's Power

### **1. Mathematical Elegance**
- **Category Theory**: Direct implementation of mathematical concepts
- **Type Safety**: Compile-time guarantees prevent entire classes of bugs
- **Composability**: Small functions combine to create complex behaviors

### **2. Advanced Type System**
- **Type-Level Computation**: Calculations performed at compile time
- **GADTs**: Express complex invariants in the type system
- **Type Families**: Generic programming with type-level functions

### **3. Lazy Evaluation**
- **Infinite Data Structures**: Work with unlimited data efficiently
- **Space Efficiency**: Only compute what's needed, when it's needed
- **Elegant Algorithms**: Express complex logic naturally

### **4. Concurrency Excellence**
- **STM**: Lock-free programming that's both safe and composable
- **Parallelism**: Automatic parallel execution with referential transparency
- **Message Passing**: Safe concurrent communication patterns

### **5. Metaprogramming Power**
- **Template Haskell**: Generate code at compile time
- **Generic Programming**: Write once, work for all types
- **DSL Creation**: Build domain-specific languages easily

### **6. Functional Purity**
- **Referential Transparency**: Functions always return the same output for the same input
- **Immutability**: Data structures that can't be accidentally modified
- **Effect Management**: Pure functions with controlled side effects

## 🚀 Getting Started

```bash
# Try the examples
ghc -o lazy-demo lazy-infinite-structures.hs && ./lazy-demo
ghc -o types-demo type-level-programming.hs && ./types-demo
ghc -o concurrency-demo advanced-concurrency.hs && ./concurrency-demo

# Interactive exploration
ghci free-monads-dsl.hs
ghci category-theory-practice.hs
```

## 📚 Learning Path Recommendation

### **Foundation Track**
1. **Start with fundamentals**: `five-fundamental-monads.hs`
2. **Understand IO and side effects**: `io-side-effects.hs`
3. **Master error handling**: `exception-handling.hs`
4. **Learn effect composition**: `effect-systems.hs`

### **Advanced Track**
5. **Explore type safety**: `type-level-programming.hs`
6. **Understand laziness**: `lazy-infinite-structures.hs`
7. **Master concurrency**: `advanced-concurrency.hs` + `software-transactional-memory.hs`
8. **Resource management**: `resource-management.hs`

### **Expert Track**
9. **Master abstractions**: `category-theory-practice.hs`
10. **Build DSLs**: `free-monads-dsl.hs`
11. **Advanced techniques**: `metaprogramming-generics.hs`

## 🎓 What Makes Haskell Unique

- **Zero-cost abstractions**: High-level code compiles to efficient machine code
- **Type inference**: Write less, express more with automatic type deduction
- **Lazy by default**: Elegant solutions to complex problems
- **Mathematical foundation**: Based on solid theoretical principles
- **Composability**: Small pieces combine into powerful systems
- **Safety**: Many runtime errors become compile-time errors
- **Effect control**: Side effects are tracked in the type system, ensuring predictable behavior
- **Concurrency excellence**: STM and async provide safe, composable concurrent programming
- **Pure functional core**: Referential transparency enables powerful optimization and reasoning

These examples demonstrate that Haskell isn't just an academic language—it's a powerful tool for building robust, efficient, and maintainable software systems.

---

*Explore the code, experiment with the examples, and discover the elegance and power of functional programming with Haskell!*
