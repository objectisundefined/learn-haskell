# Learn Haskell - Comprehensive Examples Collection

This repository showcases the **powerful features and capabilities of Haskell** through practical, well-documented examples. From basic concepts to advanced programming techniques, these examples demonstrate why Haskell is one of the most expressive and mathematically elegant programming languages.

## 🚀 New Advanced Examples (Recently Added)

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

1. **Start with fundamentals**: `five-fundamental-monads.hs`
2. **Explore type safety**: `type-level-programming.hs`
3. **Understand laziness**: `lazy-infinite-structures.hs`
4. **Learn concurrency**: `advanced-concurrency.hs`
5. **Master abstractions**: `category-theory-practice.hs`
6. **Build DSLs**: `free-monads-dsl.hs`
7. **Advanced techniques**: `metaprogramming-generics.hs`

## 🎓 What Makes Haskell Unique

- **Zero-cost abstractions**: High-level code compiles to efficient machine code
- **Type inference**: Write less, express more with automatic type deduction
- **Lazy by default**: Elegant solutions to complex problems
- **Mathematical foundation**: Based on solid theoretical principles
- **Composability**: Small pieces combine into powerful systems
- **Safety**: Many runtime errors become compile-time errors

These examples demonstrate that Haskell isn't just an academic language—it's a powerful tool for building robust, efficient, and maintainable software systems.

---

*Explore the code, experiment with the examples, and discover the elegance and power of functional programming with Haskell!*
