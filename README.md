# Haskell Lox Interpreter

A complete implementation of the **Lox programming language** in Haskell, following the excellent [Crafting Interpreters](https://craftinginterpreters.com/) book by Robert Nystrom.

## 🎯 What is This?

This repository contains a **tree-walk interpreter** for the Lox programming language, implemented entirely in Haskell. Lox is a dynamically-typed, object-oriented programming language designed for educational purposes.

## 🚀 Features

### ✅ Implemented Language Features

- **🔤 Lexical Analysis** - Complete tokenizer/scanner for Lox syntax
- **🌳 Parsing** - Recursive descent parser with proper operator precedence
- **🎭 Expression Evaluation** - Arithmetic, logical, and comparison operations
- **📦 Variables** - Variable declaration, assignment, and scoping
- **🔄 Control Flow** - If statements, while loops, and for loops
- **🎪 Functions** - Function declarations, calls, and recursion
- **📚 Block Scoping** - Proper lexical scoping with environment chains
- **🎨 Built-in Functions** - Clock function and extensible builtin system
- **⚡ REPL** - Interactive Read-Eval-Print Loop for testing
- **📁 File Execution** - Run Lox programs from `.lox` files

### 🛠 Technical Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Scanner.hs    │───▶│   Parser.hs     │───▶│ Interpreter.hs  │───▶│    Output       │
│                 │    │                 │    │                 │    │                 │
│ String → Tokens │    │ Tokens → AST    │    │ AST → Values    │    │ Print Results   │
└─────────────────┘    └─────────────────┘    └─────────────────┘    └─────────────────┘
```

### 📚 Core Modules

- **`Scanner.hs`** - Lexical analysis (string → tokens)
- **`Token.hs`** - Token definitions and types
- **`Expr.hs`** - Abstract Syntax Tree definitions
- **`Parser.hs`** - Recursive descent parser (tokens → AST)
- **`Interpreter.hs`** - Tree-walk interpreter (AST → execution)
- **`Environment.hs`** - Variable scoping and environment management
- **`Lox.hs`** - Main interpreter driver and error handling
- **`Main.hs`** - CLI interface and file/REPL modes

## 🎮 Example Lox Code

```javascript
// Variables and arithmetic
var a = 10;
var b = 20;
print a + b; // 30

// String concatenation
var greeting = "Hello, ";
var name = "World";
print greeting + name + "!"; // Hello, World!

// Control flow
if (a > 5) {
    print "a is greater than 5";
}

// Loops
for (var i = 0; i < 3; i = i + 1) {
    print "Count: " + i;
}

// Functions
fun fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

print fibonacci(10); // 55

// Classes and objects (if implemented)
class Person {
    init(name) {
        this.name = name;
    }
    
    sayHello() {
        print "Hello, I'm " + this.name;
    }
}

var person = Person("Alice");
person.sayHello(); // Hello, I'm Alice
```

## 🛠 Getting Started

### Prerequisites

- **Haskell Stack** - The Haskell build tool

### Quick Start

1. **Clone the repository**
   ```bash
   git clone <your-repo-url>
   cd Crafting-Interpretors
   ```

2. **Build the project**
   ```bash
   stack build
   ```

3. **Run the REPL**
   ```bash
   stack exec hlox
   ```

4. **Run a Lox file**
   ```bash
   stack exec hlox examples/hello.lox
   ```

For detailed setup instructions, see **[SETUP.md](SETUP.md)**.

## 📁 Project Structure

```
Crafting-Interpretors/
├── README.md              # This file
├── SETUP.md               # Detailed setup instructions
├── stack.yaml             # Stack configuration
├── package.yaml           # Package dependencies
├── hlox.cabal             # Generated Cabal file
│
├── app/
│   └── Main.hs            # CLI entry point
│
├── src/
│   ├── Scanner.hs         # Lexical analyzer
│   ├── Token.hs           # Token definitions
│   ├── Expr.hs            # AST definitions
│   ├── Parser.hs          # Recursive descent parser
│   ├── Interpreter.hs     # Tree-walk interpreter
│   ├── Environment.hs     # Variable scoping
│   └── Lox.hs             # Main interpreter driver
│
├── test/
│   └── Spec.hs            # Haskell unit tests
│
└── examples/              # Example Lox programs
    ├── hello.lox          # Basic language features
    ├── test.lox           # Comprehensive test suite
    ├── test_control_flow.lox
    ├── test_functions.lox
    ├── test_classes.lox
    └── test_expressions.lox
```

## 🧪 Testing

### Example Lox Programs
The `examples/` directory contains several Lox programs that demonstrate different aspects of the language:

- **`hello.lox`** - Basic "Hello, World!" and simple expressions
- **`test.lox`** - Comprehensive test of all language features
- **`test_expressions.lox`** - Arithmetic and logical expressions
- **`test_control_flow.lox`** - If statements, loops, and scoping
- **`test_functions.lox`** - Function definitions and calls
- **`test_classes.lox`** - Object-oriented features

Run any example with:
```bash
stack exec hlox examples/test_expressions.lox
```

### Unit Tests
Run the Haskell unit tests with:
```bash
stack test
```

## 🎓 Learning Goals

This project demonstrates several important concepts in programming language implementation:

- **Lexical Analysis** - Converting source code into tokens
- **Parsing** - Building Abstract Syntax Trees from tokens
- **Tree-Walk Interpretation** - Directly executing AST nodes
- **Environment-Based Scoping** - Managing variable lifetimes
- **Error Handling** - Graceful error reporting and recovery
- **Functional Programming** - Using Haskell's type system and monads

## 🤝 Crafting Interpreters Challenges

This implementation addresses many of the challenges from the [Crafting Interpreters](https://craftinginterpreters.com/) book:

- ✅ **Chapter 4**: Scanning (lexical analysis)
- ✅ **Chapter 5**: Representing Code (AST)
- ✅ **Chapter 6**: Parsing Expressions
- ✅ **Chapter 7**: Evaluating Expressions
- ✅ **Chapter 8**: Statements and State
- ✅ **Chapter 9**: Control Flow
- ✅ **Chapter 10**: Functions
- ✅ **Chapter 11**: Resolving and Binding
- 🚧 **Chapter 12**: Classes (partial implementation)
- 🚧 **Chapter 13**: Inheritance (planned)

## 📖 References

- **[Crafting Interpreters](https://craftinginterpreters.com/)** - The excellent book this implementation follows
- **[Lox Language Specification](https://craftinginterpreters.com/the-lox-language.html)** - Complete language reference
- **[Haskell Stack](https://docs.haskellstack.org/)** - Haskell build tool documentation

## 🤔 Why Haskell?

While the original book uses Java and C, implementing Lox in Haskell provides several benefits:

- **🎯 Type Safety** - Haskell's strong type system catches many errors at compile time
- **🧩 Pattern Matching** - Perfect for AST traversal and evaluation
- **⚡ Monads** - Clean error handling with `StateT` and `ExceptT`
- **🎨 Functional Style** - Immutable data structures and pure functions
- **📚 Learning** - Great way to understand both interpreters and functional programming

## 🚀 Future Enhancements

- [ ] Complete class inheritance implementation
- [ ] Add more built-in functions (string manipulation, math functions)
- [ ] Implement proper error recovery in the parser
- [ ] Add a debugger interface
- [ ] Performance optimizations
- [ ] Static analysis and type checking
- [ ] Compile to bytecode for better performance

---

**Happy interpreting!** 🎉

For setup instructions and detailed usage, see **[SETUP.md](SETUP.md)**.
