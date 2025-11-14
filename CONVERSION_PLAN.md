# C++ to Rust Conversion Plan: Tap Interpreter

This document outlines a plan to convert the Tap interpreter from C++ to idiomatic Rust. The goal is to create a more robust, safe, and maintainable codebase by leveraging Rust's strengths, such as its powerful type system, memory safety guarantees, and modern tooling.

## 1. Project Setup and Structure

The current C++ project uses CMake for building. We will replace this with **Cargo**, Rust's official build tool and package manager.

### Initial Setup

1.  Create a new Rust project using Cargo:
    ```bash
    cargo new tap --bin
    cd tap
    ```
2.  This will generate the following idiomatic Rust project structure:
    ```
    tap/
    ├── .git
    ├── .gitignore
    ├── Cargo.toml
    └── src/
        └── main.rs
    ```

### Proposed Module Structure

The C++ code is organized using C++20 modules (`lexer.cppm`, `parser.cppm`, etc.). In Rust, we will use Rust's module system within the `src` directory.

```
src/
├── main.rs           # Executable entry point, REPL, file runner
├── lexer.rs            # Lexer and Token definitions
├── ast.rs              # AST (Abstract Syntax Tree) nodes
├── parser.rs           # Parser logic
├── interpreter.rs      # Interpreter and runtime values
└── environment.rs      # Environment/scope management
```

## 2. Crate Organization & Dependencies

The project is small enough to be a single binary crate (`tap`). We will manage all dependencies through `Cargo.toml`.

### Dependency Mapping

| C++ Dependency | Rust Equivalent(s) | Notes |
| :--- | :--- | :--- |
| `gtest` | Rust's built-in test framework (`#[test]`) | No external dependency needed. Tests can be placed in each module or in an `tests/` directory for integration tests. |
| `readline` | `rustyline` | A popular Rust crate for REPL functionality. |
| C++ Standard Library (`<vector>`, `<string>`, `<variant>`, `<unordered_map>`) | Rust Standard Library (`Vec`, `String`, `enum`, `HashMap`) | Rust's standard library provides excellent, memory-safe alternatives. |

### Initial `Cargo.toml`

```toml
[package]
name = "tap"
version = "0.1.0"
edition = "2021"

[dependencies]
rustyline = "10.0"
```

## 3. Core Component Conversion Strategy

### 3.1. AST (`ast.cppm`)

The C++ AST uses inheritance and raw pointers (`ASTNode*`), which is not idiomatic in Rust and is prone to memory leaks.

**Rust Approach:**

-   Use Rust's `enum`s to define the AST nodes. This is a perfect use case for Rust's sum types.
-   Use `Box<T>` for recursive types (e.g., an expression containing another expression). This replaces raw pointers with smart pointers that guarantee memory safety.

**Example `ast.rs`:**

```rust
// src/ast.rs

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Assignment(String, Expression),
    // ... other statement types
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralValue),
    Identifier(String),
    BinaryOp(Box<Expression>, Operator, Box<Expression>),
    // ... other expression types
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Integer(i64),
    String(String),
    // ...
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}
```

### 3.2. Lexer (`lexer.cppm`)

The C++ lexer is a hand-rolled implementation. We can port this logic to Rust.

**Rust Approach:**

-   Create a `Lexer` struct that holds the source string and the current position.
-   Implement the `Iterator` trait for the `Lexer`. This is the most idiomatic way to handle token streams in Rust. `next()` will return `Option<Result<Token, LexerError>>`.
-   The `Token` struct and `TokenType` enum will be defined similarly to the C++ version.


### 3.3. Parser (`parser.cppm`)

The C++ parser is a recursive descent parser. This pattern is common in Rust and can be ported directly.

**Rust Approach:**

-   Create a `Parser` struct that holds the stream of tokens from the lexer.
-   Implement methods for each grammar rule (e.g., `parse_statement()`, `parse_expression()`).
-   Use Rust's `Result<T, E>` for error handling. A `ParseError` enum will be created to represent different parsing failures. This is a major improvement over throwing runtime exceptions.

**Example `parser.rs`:**

```rust
// src/parser.rs

use crate::ast::{Expression, Statement};
use crate::lexer::{Token, TokenType};

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        // ... parsing logic ...
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        // ... recursive descent logic ...
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { expected: TokenType, found: TokenType },
    // ... other error types
}
```

### 3.4. Interpreter (`interpreter.cppm`)

The tree-walking interpreter can be ported with significant improvements in safety and clarity.

**Rust Approach:**

-   The `Value` variant from C++ will become a Rust `enum`.
-   The `Environment` will use a `HashMap<String, Value>`.
-   The `evaluate` function will take a reference to an AST node and the environment, and return a `Result<Value, RuntimeError>`.
-   Pattern matching (`match` expressions) will be used extensively to handle different AST node types and values, making the code cleaner and safer than C++ `switch` statements.

**Example `interpreter.rs`:**

```rust
// src/interpreter.rs

use crate::ast::{Expression, Statement};
use crate::environment::Environment;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    String(String),
    Function(...),
    Null,
}

pub fn evaluate(statement: &Statement, env: &mut Environment) -> Result<Value, RuntimeError> {
    match statement {
        Statement::Expression(expr) => evaluate_expression(expr, env),
        // ... other cases
    }
}

fn evaluate_expression(expr: &Expression, env: &mut Environment) -> Result<Value, RuntimeError> {
    match expr {
        Expression::Literal(val) => Ok(Value::from(val)),
        Expression::BinaryOp(left, op, right) => {
            let left_val = evaluate_expression(left, env)?;
            let right_val = evaluate_expression(right, env)?;
            // ... apply operator
        }
        // ... other cases
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeError(String),
}
```

### 3.5. Missing Features

Based on a comparison between the C++ and Rust codebases, the following features are missing or incomplete in the Rust implementation:

*   **Lambdas:** While the AST in Rust has a variant for lambdas, the parser does not yet support parsing them.
*   **Struct/Enum Declarations:** The C++ parser has `TODO` markers for struct and enum declarations, and this functionality is also missing from the Rust implementation.
*   **Full Interpreter Implementation:** The Rust interpreter is still under development. The following features need to be fully implemented:
    *   Lists
    *   Function type annotations (`int -> int`)
    *   Compound assignments (`+=`)

## 4. Testing

Rust has a fantastic built-in testing framework. We will convert the GoogleTest tests to Rust tests.

**Rust Approach:**

-   For each module (`lexer.rs`, `parser.rs`), we will add a `#[cfg(test)]` section with unit tests.
-   Integration tests that parse and interpret whole files can be placed in the `tests/` directory at the root of the project.

**Example Test in `parser.rs`:**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_simple_assignment() {
        let source = "x = 5;";
        let tokens = Lexer::new(source).collect::<Result<Vec<_>, _>>().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse().unwrap();
        // Assertions about the generated AST
        assert_eq!(program.len(), 1);
        // ... more detailed assertions
    }
}
```

## 4. Testing

Rust has a fantastic built-in testing framework. We will convert the GoogleTest tests to Rust tests.

### 4.1. Missing Tests

Based on the analysis of the C++ and Rust codebases, the following tests are missing in the Rust implementation:

*   **Parser tests:** Similar to the C++ `parser_test.cpp`, we need tests to verify the correctness of the parser's output. This could be implemented as snapshot tests that compare the generated AST with a reference output.
*   **Interpreter tests:** We need tests to verify the correctness of the interpreter's output for various language features. This includes tests for:
    *   Variable assignments and lookups.
    *   Function calls.
    *   Control flow (if/else statements, loops, etc.).
    *   Error conditions (e.g., undefined variables, type mismatches).
*   **End-to-end tests:** Tests that run a Tap program from source code and verify the final output. These tests would cover the entire pipeline from lexing to interpretation.

## 5. Step-by-Step Conversion Plan

This phased approach allows for incremental development and testing.

**Phase 1: Project Setup & Lexer**
1.  Set up the Cargo project.
2.  Add `rustyline` dependency.
3.  Implement `lexer.rs` with the `Token` and `TokenType` definitions.
4.  Write unit tests for the lexer to ensure it correctly tokenizes Tap source code.

**Phase 2: AST and Parser**
1.  Define the AST enums in `ast.rs`.
2.  Implement the `Parser` in `parser.rs`.
3.  Start with simple expressions and statements, and incrementally add more complex grammar rules.
4.  Write extensive unit tests for the parser, covering all grammar rules and error conditions. The existing C++ tests are a great source of test cases.
5.  Implement parsing for lambdas, struct and enum declarations.

**Phase 3: Interpreter**
1.  Implement `interpreter.rs` and `environment.rs`.
2.  Define the `Value` enum.
3.  Implement the `evaluate` function, starting with simple expressions (literals, binary operations).
4.  Add support for variables, assignments, and scopes (environments).
5.  Implement function definitions and calls.
6.  Implement lists, function type annotations, and compound assignments.
7.  Write comprehensive interpreter tests for all supported features.

**Phase 4: Executable & REPL**
1.  Flesh out `main.rs`.
2.  Add logic to read from a file or start a REPL.
3.  Use `rustyline` to create an interactive REPL experience.
4.  Connect all the components: the input is passed to the lexer, then the parser, then the interpreter, and the result is printed.

**Phase 5: Finalizing and Refactoring**
1.  Review the entire codebase for idiomatic Rust practices.
2.  Improve error reporting with more detailed error messages.
3.  Add documentation with `cargo doc`.
4.  Ensure all C++ functionality that was in scope has been ported.
5.  Write end-to-end tests that run Tap programs and verify their output.
