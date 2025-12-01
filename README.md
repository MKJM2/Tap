# Tap

An unremarkable but deliberately _Polished_ interpreted programming language.
Originally built as a personal project to learn C++20 (CMake, Gtest, and other
ritual sacrifices), it has since been rewritten. Currently in its infancy.

The short term goal is to solve all Advent of Code problems using Tap. There is
no long term goal. The purpose of this project was and remains education and fun.

## Current state of affairs:
- Rust / C++ inspired syntax
- tree walking interpreter
- rudimentary runtime type checking
- hand rolled lexer & parser
- basic closure & lexical scoping
- **bilingual keywords** (English + Polish aliases for all keywords) 🇵🇱

## but... why?
Because I can. Also it's fun.
Lexing and parsing are already solved problems (see the excellent [logos](https://docs.rs/logos/latest/logos/) crate for creating lexers,
or [nom](https://docs.rs/nom/latest/nom/) creating parsers. People older to the trade are surely familiar with the [GNU Bison](https://www.gnu.org/software/bison/) parser _generator_).


## Planned features
- type inference
- emitting byte code + a VM implementation
- VM
- potentially experimenting into JIT compilation (but nothing too serious, I have a life)
- a faster, state machine based lexer (DFA)
    - might require edits to the Tap grammar
    - perfect hashing for keywords?
    - cache friendly token storage

### Tap syntax examples
[Check out the WIP EBNF Grammar](grammar.ebnf)
```
// Type Declarations

// Sum type with variants
type Option = Some(int) | None;

// Sum type with multiple variants
type Result = Ok(int) | Error(string);

// Record type
type Point = { x: int, y: int };

// Generic type usage
type IntList = [int];


// Variable Bindings

// Immutable variable
x: int = 42;

// Immutable variable with type inference
name = "Alice";

// Mutable variable
mut counter: int = 0;


// Function Definitions

// Function with parameters and return type
add(a: int, b: int): int = {
    a + b
}

// Function with block body and statements
factorial(n: int): int = {
    mut result = 1;
    mut i = 1;
    while (i <= n) {
        result *= i;
        i += 1;
    }
    result
}

// Function using records
distance(p1: Point, p2: Point): float = {
    dx = p1.x - p2.x;
    dy = p1.y - p2.y;
    (dx * dx + dy * dy)
}


// Control Flow

// If expression
max(a: int, b: int): int = {
    if (a > b) {
        a
    } else {
        b
    }
}

// While loop
print_numbers(n: int): int = {
    mut i = 0;
    while (i < n) {
        i += 1;
    }
    i
}

// For loop
for i in [1, 2, 3, 4, 5] {
    print(i);
}

for fruit in fruits {
    print(fruit); //  "apple" "banana" "cherry"
}

// Pattern Matching

// Match expression with patterns
unwrap_or(opt: Option, default: int): int = {
    match (opt) {
        | Some(value) => value,
        | None => default
    }
}

// Match with multiple patterns
handle_result(res: Result): string = {
    match (res) {
        | Ok(val) => "Success",
        | Error(msg) => msg
    }
}


// Lambda Expressions

// Lambda with type annotation
map_fn = (f: int -> int, lst: [int]): [int] => {
    lst
}

// Lambda without type annotation
simple_lambda = (x: int) => x + 1;


// Operators and Expressions

compute(): int = {
    // Arithmetic operators
    a = 10 + 5;
    b = 20 - 3;
    c = 4 * 7;
    d = 15 / 3;

    // Comparison operators
    is_equal = (a == 15);
    is_not_equal = (b != 10);
    is_less = (c < 30);
    is_greater = (d > 2);
    is_less_equal = (a <= 15);
    is_greater_equal = (b >= 17);

    // Logical operators
    both_true = is_equal && is_not_equal;
    either_true = is_less || is_greater;

    // Unary operators
    negated = -a;
    positive = +b;
    not_true = !is_equal;

    // Compound assignment
    mut x = 10;
    x += 5;
    x -= 2;
    x *= 3;
    x /= 4;

    x
}


// Data Structures

// Record literal
origin: Point = { x: 0, y: 0 };

// List literal
numbers: [int] = [1, 2, 3, 4, 5];
fruits: [string] = ["apple", "banana", "cherry"];

// Empty list
empty = [];


// Accessing Data

access_demo(): int = {
    // Field access
    p = { x: 10, y: 20 };
    x_val = p.x;

    // Array indexing
    arr = [1, 2, 3];
    first = arr[0];

    // Function call
    result = add(x_val, first);

    result
}


// Variant Construction

// Creating sum type values
some_value: Option = Some(42);
no_value: Option = None;

ok_result: Result = Ok(100);
error_result: Result = Error("Something went wrong");


// Nested Expressions

complex_expr(): int = {
    if (true && (10 > 5)) {
        match (Some(42)) {
            | Some(x) => {
                y = x + 10;
                y * 2
            },
            | _ => 0
        }
    } else {
        -1
    }
}
```


### Dependencies
- Rust 1.70+
- Cargo

### Building
To build the project, from the root directory
```bash
cargo build --release
cargo run               # REPL
cargo run -- <file>    # Run a script
```

### Testing
Run the test suite:
```bash
cargo test
```
