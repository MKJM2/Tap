# Tap
A simple strongly-typed interpretted programming language crafted
in modern C++. Tap's syntax is a fusion of features offered by 
language I love: C++, Haskell, Rust, GoLang, OCaml.

Tap was built as a playground for learning programming language design,
C++17 and C++20 features (including modules, variants), CMake 
project management and unit testing (using GoogleTest).
As such, I implement my own lexer, parser, and interpreter
(no GNU Bison used).

Disclaimer: Tap is in its infancy and many of the features are still not implemented.

### Grammar (EBNF)
```
program         = {statement} .
statement       = [assignment ";"]
                  [ident ":" type-annotation ";"]
                  [expression ";" ] 
                  ["return" expression ";" ] .
assignment      = ident [ ":" type-annotation ] "=" expression { "," ident "=" expression } .
type-annotation = type-ident { "->" type-annotation } | "[" type-annotation "]" .
expression      = term { ("+" | "-") term } .
term            = factor { ("*" | "/") factor } .
factor          = ident | integer | string | list | lambda | function_call | "(" expression ")" .
list            = "[" [ expression { "," expression } ] "]" .
lambda          = "\" ident "." expression .
function_call   = ident "(" arglist ")" .
function_def    = "func" ident "(" typed-arglist ")" "{" {statement} "}" .
arglist         = [ expression { "," expression } ] .
typed-arglist   = [ ident [ ":" type-annotation ] { "," ident [ ":" type-annotation ] } ] .
ident           = letter { letter | digit } .
type-ident      = "int" | "str" | "float" .
integer         = digit {digit} .
string          = '"' { character } '"' .
character       = UTF8 \ '"' .
letter          = ['a'-'z'] | ['A' - 'Z' ] .
```

### Syntax examples
```
# Variable assignment
x = 5;   

# Optional type annotations
y : int;
y = 20;

y : int = 20;  # Type annotations can be inlined

# Lists
z : [str] = ["John", "Smith"];  

# Lambda expressions
f : int -> int = \x. x + 5;   

# Recursive functions
func fib(n: int) : int {
    return fib(n - 1) + fib(n - 2);   
}

# Implicit return statements
add : int -> int -> int;
func add(x, y) {
    x + y;   
}

func sub(x: int, y: int) : int {
    x - y;
}

# Functions are first class citizens
add2 = \x. add(2, x);

func custom_inc(x: int) : int -> int -> int {
    \y. add(x, y);
}

### TODOs

# Structs
car : struct {
    make : str,
    model : str,
    year_produced : int
};

# Matching

# Enums

# For loops

# While loops

# If statements

```
### Dependencies
- CMake
- Ninja
- GoogleTest
- libreadline
- C++20 compatible compiler

### Building
To build the project, from the root directory execute
```bash
mkdir build
cd build
cmake -GNinja ..
ninja
```