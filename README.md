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
```ebnf
program         = {statement} .

statement       = assignment ";"
                | var_decl ";"
                | expression ";"
                | return_stmt
                | function_def
                | struct_decl
                | enum_decl
                | if_stmt
                | match_stmt
                | while_loop
                | for_loop .

var_decl        = ident ":" type-annotation .
assignment      = ident [ ":" type-annotation ] "=" expression .
return_stmt     = "return" expression ";" .

struct_decl     = ident ":" struct_type ";" .
enum_decl       = ident ":" enum_type ";" .

if_stmt         = "if" expression block [ "else" block ] .
block           = "{" {statement} "}" .

match_stmt      = "match" expression "{" match_arm { match_arm } "}" .
match_arm       = pattern "=>" expression ";" .
pattern         = ident
                | literal
                | list_pattern
                | struct_pattern
                | variant_pattern .

list_pattern    = "[" [ pattern { "," pattern } ] "]" .
struct_pattern  = "struct" "{" field_pattern { "," field_pattern } "}" .
field_pattern   = ident ":" pattern .
variant_pattern = ident "(" pattern { "," pattern } ")" .

while_loop      = "while" expression block .
for_loop        = "for" ident "in" expression block .

expression      = term { ("+" | "-") term } .
term            = factor { ("*" | "/") factor } .
factor          = literal
                | ident
                | list
                | lambda
                | if_stmt // TODO: Make if_stmt an if_expression instead and remove from expressions?
                | function_call
                | list_access
                | "(" expression ")" .

literal         = integer | float | string | "true" | "false" .

list            = "[" [ expression { "," expression } ] "]" .
lambda          = "\"" ident { ident } "." expression .
function_call   = ident "(" arglist ")" .
list_access     = ident "[" expression "]" .

function_def    = "func" ident "(" typed-arglist ")" [ ":" type-annotation ] "{" {statement} "}" .

arglist         = [ expression { "," expression } ] .
typed-arglist   = [ typed_param { "," typed_param } ] .
typed_param     = ident [ ":" type-annotation ] .

type-annotation = function_type
                | array_type
                | struct_type
                | enum_type
                | type-ident
                | "(" type-annotation ")" .

function_type   = type-ident "->" type-annotation .
array_type      = "[" type-annotation "]" .
struct_type     = "struct" "{" field { "," field } "}" .
enum_type       = "enum" "{" variant { "," variant } "}" .
field           = ident ":" type-annotation .
variant         = ident [ "(" type-annotation { "," type-annotation } ")" ] .

type-ident      = primitive_type | ident .
primitive_type  = "int" | "str" | "float" | "bool" | "unit" .

ident           = letter { letter | digit | "_" } .
letter          = "a"..."z" | "A"..."Z" .
digit           = "0"..."9" .

integer         = digit {digit} .
float           = digit {digit} "." digit {digit} [ exponent ] .
exponent        = ("e" | "E") ["+" | "-"] digit {digit} .

string          = '"' { char } '"' .
char            = ? any character except ", \, and newline ? | escape_seq .
escape_seq      = "\" ( '"' | '\\' | "n" | "t" | "r" ) .

comment         = "#" { ? any character except newline ? } "\n" .
whitespace      = " " | "\t" | "\n" | "\r" .
```

### Lexer Precedence
- Keywords (func, return, struct, etc.) > Identifiers
- Reserved words cannot be redefined

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
    if n < 2 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

# Structs
car : struct {
    make : str,
    model : str,
    year_produced : int
};

# Enums
opt : enum { Some(int), None };
color : enum { Red, Green, Blue };

# Match statements
match opt {
    Some(x) => x + 1,
    None => 0
};

# If statements
if x > 0 {
    x = x - 1;
} else {
    x = 0;
}

# While loops
while x > 0 {
    x = x - 1;
}

# For loops
for i in [1, 2, 3] {
    # do something
}
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
