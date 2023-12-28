# Tap
A simple interpreted programming language.

### Grammar (EBNF)
```
program         = {statement} .
statement       = [assignment ";"]
                  [ident ":" type-annotation ";"]
                  [function_call ";" ]
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
function_def    = "func" ident "(" typed-arglist ")" "{" statement "}" .
arglist         = [ expression { "," expression } ] .
typed-arglist   = [ expression [ ":" type-annotation ] { "," expression [ ":" type-annotation ] } ] .
ident           = letter { letter | digit } .
type-ident      = "int" | "str" | "float" .
integer         = digit {digit} .
string          = '"' { character } '"' .
character       = UTF8 \ '"' .
letter          = ['a'-'z'] | ['A' - 'Z' ] .
```

### Example Syntax
```
# Variable assignment
x = 5;   

# Optional type annotations
y : int;
y = 20;

y : int = 20;  # Type signatures can be inlined

# Lists
z : [str] = ["John", "Smith"];  
# Lambda expressions
f : int -> int = \x. x + 5;   

# Recursive functions
func fib(n: int) : int -> int {
    return fib(n - 1) + fib(n - 2);   
}

# Implicit return statements
add : int -> int -> int;
func add(x, y) {
    x + y;   
}

func sub(x: int, y: int) -> int {
    x - y;
}

add2 = \x. add(2, x);

# Functions are first class citizens
func custom_add_x(x: int) : int -> int -> int {
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

```
### Dependencies
- CMake
- Ninja
- libreadline
- C++20 compatible compiler

