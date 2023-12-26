# Tap
A simple interpreted programming language.

### Syntax
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



