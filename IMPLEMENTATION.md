When running `cargo t`, several tests are failing.
I want you to systemically fix them one by one. For each test that you've attempted a fix for,
create a separate Markdown file explaining the issue and how you fixed it.
DO NOT Change the tests themselves. When you think a test is wrong, CONFIRM with the user first and foremost.

Current state of `cargo t`:
```

running 37 tests
test test_array_access_expression ... ok
test test_array_mutation ... ok
test test_fib ... ok
test test_enum_decl ... ok
test test_dodaj_polish ... ok
test test_conditional_as_value ... FAILED
test test_complex_boolean_logic ... FAILED
test test_early_return_in_loop ... FAILED
test test_float_arithmetic ... ok
test test_fib_lambda ... ok
test test_float_comparison ... ok
test test_function_returning_lambda ... FAILED
test test_hello ... ok
test test_hi_func ... ok
test test_inline_lambda ... ok
test test_lambda_declaration ... ok
test test_iterative_fib ... FAILED
test test_lambdas2 ... FAILED
test test_list_sum ... FAILED
test test_logic_short_circuit_and ... FAILED
test test_logic_short_circuit_or ... FAILED
test test_modulo ... ok
test test_match ... FAILED
test test_mutual_recursion ... FAILED
test test_nested_loops_continue ... FAILED
test test_precedence_order ... ok
test test_option_type ... FAILED
test test_nested_loops_break ... FAILED
test test_nested_if_expression ... FAILED
test test_silnia_polish ... ok
test test_struct_access_nested ... FAILED
test test_string_concatenation ... FAILED
test test_structs ... FAILED
test test_unit_return ... FAILED
test test_unary_operators ... FAILED
test test_polish_if_else ... FAILED
test test_polish_while_loop has been running for over 60 seconds
^C
```
I have disabled the test that was timing out and here is the test results:
```
failures:

---- test_early_return_in_loop stdout ----

thread 'test_early_return_in_loop' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse function definition
   1: body of 'find_match'
   2: Unexpected token in block: expected '{', but found '->' at line 2

Location:
    src/parser.rs:734:13

---- test_conditional_as_value stdout ----

thread 'test_conditional_as_value' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse declaration statement
   1: Failed to parse expression or assignment statement
   2: Unexpected token in expression statement: expected ';', but found '}' at line 7

Location:
    src/parser.rs:734:13
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

---- test_complex_boolean_logic stdout ----

thread 'test_complex_boolean_logic' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in group: expected ')', but found '||' at line 7

Location:
    src/parser.rs:734:13

---- test_function_returning_lambda stdout ----

thread 'test_function_returning_lambda' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse function definition
   1: body of 'make_adder'
   2: Unexpected token in block: expected '{', but found '->' at line 2

Location:
    src/parser.rs:734:13

---- test_lambdas2 stdout ----

thread 'test_lambdas2' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse function definition
   1: body of 'transform'
   2: Unexpected token in block: expected '{', but found '->' at line 3

Location:
    src/parser.rs:734:13

---- test_list_sum stdout ----

thread 'test_list_sum' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse for loop
   1: Unexpected token in field: expected ':', but found '=' at line 6

Location:
    src/parser.rs:734:13

---- test_iterative_fib stdout ----

thread 'test_iterative_fib' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse function definition
   1: body of 'fib_iter'
   2: Unexpected token in block: expected '{', but found '->' at line 2

Location:
    src/parser.rs:734:13

---- test_logic_short_circuit_and stdout ----

thread 'test_logic_short_circuit_and' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found '&&' at line 2

Location:
    src/parser.rs:734:13

---- test_logic_short_circuit_or stdout ----

thread 'test_logic_short_circuit_or' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found '||' at line 2

Location:
    src/parser.rs:734:13

---- test_mutual_recursion stdout ----

thread 'test_mutual_recursion' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse function definition
   1: body of 'is_even'
   2: Unexpected token in block: expected '{', but found '->' at line 2

Location:
    src/parser.rs:734:13

---- test_match stdout ----

thread 'test_match' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found 'Light' at line 2

Location:
    src/parser.rs:734:13

---- test_nested_loops_break stdout ----

thread 'test_nested_loops_break' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse while loop
   1: Failed to parse while loop
   2: Failed to parse if-statement
   3: Failed to parse expression or assignment statement
   4: Unexpected token in expression: expected literal, identifier, or '(', but found 'break' at line 10

Location:
    src/parser.rs:734:13

---- test_option_type stdout ----

thread 'test_option_type' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found 'MaybeInt' at line 2

Location:
    src/parser.rs:734:13

---- test_polish_if_else stdout ----

thread 'test_polish_if_else' panicked at tests/integration.rs:524:5:
assertion `left == right` failed
  left: Some(Integer(0))
 right: Some(Integer(2))

---- test_nested_if_expression stdout ----

thread 'test_nested_if_expression' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse declaration statement
   1: Failed to parse if-statement
   2: Failed to parse expression or assignment statement
   3: Unexpected token in expression statement: expected ';', but found '}' at line 8

Location:
    src/parser.rs:734:13

---- test_nested_loops_continue stdout ----

thread 'test_nested_loops_continue' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse while loop
   1: Failed to parse if-statement
   2: Failed to parse expression or assignment statement
   3: Unexpected token in expression: expected literal, identifier, or '(', but found 'continue' at line 7

Location:
    src/parser.rs:734:13

---- test_string_concatenation stdout ----

thread 'test_string_concatenation' panicked at tests/integration.rs:18:42:
Runtime error: TypeError("Type mismatch in binary op")

---- test_unit_return stdout ----

thread 'test_unit_return' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression: expected literal, identifier, or '(', but found '{' at line 2

Location:
    src/parser.rs:734:13

---- test_struct_access_nested stdout ----

thread 'test_struct_access_nested' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found 'Point' at line 2

Location:
    src/parser.rs:734:13

---- test_structs stdout ----

thread 'test_structs' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found 'Rect' at line 2

Location:
    src/parser.rs:734:13

---- test_unary_operators stdout ----

thread 'test_unary_operators' panicked at tests/integration.rs:13:42:
Parser error: Failed to parse top-level statement

Caused by:
   0: Failed to parse expression or assignment statement
   1: Unexpected token in expression statement: expected ';', but found '&&' at line 4

Location:
    src/parser.rs:734:13
```
