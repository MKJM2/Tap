use tap::diagnostics::Reporter;
use tap::interpreter::{Interpreter, RuntimeError, Value};
use tap::lexer::Lexer;
use tap::parser::Parser;
use tap::utils::pretty_print_tokens;
// We don't import `ast::Span` directly into the test file
// because it's only used internally by the parser.

// Helper function to interpret a source string and return the result
fn interpret_source(source: &str) -> Result<Option<Value>, RuntimeError> {
    let mut reporter = Reporter::new();
    let tokens = Lexer::new(source, &mut reporter)
        .tokenize()
        .expect("Lexing failed");

    // Check for lexer errors first
    if reporter.has_errors() {
        panic!(
            "Lexing failed with reporter errors for source:\n\"{}\"\nTokens: {}\nLexer Errors: {:?}",
            source,
            pretty_print_tokens(&tokens),
            reporter.diagnostics
        );
    }

    let mut parser = Parser::new(&tokens, &mut reporter);
    let program = parser.parse_program();

    // Check for parser errors
    if reporter.has_errors() {
        panic!(
            "Parsing failed with reporter errors for source:\n\"{}\"\nTokens: {}\nParser Errors: {:?}",
            source,
            pretty_print_tokens(&tokens),
            reporter.diagnostics
        );
    }

    let program = program.expect("Parser failed unexpectedly but no errors reported.");
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&program)
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;

    // --- Small Snippets (Core Functionality) ---

    #[test]
    fn test_interpret_integer_literal() {
        assert_eq!(interpret_source("123;"), Ok(Some(Value::Integer(123))));
    }

    #[test]
    fn test_interpret_float_literal() {
        assert_eq!(interpret_source("3.14;"), Ok(Some(Value::Float(3.14))));
    }

    #[test]
    fn test_interpret_string_literal() {
        assert_eq!(
            interpret_source("\"hello\";"),
            Ok(Some(Value::String("hello".to_string())))
        );
    }

    #[test]
    fn test_interpret_boolean_literal_true() {
        assert_eq!(interpret_source("true;"), Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_boolean_literal_false() {
        assert_eq!(interpret_source("false;"), Ok(Some(Value::Boolean(false))));
    }

    #[test]
    fn test_interpret_none_literal() {
        assert_eq!(interpret_source("None;"), Ok(Some(Value::Unit)));
    }

    #[test]
    fn test_interpret_integer_addition() {
        assert_eq!(interpret_source("1 + 2;"), Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_interpret_float_multiplication() {
        assert_eq!(interpret_source("2.5 * 2.0;"), Ok(Some(Value::Float(5.0))));
    }

    #[test]
    fn test_interpret_integer_division() {
        assert_eq!(interpret_source("10 / 2;"), Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_division_by_zero() {
        assert_eq!(
            interpret_source("10 / 0;"),
            Err(RuntimeError::DivisionByZero)
        );
    }

    #[test]
    fn test_interpret_unary_minus_integer() {
        assert_eq!(interpret_source("-5;"), Ok(Some(Value::Integer(-5))));
    }

    #[test]
    fn test_interpret_unary_not_boolean() {
        assert_eq!(interpret_source("!true;"), Ok(Some(Value::Boolean(false))));
        assert_eq!(interpret_source("!false;"), Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_unary_not_truthiness() {
        // According to interpreter's is_truthy: non-Boolean, non-Unit are truthy
        assert_eq!(interpret_source("!10;"), Ok(Some(Value::Boolean(false))));
        assert_eq!(
            interpret_source("!\"hello\";"),
            Ok(Some(Value::Boolean(false)))
        );
        assert_eq!(interpret_source("!None;"), Ok(Some(Value::Boolean(true)))); // Unit is not truthy
    }

    #[test]
    fn test_interpret_variable_declaration_no_type() {
        // Corresponds to `name = "Alice";`
        let source = "x = 10; x;";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_variable_declaration_with_type() {
        // Corresponds to `x: int = 42;`
        let source = "x: int = 10; x;";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_mutable_variable_declaration() {
        // `mut counter: int = 0;` - `mut` is parsed but not used for re-assignment logic yet
        // Only checks the initial binding
        let source = "mut x: int = 10; x;";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_variable_in_expression() {
        let source = "x = 5; x + 3;";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(8))));
    }

    #[test]
    fn test_interpret_multiple_variable_declarations() {
        let source = "a = 1; b = 2; a * b;";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_interpret_undefined_variable() {
        let source = "x;";
        assert_eq!(
            interpret_source(source),
            Err(RuntimeError::TypeError("Undefined variable: x".to_string()))
        );
    }

    #[test]
    fn test_interpret_binary_comparison_equal_error() {
        // Current interpreter's apply_binary_op does not implement comparison ops for integers/floats
        assert_eq!(
            interpret_source("1 == 1;"),
            Err(RuntimeError::TypeError("Invalid integer operator".into()))
        );
    }

    #[test]
    fn test_interpret_binary_logical_and_error() {
        // Current interpreter's apply_binary_op does not implement logical ops for integers/floats
        assert_eq!(
            interpret_source("1 && 0;"),
            Err(RuntimeError::TypeError(
                "Type mismatch in binary operation".into()
            )) // This hits the `_ => Err(TypeError)` for (Integer, Integer) when op is &&.
               // If it were (Bool, Bool), it would hit the type mismatch.
        );
    }

    #[test]
    fn test_interpret_mixed_types_arithmetic_error() {
        let source = "10 + 3.5;";
        assert_eq!(
            interpret_source(source),
            Err(RuntimeError::TypeError(
                "Type mismatch in binary operation".to_string()
            ))
        );
    }

    #[test]
    fn test_interpret_parenthesized_expression() {
        assert_eq!(
            interpret_source("(2 + 3) * 4;"),
            Ok(Some(Value::Integer(20)))
        );
    }

    // --- Tests for features causing `unimplemented!()` or specific errors ---

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_type_declaration_panics() {
        // `type Option = Some(int) | None;` will parse, but `eval_top_statement` will panic.
        interpret_source("type Option = Some(int) | None;").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_function_definition_panics() {
        // `add(a: int, b: int): int = { a + b }` will parse, but `eval_let_statement` will panic.
        interpret_source("add(a: int, b: int): int = { a + b };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_if_expression_panics() {
        // `if (true) { 1 } else { 0 }` will parse, but `eval_expr` for `Expression::If` will panic.
        interpret_source("if (true) { 1 } else { 0 };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_while_expression_panics() {
        // `while (true) { 1; }` will parse, but `eval_expr` for `Expression::While` will panic.
        interpret_source("while (true) { 1; };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_for_expression_panics() {
        // `for i in [1, 2] { 1; }` will parse, but `eval_expr` for `Expression::For` will panic.
        interpret_source("for i in [1, 2] { 1; };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_match_expression_panics() {
        // `match (1) { | _ => 1 }` will parse, but `eval_expr` for `Expression::Match` will panic.
        interpret_source("match (1) { | _ => 1 };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_lambda_expression_panics() {
        // `(x: int) => x + 1;` will parse, but `eval_expr` for `Expression::Lambda` will panic.
        interpret_source("f = (x: int) => x + 1;").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_record_literal_panics() {
        // `{ x: 1, y: 2 }` will parse, but `eval_expr` for `PrimaryExpression::Record` will panic.
        interpret_source("p = { x: 1, y: 2 };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_list_literal_panics() {
        // `[1, 2, 3]` will parse, but `eval_expr` for `PrimaryExpression::List` will panic.
        interpret_source("numbers = [1, 2, 3];").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_field_access_panics() {
        // `{ x: 10, y: 20 }.x` will parse, but `eval_expr` for `Expression::Postfix` will panic.
        interpret_source("p = { x: 10, y: 20 }; p.x;").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_list_access_panics() {
        // `[1, 2, 3][0]` will parse, but `eval_expr` for `Expression::Postfix` will panic.
        interpret_source("arr = [1, 2, 3]; arr[0];").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_function_call_panics() {
        // `my_func()` will parse as Postfix::Call, but `eval_expr` for `Expression::Postfix` will panic.
        // This implicitly tests if a function binding exists, but the call itself isn't supported.
        interpret_source("my_func(): int = { 0 }; my_func();").unwrap();
    }

    #[test]
    fn test_interpret_compound_assignment_error() {
        // `x += 5;` will parse as a binary expression (AddAssign),
        // but `apply_binary_op` does not handle `AddAssign`.
        let source = "mut x = 10; x += 5;";
        assert_eq!(
            interpret_source(source),
            Err(RuntimeError::TypeError("Invalid integer operator".into()))
        );
    }

    // --- More Full-Fledged Examples (Combinations leading to expected errors/panics) ---

    #[test]
    fn test_interpret_complex_arithmetic_with_variables() {
        let source = "
            x = 10;
            y = 5;
            // The result of `(x + y) * 2 / 3` is `(15) * 2 / 3 = 30 / 3 = 10`
            result = (x + y) * 2 / 3;
            result;
        ";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(10))));
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_complex_nested_expression_panics() {
        // This combines `if` and `match`, which are both unimplemented at runtime.
        let source = "
            complex_expr(): int = {
                if (true) {
                    match (None) {
                        | None => { 1 }
                    }
                } else {
                    -1
                }
            };
            complex_expr();
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    fn test_interpret_multiple_statements() {
        let source = "
            val1 = 1;
            val2 = 2;
            val3 = val1 + val2;
            val3;
        ";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_interpret_complex_precedence() {
        // 1 + 2 * 3 - 4 / 2 = 1 + 6 - 2 = 7 - 2 = 5
        assert_eq!(
            interpret_source("1 + 2 * 3 - 4 / 2;"),
            Ok(Some(Value::Integer(5)))
        );
    }

    #[test]
    fn test_interpret_nested_parentheses() {
        // ( (1 + 1) * 2 ) - 3 = ( 2 * 2 ) - 3 = 4 - 3 = 1
        assert_eq!(
            interpret_source("((1 + 1) * 2) - 3;"),
            Ok(Some(Value::Integer(1)))
        );
    }

    #[test]
    fn test_interpret_unary_minus_in_expression() {
        assert_eq!(interpret_source("10 + (-5);"), Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_unary_plus() {
        assert_eq!(interpret_source("+10;"), Ok(Some(Value::Integer(10))));
        assert_eq!(interpret_source("+(2.5);"), Ok(Some(Value::Float(2.5))));
    }

    #[test]
    fn test_interpret_chain_variable_assignment_then_access() {
        let source = "
            a = 5;
            b = a;
            b;
        ";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_variable_shadowing_not_supported_yet() {
        // Current interpreter's `define` re-assigns if name exists.
        // This tests that behavior, not true shadowing.
        let source = "
            x = 10;
            x = 20; // This should re-assign, not shadow
            x;
        ";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(20))));
    }

    // --- Additional Tests for Features Causing `unimplemented!()` or specific errors ---

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_record_type_declaration_panics() {
        // `type Point = { x: int, y: int };`
        interpret_source("type Point = { x: int, y: int };").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_generic_type_declaration_panics() {
        // `type IntList = [int];`
        interpret_source("type IntList = [int];").unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_if_else_if_expression_panics() {
        let source = "
            val = if (false) { 1 } else if (true) { 2 } else { 3 };
            val;
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_while_loop_with_block_panics() {
        let source = "
            mut i = 0;
            while (i < 2) {
                i += 1;
            };
            i;
        ";
        // The panic will likely come from `Expression::While` within `eval_expr`
        // or if `i += 1` is also not fully handled, it might be an earlier error.
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_for_loop_identifier_pattern_panics() {
        let source = "
            for i in [1, 2, 3] {
                // print(i); // Assuming print is a placeholder, still panics on `for`
                i;
            };
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_for_loop_wildcard_pattern_panics() {
        let source = "
            for _ in [1, 2] {
                1;
            };
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_match_expression_with_variant_panics() {
        let source = "
            type Option = Some(int) | None;
            opt_val: Option = Some(10);
            match (opt_val) {
                | Some(x) => x,
                | None => 0
            };
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_match_expression_with_multiple_arms_panics() {
        let source = "
            type Result = Ok(string) | Error(int);
            res_val: Result = Ok(\"success\");
            match (res_val) {
                | Ok(s) => s,
                | Error(e) => \"failure\"
            };
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_lambda_assignment_panics() {
        let source = "
            my_lambda = (x: int) => x * 2;
            // my_lambda(5); // This would also panic
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_record_literal_and_access_panics() {
        let source = "
            p = { x: 10, y: 20 };
            p.x;
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    fn test_method_call_on_record_member() {
        let source = "
            type Circle = {
                radius: float,
                area(this): float = { 3.14 * this.radius * this.radius },
            }
            mut c: Circle = { radius: 5.0 };
            c.radius = 10.0;
            c.area();
        ";
        let res = interpret_source(source);
        assert_eq!(res, Ok(Some(Value::Float(314.0))));
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_list_literal_and_access_panics() {
        let source = "
            arr = [1, 2, 3];
            arr[0];
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    fn test_interpret_compound_assignment_add_error() {
        let source = "mut x = 10; x += 5; x;"; // += not handled by apply_binary_op
        assert_eq!(
            interpret_source(source),
            Err(RuntimeError::TypeError("Invalid integer operator".into()))
        );
    }

    #[test]
    fn test_interpret_compound_assignment_subtract_error() {
        let source = "mut x = 10; x -= 5; x;"; // -= not handled by apply_binary_op
        assert_eq!(
            interpret_source(source),
            Err(RuntimeError::TypeError("Invalid integer operator".into()))
        );
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_factorial_function_panics() {
        // Full function with `while` and compound assignments, all unimplemented
        let source = "
            factorial(n: int): int = {
                mut result = 1;
                mut i = 1;
                while (i <= n) {
                    result *= i;
                    i += 1;
                }
                result
            };
            factorial(5);
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_access_demo_panics() {
        // Combines record, list, function calls - all unimplemented
        let source = "
            add(a: int, b: int): int = { a + b }
            access_demo() : int = {
                p = { x: 10, y : 20 };
                x_val = p.x;
                arr = [1, 2, 3];
                first = arr[0];
                result = add(x_val, first);
                result
            };
            access_demo();
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_variant_construction_panics() {
        // `Some(42)` and `None` are handled by type_constructor, but the AST for Expression::Variant isn't handled by interpreter
        let source = "
            type Option = Some(int) | None;
            some_value: Option = Some(42);
            no_value: Option = None;
        ";
        interpret_source(source).unwrap(); // This should panic when trying to eval the 'Some(42)' or 'None' expression
    }

    #[test]
    #[should_panic(expected = "unimplemented!")]
    fn test_interpret_block_as_final_expression_in_function_panics() {
        // The block itself will parse, but `Expression::Block` is unimplemented in `eval_expr`
        let source = "
            my_func(): int = {
                {
                    a = 1;
                    a + 1
                }
            };
            my_func();
        ";
        interpret_source(source).unwrap();
    }

    #[test]
    fn test_interpret_arithmetic_with_multiple_variables() {
        let source = "
            v1 = 10;
            v2 = 5;
            v3 = 2;
            // 10 + 5 * 2 = 10 + 10 = 20
            result = v1 + v2 * v3;
            result;
        ";
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(20))));
    }

    #[test]
    fn test_interpret_boolean_expressions_with_variables_error() {
        let source = "
            is_valid = true;
            count = 10;
            // The `&&` operator on a bool and an int will cause a type mismatch error
            check = is_valid && count;
            check;
        ";
        assert_eq!(
            interpret_source(source),
            Err(RuntimeError::TypeError(
                "Type mismatch in binary operation".to_string()
            ))
        );
    }

    #[test]
    fn test_interpret_dfs_example() {
        let dfs_source = r#"
            // Recursive helper function for DFS.
            // graph_adj: Adjacency list (e.g., `[[1, 2], [0, 3], [0], [1]]`)
            // u: The current node being visited
            // visited_status: A mutable list of booleans indicating if a node has been visited
            // num_nodes: Total number of nodes in the graph
            dfs_visit_and_count(graph_adj: [[int]], u: int, visited_status: [bool], num_nodes: int): int = {
                // Bounds check for the current node.
                if (u < 0 || u >= num_nodes) {
                    return 0; // Invalid node, no new nodes visited.
                };

                // If the node has already been visited, return 0 as no new nodes were added.
                if (visited_status[u]) {
                    return 0; // Already visited.
                };

                // Mark the current node as visited.
                visited_status[u] = true;

                // Start counting with the current node itself.
                mut count = 1;

                // Iterate over neighbors of the current node.
                for v in graph_adj[u] {
                    // Recursively call DFS for each unvisited neighbor.
                    // Sum up the counts of newly visited nodes from each branch.
                    count = count + dfs_visit_and_count(graph_adj, v, visited_status, num_nodes);
                };

                // Return the total count of nodes visited starting from 'u' in this branch.
                count // Implicit return (final expression of the block)
            };

            // Main DFS entry point function.
            // Initializes the visited array and calls the recursive helper.
            run_dfs(graph: [[int]], start_node: int, graph_size: int): int = {
                // Initialize a mutable boolean list to track visited nodes.
                mut visited_nodes: [bool] = [false, false, false, false]; // Assuming graph_size 4 for this example

                // Call the recursive DFS helper to perform the traversal.
                // This is a function call expression.
                dfs_visit_and_count(graph, start_node, visited_nodes, graph_size)
            };

            // --- Example Graph Definition ---
            // This is an adjacency list representation of a graph with 4 nodes (0-indexed).
            // Example graph structure:
            // 0 --- 1
            // |     |
            // 2     3
            //
            // Adjacency list:
            // Node 0: [1, 2]
            // Node 1: [0, 3]
            // Node 2: [0]
            // Node 3: [1]
            // All nodes are connected if starting from node 0.
            example_graph = [[1, 2], [0, 3], [0], [1]];
            num_example_nodes = 4;

            // --- Run the DFS and Get Result ---
            // Starting DFS from node 0, all 4 nodes should be reachable in this graph.
            // This is the final expression, calling the `run_dfs` function.
            run_dfs(example_graph, 0, num_example_nodes);
        "#;

        assert_eq!(interpret_source(dfs_source), Ok(Some(Value::Integer(4))));
    }

    #[test]
    fn test_interpret_recursive_fibonacci() {
        let fib_source = r#"
            // Calculates the nth Fibonacci number recursively.
            // Base cases: fib(0) = 0, fib(1) = 1.
            fib(n: int): int = {
                if (n <= 1) {
                    n
                } else {
                    fib(n - 1) + fib(n - 2)
                }
            }

            // Calculate the 6th Fibonacci number (0, 1, 1, 2, 3, 5, 8).
            fib(6);
        "#;

        assert_eq!(interpret_source(fib_source), Ok(Some(Value::Integer(8))));
    }

    #[test]
    fn test_interpret_iterative_binary_search() {
        let binary_search_source = r#"
            // Searches for a target value in a sorted list using binary search.
            // Returns the 0-indexed position if found, otherwise -1.
            binary_search(list: [int], target: int): int = {
                mut low = 0;
                mut high = list.length() - 1; // Runtime provided length() method for list size.

                while (low <= high) {
                    mut mid = low + (high - low) / 2;

                    if (list[mid] == target) {
                        return mid; // Target found
                    } else if (list[mid] < target) {
                        low = mid + 1; // Search in the right half
                    } else {
                        high = mid - 1; // Search in the left half
                    }
                };

                return -1; // Target not found
            };

            sorted_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            target_value = 7;
            // Expected index is 6 (0-indexed).
            binary_search(sorted_list, target_value);
        "#;

        // When fully implemented, this should return Value::Integer(6).
        assert_eq!(
            interpret_source(binary_search_source),
            Ok(Some(Value::Integer(6)))
        );
    }

    #[test]
    fn test_interpret_bubble_sort_panics() {
        let bubble_sort_source = r#"
            // Sorts a list of integers in ascending order using bubble sort (in-place)
            bubble_sort(list_to_sort: [int]) = {
                mut n = list_to_sort.length(); // Runtime provides the .length() method
                mut i = 0;
                while (i < n - 1) {
                    mut j = 0;
                    while (j < n - i - 1) {
                        if (list_to_sort[j] > list_to_sort[j+1]) {
                            // Swap elements
                            mut temp = list_to_sort[j];
                            list_to_sort[j] = list_to_sort[j+1];
                            list_to_sort[j+1] = temp;
                        };
                        j = j + 1;
                    };
                    i = i + 1;
                };
                // No need to return unit/none explicitly, hence commented out
                // return ();
            };

            mut unsorted = [5, 1, 4, 2, 8];
            bubble_sort(unsorted);
            unsorted == [1, 2, 4, 5, 8];
        "#;

        // When fully implemented and the `unsorted` list is modified in place,
        // and its first two elements are summed, this should return Value::Integer(3).
        assert_eq!(
            interpret_source(bubble_sort_source),
            Ok(Some(Value::Boolean(true)))
        );
    }

    #[test]
    fn test_interpret_list_mapping_with_lambda() {
        let map_lambda_source = r#"
            // Applies a function 'f' to each element of 'lst' and returns a new list
            // with the results.
            map(f: int -> int, lst: [int]): [int] = {
                mut result_list: [int] = [];
                for element in lst { // For loop
                    // Runtime provides a .push() method for lists
                    result_list = result_list.push(f(element));
                };
                return result_list;
            };

            // A lambda function that doubles an integer.
            double = (x: int) => x * 2;

            input_list = [1, 2, 3];
            // Apply the 'double' lambda to 'input_list'.
            mapped_list = map(double, input_list);
            mapped_list == [2, 4, 6];
        "#;

        // When fully implemented, this should return Value::Integer(3) (the length of the new list).
        // If a Value::List type were available, we would assert the list itself.
        assert_eq!(
            interpret_source(map_lambda_source),
            Ok(Some(Value::Boolean(true)))
        );
    }

    #[test]
    fn test_interpret_record_factory_and_access() {
        // This test defines a record type and a factory function to create instances of it.
        // It validates record type declarations, record literals, functions returning records,
        // and field access.

        let record_source = r#"
            // Define a Point record type.
            type Point = { x: int, y: int };

            // Factory function to create new Point records.
            make_point(x_val: int, y_val: int): Point = {
                return { x: x_val, y: y_val }; // Record literal creation
            }

            // Create a point using the factory function.
            origin = make_point(0, 0);
            my_point = make_point(10, 20);

            // Access a field of the created record.
            my_point.x; // Field access
        "#;

        // When fully implemented, this should return Value::Integer(10).
        assert_eq!(
            interpret_source(record_source),
            Ok(Some(Value::Integer(10)))
        );
    }

    #[test]
    fn test_snippet_fizzbuzz_last_element() {
        let source = r#"
            // Returns the last element of the FizzBuzz sequence up to N.
            fizzbuzz_last(n: int): string = {
                mut results: [string] = [];
                mut i = 1;
                while (i <= n) {
                    if (i % 15 == 0) {
                        results = results.push("FizzBuzz");
                    } else if (i % 3 == 0) {
                        results = results.push("Fizz");
                    } else if (i % 5 == 0) {
                        results = results.push("Buzz");
                    } else {
                        results = results.push(i.to_string());
                    };
                    i = i + 1;
                }
                results[n - 1] // Get last element
            }
            fizzbuzz_last(5); // Output: "Buzz"
        "#;
        assert_eq!(
            interpret_source(source),
            Ok(Some(Value::String("Buzz".to_string())))
        );
    }

    #[test]
    fn test_snippet_iterative_factorial() {
        let source = r#"
            // Calculates the factorial of n iteratively.
            factorial(n: int): int = {
                mut result = 1;
                mut i = 1;
                while (i <= n) {
                    result = result * i;
                    i = i + 1;
                }
                result
            }
            factorial(5); // 5! = 120
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(120))));
    }

    #[test]
    fn test_snippet_prime_number_checker() {
        let source = r#"
            // Checks if a number is prime.
            is_prime(n: int): bool = {
                if (n <= 1) {
                    return false;
                }
                mut i = 2;
                while (i * i <= n) {
                    if (n % i == 0) {
                        return false;
                    }
                    i = i + 1;
                }
                return true;
            }
            is_prime(7); // 7 is prime
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_snippet_gcd_euclidean_algorithm() {
        let source = r#"
            // Calculates the Greatest Common Divisor using Euclidean algorithm.
            gcd(a: int, b: int): int = {
                mut x = a;
                mut y = b;
                while (y != 0) {
                    mut temp = y;
                    y = x % y;
                    x = temp;
                }
                return x;
            }
            gcd(48, 18); // GCD(48, 18) = 6
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(6))));
    }

    #[test]
    fn test_snippet_list_reversal_first_element() {
        let source = r#"
            // Reverses a list and returns the first element of the new list.
            reverse_list(lst: [int]): [int] = {
                mut reversed: [int] = [];
                mut i = lst.length() - 1; // Conceptual .length()
                while (i >= 0) {
                    reversed = reversed.append(lst[i]); // Conceptual .append()
                    i = i - 1;
                }
                reversed
            }
            reverse_list([1, 2, 3])[0]; // Reversed is [3, 2, 1], first element is 3
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_count_occurrences_in_list() {
        let source = r#"
            // Counts occurrences of a target element in a list.
            count_occurrences(lst: [int], target: int): int = {
                mut count = 0;
                for element in lst {
                    if (element == target) {
                        count = count + 1;
                    }
                }
                count
            };
            count_occurrences([1, 2, 1, 3, 1], 1);
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_string_palindrome_checker() {
        let source = r#"
            is_palindrome(s: string): bool = {
                mut len = s.length();
                if (len <= 1) {
                    return true;
                }
                mut i = 0;
                while (i < len / 2) {
                    if (s.substring(i, 1) != s.substring(len - 1 - i, 1)) { // TODO: runtime provides .substring()
                        return false;
                    }
                    i = i + 1;
                }
                return true;
            }
            is_palindrome("madam");
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_snippet_sum_type_state_machine() {
        let source = r#"
            // Defines a state machine and returns the name of the next state.
            type State = Start | Running | Paused | End;

            get_next_state_name(current: State): string = {
                match (current) {
                    | Start => "Running",
                    | Running => "Paused",
                    | Paused => "End",
                    | End => "Start"
                }
            }
            get_next_state_name(Start); // Next state from Start is Running
        "#;
        assert_eq!(
            interpret_source(source),
            Ok(Some(Value::String("Running".to_string())))
        );
    }

    #[test]
    fn test_snippet_list_filtering_lambda_predicate() {
        let source = r#"
            // Filters a list based on a given predicate lambda and returns the first element of the filtered list.
            filter_list(predicate: int -> bool, lst: [int]): [int] = {
                mut filtered: [int] = [];
                for element in lst {
                    if (predicate(element)) {
                        filtered = filtered.push(element);
                    }
                };
                return filtered;
            }

            is_even = (x: int) => x % 2 == 0;
            input_list = [1, 2, 3, 4, 5];
            filter_list(is_even, input_list)[0]; // Filtered is [2, 4], first element is 2
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_snippet_find_maximum_element_in_list() {
        let source = r#"
            // Finds the maximum integer in a list.
            find_max(lst: [int]): int = {
                if (lst.length() == 0) { // Conceptual .length()
                    return -1; // Indicate error or empty list
                };
                mut max_val = lst[0];
                mut i = 1;
                while (i < lst.length()) {
                    if (lst[i] > max_val) {
                        max_val = lst[i];
                    };
                    i = i + 1;
                }
                return max_val;
            }
            find_max([10, 5, 99, 23, 7]);
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(99))));
    }

    #[test]
    fn test_snippet_average_of_list_of_numbers() {
        let source = r#"
            // Calculates the average of integers in a list.
            average(lst: [int]): float = {
                if (lst.length() == 0) { // Conceptual .length()
                    return 0.0;
                }
                mut sum_val = 0;
                for element in lst {
                    sum_val = sum_val + element;
                }
                return sum_val.to_float() / lst.length(); // Runtime provided 'to_float()' method
            }
            average([1, 2, 3, 4, 5]); // (1+2+3+4+5)/5 = 15/5 = 3.0
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Float(3.0))));
    }

    #[test]
    fn test_snippet_calculate_distance_squared_between_points() {
        let source = r#"
            // Defines a Point record and calculates the squared Euclidean distance between two points.
            type Point = { x: int, y: int };

            distance_squared(p1: Point, p2: Point): float = {
                mut dx = p1.x - p2.x;
                mut dy = p1.y - p2.y;
                return (dx * dx + dy * dy).to_float(); // Returns squared distance
            };
            distance_squared({x:0, y:0}, {x:3, y:4}); // dx=3, dy=4. (3*3 + 4*4) = 9 + 16 = 25.0
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Float(25.0))));
    }

    #[test]
    fn test_snippet_simple_vowel_counter() {
        let source = r#"
            // Counts the number of vowels in a string.
            count_vowels(s: string): int = {
                mut count = 0;
                mut i = 0;
                while (i < s.length()) { // Runtime provides .length() and .substring() methods
                    mut char_str = s.substring(i, 1);
                    if (char_str == "a" || char_str == "e" || char_str == "i" || char_str == "o" || char_str == "u") {
                        count = count + 1;
                    };
                    i = i + 1;
                };
                return count;
            };
            count_vowels("hello world"); // 'e', 'o', 'o' -> 3 vowels
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_collatz_conjecture_step_function() {
        let source = r#"
            // Implements one step of the Collatz sequence.
            collatz_step(n: int): int = {
                if (n % 2 == 0) {
                    n / 2
                } else {
                    return n * 3 + 1;  // test both implicit and explicit return here
                }
            };
            collatz_step(10); // 10 is even, so 10 / 2 = 5
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_snippet_convert_celsius_to_fahrenheit() {
        let source = r#"
            // Converts temperature from Celsius to Fahrenheit.
            celsius_to_fahrenheit(celsius: float): float = {
                celsius * 9.0 / 5.0 + 32.0
            };
            celsius_to_fahrenheit(0.0); // 0 Celsius = 32 Fahrenheit
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Float(32.0))));
    }

    #[test]
    fn test_snippet_find_unique_elements_first_element() {
        let source = r#"
            // Helper function to check if a list contains an element.
            contains(lst: [int], target: int): bool = {
                for element in lst {
                    if (element == target) {
                        return true;
                    }
                };
                return false;
            }

            // Returns a new list with only unique elements and then takes the first element.
            unique_elements(lst: [int]): [int] = {
                mut uniques: [int] = [];
                for element in lst {
                    if (!contains(uniques, element)) {
                        uniques = uniques.append(element); // Conceptual .append()
                    };
                };
                return uniques;
            };
            unique_elements([1, 2, 2, 3, 1])[0]; // Unique elements: [1, 2, 3], first is 1
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_snippet_sum_until_five_or_max() {
        let source = r#"
            // Sums numbers from 1 up to a max_val, but stops (conceptually breaks) if 5 is reached.
            sum_until_five_or_max(max_val: int): int = {
                mut sum = 0;
                mut i = 1;
                while (i <= max_val) {
                    if (i == 5) {
                        break;
                    } else {
                        sum = sum + i;
                        i = i + 1;
                    };
                };
                return sum;
            };
            sum_until_five_or_max(10); // Sums 1, 2, 3, 4 = 10 (breaks at 5)
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_snippet_match_day_name_default() {
        let source = r#"
            // Returns the name of the day for a given day number, with a default.
            get_day_name(day_num: int): string = {
                match (day_num) {
                    | 1 => "Monday",
                    | 2 => "Tuesday",
                    | 3 => "Wednesday",
                    | 4 => "Thursday",
                    | 5 => "Friday",
                    | 6 => "Saturday",
                    | _ => "Sunday" // Wildcard for 0 or > 6
                }
            };
            get_day_name(3); // 3rd day is Wednesday
        "#;
        assert_eq!(
            interpret_source(source),
            Ok(Some(Value::String("Wednesday".to_string())))
        );
    }

    #[test]
    fn test_snippet_calculate_nth_power_iterative() {
        let source = r#"
            // Calculates base to the power of exponent iteratively.
            power(base, exponent: int) = {
                if (exponent < 0) {
                    return 0; // Or handle as error
                };
                if (exponent == 0) {
                    return 1;
                };
                mut result = 1;
                mut i = 0;
                while (i < exponent) {
                    result = result * base;
                    i = i + 1;
                };
                return result;
            };
            power(5, 3); // 5^3 = 125
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(125))));
    }

    #[test]
    fn test_snippet_map_records_to_list_of_fields_first_element() {
        let source = r#"
            // Defines a Point record, creates a list of points, and maps them to a list of x-coordinates.
            type Point = { x: int, y: int };

            map_points_to_x(points: [Point]): [int] = {
                mut x_coords: [int] = [];
                for p in points {
                    x_coords.push(p.x); // Runtime provided method
                }
                return x_coords;
            };

            list_of_points = [{x:1, y:10}, {x:3, y:30}, {x:5, y:50}];
            map_points_to_x(list_of_points)[1];
        "#;
        assert_eq!(interpret_source(source), Ok(Some(Value::Integer(3))));
    }
}
