use tap::diagnostics::Reporter;
use tap::interpreter::{Interpreter, RuntimeError, Value};
use tap::lexer::Lexer;
use tap::parser::Parser;
use tap::utils::pretty_print_tokens;

// Assume Program is defined as part of tap::parser module
// If Program is not directly accessible, we might need a type alias or to
// infer its path based on where `Parser::parse_program` returns it.
// For now, let's assume it's `tap::ast::Program` or similar.
// If not, replace `Program` with the actual type.
type AstProgram = tap::ast::Program; // Adjust this if `Program` is in a different module or named differently

// A new struct to hold both the interpretation result and the AST
struct InterpretOutput {
    pub result: Result<Option<Value>, RuntimeError>,
    pub ast: Option<AstProgram>, // Store the AST here, it might be None if parsing failed
    pub source: String,          // Store the source for better error messages
}

// Helper function to interpret a source string and return the result along with the AST
fn interpret_source_with_ast(source: &str) -> InterpretOutput {
    let mut reporter = Reporter::new();
    let tokens = Lexer::new(source, &mut reporter)
        .tokenize()
        .unwrap_or_else(|e| {
            eprintln!("Lexing failed for source:\n\"{}\"\nError: {:?}", source, e);
            panic!("Lexing failed.");
        });

    if reporter.has_errors() {
        eprintln!(
            "Lexing failed with reporter errors for source:\n\"{}\"\nTokens: {}\nLexer Errors: {:?}",
            source,
            pretty_print_tokens(&tokens),
            reporter.diagnostics
        );
        panic!("Lexing failed with reporter errors.");
    }

    let mut parser = Parser::new(&tokens, &mut reporter);
    let program_result = parser.parse_program();

    if reporter.has_errors() {
        eprintln!(
            "Parsing failed with reporter errors for source:\n\"{}\"\nTokens: {}\nParser Errors: {:?}\nAST: {:#?}",
            source,
            pretty_print_tokens(&tokens),
            reporter.diagnostics,
            program_result // This will print Result::Ok(Program) or Result::Err(Diagnostic)
        );
        panic!("Parsing failed with reporter errors.");
    }

    let program = program_result.expect("Parser failed unexpectedly but no errors reported.");
    let mut interpreter = Interpreter::new();
    let interpretation_result = interpreter.interpret(&program);

    InterpretOutput {
        result: interpretation_result,
        ast: Some(program),
        source: source.to_string(),
    }
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;

    // A new helper macro to reduce boilerplate in tests
    macro_rules! assert_interpret_output_and_dump_ast {
        ($source:expr, $expected:expr) => {{
            let output = interpret_source_with_ast($source);
            // Explicitly type the expected value to help the compiler infer generic parameters for Result
            let expected_val: Result<Option<Value>, RuntimeError> = $expected;
            if output.result != expected_val {
                eprintln!("\n--- Test Assertion Failed ---");
                eprintln!("Source:\n```tap\n{}\n```", output.source);
                if let Some(ast) = output.ast {
                    eprintln!("AST:\n{:#?}", ast);
                } else {
                    eprintln!("AST: Not available due to parsing error.");
                }
                eprintln!("Expected: {:?}", expected_val); // Use the explicitly typed variable here
                eprintln!("Actual: {:?}", output.result);
                eprintln!("--- End Test Assertion Failed ---");
                panic!("Assertion failed: Interpreter output did not match expected value. See above for details and AST dump.");
            }
        }};
    }

    // --- Small Snippets (Core Functionality) ---

    #[test]
    fn test_interpret_integer_literal() {
        assert_interpret_output_and_dump_ast!("123;", Ok(Some(Value::Integer(123))));
    }

    #[test]
    fn test_interpret_float_literal() {
        assert_interpret_output_and_dump_ast!("3.14;", Ok(Some(Value::Float(3.14))));
    }

    #[test]
    fn test_interpret_string_literal() {
        assert_interpret_output_and_dump_ast!(
            "\"hello\";",
            Ok(Some(Value::String("hello".to_string())))
        );
    }

    #[test]
    fn test_interpret_boolean_literal_true() {
        assert_interpret_output_and_dump_ast!("true;", Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_boolean_literal_false() {
        assert_interpret_output_and_dump_ast!("false;", Ok(Some(Value::Boolean(false))));
    }

    #[test]
    fn test_interpret_none_literal() {
        assert_interpret_output_and_dump_ast!("None;", Ok(Some(Value::Unit)));
    }

    #[test]
    fn test_interpret_integer_addition() {
        assert_interpret_output_and_dump_ast!("1 + 2;", Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_interpret_float_multiplication() {
        assert_interpret_output_and_dump_ast!("2.5 * 2.0;", Ok(Some(Value::Float(5.0))));
    }

    #[test]
    fn test_interpret_integer_division() {
        assert_interpret_output_and_dump_ast!("10 / 2;", Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_division_by_zero() {
        assert_interpret_output_and_dump_ast!("10 / 0;", Err(RuntimeError::DivisionByZero));
    }

    #[test]
    fn test_interpret_unary_minus_integer() {
        assert_interpret_output_and_dump_ast!("-5;", Ok(Some(Value::Integer(-5))));
    }

    #[test]
    fn test_interpret_unary_not_boolean() {
        assert_interpret_output_and_dump_ast!("!true;", Ok(Some(Value::Boolean(false))));
        assert_interpret_output_and_dump_ast!("!false;", Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_unary_not_truthiness() {
        assert_interpret_output_and_dump_ast!("!10;", Ok(Some(Value::Boolean(false))));
        assert_interpret_output_and_dump_ast!("!\"hello\";", Ok(Some(Value::Boolean(false))));
        assert_interpret_output_and_dump_ast!("!None;", Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_variable_declaration_no_type() {
        let source = "x = 10; x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_variable_declaration_with_type() {
        let source = "x: int = 10; x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_mutable_variable_declaration() {
        let source = "mut x: int = 10; x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_variable_in_expression() {
        let source = "x = 5; x + 3;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(8))));
    }

    #[test]
    fn test_interpret_multiple_variable_declarations() {
        let source = "a = 1; b = 2; a * b;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_interpret_undefined_variable() {
        let source = "x;";
        assert_interpret_output_and_dump_ast!(
            source,
            Err(RuntimeError::Type("Undefined variable: x".to_string()))
        );
    }

    #[test]
    fn test_interpret_binary_comparison_equal() {
        assert_interpret_output_and_dump_ast!("1 == 1;", Ok(Some(Value::Boolean(true))));
        assert_interpret_output_and_dump_ast!("1 == 2;", Ok(Some(Value::Boolean(false))));
    }

    #[test]
    fn test_interpret_binary_logical_and_error() {
        assert_interpret_output_and_dump_ast!(
            "1 && 0;",
            Err(RuntimeError::Type(
                "Type mismatch in binary operation".into()
            ))
        );
    }

    #[test]
    fn test_interpret_mixed_types_arithmetic_error() {
        let source = "10 + 3.5;";
        assert_interpret_output_and_dump_ast!(
            source,
            Err(RuntimeError::Type(
                "Type mismatch in binary operation".to_string()
            ))
        );
    }

    #[test]
    fn test_interpret_parenthesized_expression() {
        assert_interpret_output_and_dump_ast!("(2 + 3) * 4;", Ok(Some(Value::Integer(20))));
    }

    // --- Tests for implemented features ---

    #[test]
    fn test_interpret_type_declaration() {
        assert_interpret_output_and_dump_ast!(
            "type Option = Some(int) | None;",
            Ok(Some(Value::Unit))
        );
    }

    #[test]
    fn test_interpret_function_definition() {
        let source = "add(a: int, b: int): int = { a + b }; add(2, 3);";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_if_expression() {
        assert_interpret_output_and_dump_ast!(
            "if (true) { 1 } else { 0 };",
            Ok(Some(Value::Integer(1)))
        );
    }

    #[test]
    fn test_interpret_while_expression() {
        let source = "mut i = 0; while (i < 3) { i = i + 1; }; i;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_interpret_for_expression() {
        let source = "mut sum = 0; for i in [1, 2] { sum = sum + i; }; sum;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_interpret_match_expression() {
        assert_interpret_output_and_dump_ast!(
            "match (1) { | _ => 1 };",
            Ok(Some(Value::Integer(1)))
        );
    }

    #[test]
    fn test_interpret_lambda_expression() {
        let source = "f = (x: int) => x + 1; f(5);";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(6))));
    }

    #[test]
    fn test_interpret_record_literal() {
        let source = "p = { x: 1, y: 2 }; p.x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_list_literal() {
        let source = "numbers = [1, 2, 3]; numbers[0];";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_field_access() {
        let source = "p = { x: 10, y: 20 }; p.x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_list_access() {
        let source = "arr = [1, 2, 3]; arr[0];";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_function_call() {
        let source = "my_func(): int = { 0 }; my_func();";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(0))));
    }

    #[test]
    fn test_interpret_compound_assignment() {
        let source = "mut x = 10; x += 5; x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(15))));
    }

    #[test]
    fn test_interpret_compound_assignment_add() {
        let source = "mut x = 10; x += 5; x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(15))));
    }

    #[test]
    fn test_interpret_compound_assignment_subtract() {
        let source = "mut x = 10; x -= 5; x;";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(5))));
    }

    // --- More Full-Fledged Examples ---

    #[test]
    fn test_interpret_complex_arithmetic_with_variables() {
        let source = "
            x = 10;
            y = 5;
            result = (x + y) * 2 / 3;
            result;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_complex_nested_expression() {
        let source = "
            complex_expr(): int = {
                if (true) {
                    match (None) {
                        | _ => { 1 }
                    }
                } else {
                    -1
                }
            };
            complex_expr();
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_multiple_statements() {
        let source = "
            val1 = 1;
            val2 = 2;
            val3 = val1 + val2;
            val3;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_interpret_complex_precedence() {
        assert_interpret_output_and_dump_ast!("1 + 2 * 3 - 4 / 2;", Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_nested_parentheses() {
        assert_interpret_output_and_dump_ast!("((1 + 1) * 2) - 3;", Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_unary_minus_in_expression() {
        assert_interpret_output_and_dump_ast!("10 + (-5);", Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_unary_plus() {
        assert_interpret_output_and_dump_ast!("+10;", Ok(Some(Value::Integer(10))));
        assert_interpret_output_and_dump_ast!("+(2.5);", Ok(Some(Value::Float(2.5))));
    }

    #[test]
    fn test_interpret_chain_variable_assignment_then_access() {
        let source = "
            a = 5;
            b = a;
            b;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_interpret_variable_shadowing_not_supported_yet() {
        let source = "
            x = 10;
            x = 20;
            x;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(20))));
    }

    // --- Additional Tests for Implemented Features ---

    #[test]
    fn test_interpret_record_type_declaration() {
        assert_interpret_output_and_dump_ast!(
            "type Point = { x: int, y: int };",
            Ok(Some(Value::Unit))
        );
    }

    #[test]
    fn test_interpret_generic_type_declaration() {
        assert_interpret_output_and_dump_ast!("type IntList = [int];", Ok(Some(Value::Unit)));
    }

    #[test]
    fn test_interpret_if_else_if_expression() {
        let source = "
            val = if (false) { 1 } else if (true) { 2 } else { 3 };
            val;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_interpret_while_loop_with_block() {
        let source = "
            mut i = 0;
            while (i < 2) {
                i = i + 1;
            };
            i;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_interpret_for_loop_identifier_pattern() {
        let source = "
            mut sum = 0;
            for i in [1, 2, 3] {
                sum = sum + i;
            };
            sum;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(6))));
    }

    #[test]
    fn test_interpret_for_loop_wildcard_pattern() {
        let source = "
            mut count = 0;
            for _ in [1, 2] {
                count = count + 1;
            };
            count;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_interpret_match_expression_with_variant() {
        let source = "
            type Option = Some(int) | None;
            opt_val = Some(10);
            match (opt_val) {
                | Some(x) => x,
                | None => 0
            };
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_match_expression_with_multiple_arms() {
        let source = "
            type Result = Ok(string) | Error(int);
            res_val = Ok(\"success\");
            match (res_val) {
                | Ok(s) => s,
                | Error(e) => \"failure\"
            };
        ";
        assert_interpret_output_and_dump_ast!(
            source,
            Ok(Some(Value::String("success".to_string())))
        );
    }

    #[test]
    fn test_interpret_lambda_assignment() {
        let source = "
            my_lambda = (x: int) => x * 2;
            my_lambda(5);
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_record_literal_and_access() {
        let source = "
            p = { x: 10, y: 20 };
            p.x;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    #[ignore]
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
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Float(314.0))));
    }

    #[test]
    fn test_interpret_list_literal_and_access() {
        let source = "
            arr = [1, 2, 3];
            arr[0];
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_factorial_function() {
        let source = "
            factorial(n: int): int = {
                mut result = 1;
                mut i = 1;
                while (i <= n) {
                    result = result * i;
                    i = i + 1;
                }
                result
            };
            factorial(5);
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(120))));
    }

    #[test]
    fn test_interpret_access_demo() {
        let source = "
            add(a: int, b: int): int = { a + b }
            access_demo(): int = {
                p = { x: 10, y: 20 };
                x_val = p.x;
                arr = [1, 2, 3];
                first = arr[0];
                result = add(x_val, first);
                result
            };
            access_demo();
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(11))));
    }

    #[test]
    fn test_interpret_variant_construction() {
        let source = "
            type Option = Some(int) | None;
            some_value = Some(42);
            no_value = None;
            some_value;
        ";
        // Should construct a variant value
        // assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Variant("Some".to_string(), Box::new(Value::Integer(42))))));
        // The original test just checked `is_ok()`, which this macro will handle if the output matches.
        let output = interpret_source_with_ast(source);
        assert!(output.result.is_ok());
    }

    #[test]
    fn test_interpret_block_as_final_expression_in_function() {
        let source = "
            my_func(): int = {
                {
                    a = 1;
                    a + 1
                }
            };
            my_func();
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_interpret_arithmetic_with_multiple_variables() {
        let source = "
            v1 = 10;
            v2 = 5;
            v3 = 2;
            result = v1 + v2 * v3;
            result;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(20))));
    }

    #[test]
    fn test_interpret_boolean_expressions_with_variables_error() {
        let source = "
            is_valid = true;
            count = 10;
            check = is_valid && count;
            check;
        ";
        assert_interpret_output_and_dump_ast!(
            source,
            Err(RuntimeError::Type(
                "Type mismatch in binary operation".to_string()
            ))
        );
    }

    #[test]
    fn test_basic_range_inclusive() {
        let source = "
            mut sum = 0;
            for i in 0..=5 {
                sum += i;
            }
            sum;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(15))));
    }

    #[test]
    fn test_basic_range_exclusive() {
        let source = "
            mut sum = 0;
            for i in 0..<5 {
                sum += i;
            }
            sum;
        ";
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_interpret_dfs_example() {
        let dfs_source = r#"
            dfs_visit_and_count(graph_adj: [[int]], u: int, visited_status: [bool], num_nodes: int): int = {
                if (u < 0 || u >= num_nodes) {
                    return 0;
                };

                if (visited_status[u]) {
                    return 0;
                };

                visited_status[u] = true;

                mut count = 1;

                for v in graph_adj[u] {
                    count = count + dfs_visit_and_count(graph_adj, v, visited_status, num_nodes);
                };

                count
            };

            run_dfs(graph: [[int]], start_node: int, graph_size: int): int = {
                mut visited_nodes: [bool] = [false, false, false, false];
                dfs_visit_and_count(graph, start_node, visited_nodes, graph_size)
            };

            example_graph = [[1, 2], [0, 3], [0], [1]];
            num_example_nodes = 4;

            run_dfs(example_graph, 0, num_example_nodes);
        "#;

        assert_interpret_output_and_dump_ast!(dfs_source, Ok(Some(Value::Integer(4))));
    }

    #[test]
    fn test_interpret_recursive_fibonacci() {
        let fib_source = r#"
            fib(n: int): int = {
                if (n <= 1) {
                    n
                } else {
                    fib(n - 1) + fib(n - 2)
                }
            }

            fib(6);
        "#;

        assert_interpret_output_and_dump_ast!(fib_source, Ok(Some(Value::Integer(8))));
    }

    #[test]
    fn test_interpret_iterative_binary_search() {
        let binary_search_source = r#"
            binary_search(list: [int], target: int): int = {
                mut low = 0;
                mut high = list.length() - 1;

                while (low <= high) {
                    mut mid = low + (high - low) / 2;

                    if (list[mid] == target) {
                        return mid;
                    } else if (list[mid] < target) {
                        low = mid + 1;
                    } else {
                        high = mid - 1;
                    }
                };

                return -1;
            };

            sorted_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            target_value = 7;
            binary_search(sorted_list, target_value);
        "#;

        assert_interpret_output_and_dump_ast!(binary_search_source, Ok(Some(Value::Integer(6))));
    }

    #[test]
    #[ignore]
    fn test_interpret_bubble_sort() {
        let bubble_sort_source = r#"
            bubble_sort(list_to_sort: [int]) = {
                mut n = list_to_sort.length();
                mut i = 0;
                while (i < n - 1) {
                    mut j = 0;
                    while (j < n - i - 1) {
                        if (list_to_sort[j] > list_to_sort[j+1]) {
                            mut temp = list_to_sort[j];
                            list_to_sort[j] = list_to_sort[j+1];
                            list_to_sort[j+1] = temp;
                        };
                        j = j + 1;
                    };
                    i = i + 1;
                };
            };

            mut unsorted = [5, 1, 4, 2, 8];
            bubble_sort(unsorted);
            unsorted == [1, 2, 4, 5, 8];
        "#;

        assert_interpret_output_and_dump_ast!(bubble_sort_source, Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_list_mapping_with_lambda() {
        let map_lambda_source = r#"
            map(f: int -> int, lst: [int]): [int] = {
                mut result_list: [int] = [];
                for element in lst {
                    result_list = result_list.push(f(element));
                };
                return result_list;
            };

            double = (x: int) => x * 2;

            input_list = [1, 2, 3];
            mapped_list = map(double, input_list);
            mapped_list == [2, 4, 6];
        "#;

        assert_interpret_output_and_dump_ast!(map_lambda_source, Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_interpret_record_factory_and_access() {
        let record_source = r#"
            type Point = { x: int, y: int };

            make_point(x_val: int, y_val: int): Point = {
                return { x: x_val, y: y_val };
            }

            origin = make_point(0, 0);
            my_point = make_point(10, 20);

            my_point.x;
        "#;

        assert_interpret_output_and_dump_ast!(record_source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_snippet_fizzbuzz_last_element() {
        let source = r#"
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
                results[n - 1]
            }
            fizzbuzz_last(5);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::String("Buzz".to_string()))));
    }

    #[test]
    fn test_snippet_iterative_factorial() {
        let source = r#"
            factorial(n: int): int = {
                mut result = 1;
                mut i = 1;
                while (i <= n) {
                    result = result * i;
                    i = i + 1;
                }
                result
            }
            factorial(5);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(120))));
    }

    #[test]
    fn test_snippet_prime_number_checker() {
        let source = r#"
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
            is_prime(7);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_snippet_gcd_euclidean_algorithm() {
        let source = r#"
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
            gcd(48, 18);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(6))));
    }

    #[test]
    fn test_snippet_list_reversal_first_element() {
        let source = r#"
            reverse_list(lst: [int]): [int] = {
                mut reversed: [int] = [];
                mut i = lst.length() - 1;
                while (i >= 0) {
                    reversed = reversed.append(lst[i]);
                    i = i - 1;
                }
                reversed
            }
            reverse_list([1, 2, 3])[0];
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_count_occurrences_in_list() {
        let source = r#"
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
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
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
                    if (s.substring(i, 1) != s.substring(len - 1 - i, 1)) {
                        return false;
                    }
                    i = i + 1;
                }
                return true;
            }
            is_palindrome("madam");
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_snippet_sum_type_state_machine() {
        let source = r#"
            type State = Start | Running | Paused | End;

            get_next_state_name(current: State): string = {
                match (current) {
                    | Start => "Running",
                    | Running => "Paused",
                    | Paused => "End",
                    | End => "Start"
                }
            }
            get_next_state_name(Start);
        "#;
        assert_interpret_output_and_dump_ast!(
            source,
            Ok(Some(Value::String("Running".to_string())))
        );
    }

    #[test]
    fn test_snippet_list_filtering_lambda_predicate() {
        let source = r#"
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
            filter_list(is_even, input_list)[0];
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(2))));
    }

    #[test]
    fn test_snippet_find_maximum_element_in_list() {
        let source = r#"
            find_max(lst: [int]): int = {
                if (lst.length() == 0) {
                    return -1;
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
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(99))));
    }

    #[test]
    fn test_snippet_average_of_list_of_numbers() {
        let source = r#"
            average(lst: [int]): float = {
                if (lst.length() == 0) {
                    return 0.0;
                }
                mut sum_val = 0;
                for element in lst {
                    sum_val = sum_val + element;
                }
                return sum_val.to_float() / lst.length();
            }
            average([1, 2, 3, 4, 5]);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Float(3.0))));
    }

    #[test]
    fn test_snippet_calculate_distance_squared_between_points() {
        let source = r#"
            type Point = { x: int, y: int };

            distance_squared(p1: Point, p2: Point): float = {
                mut dx = p1.x - p2.x;
                mut dy = p1.y - p2.y;
                return (dx * dx + dy * dy).to_float();
            };
            distance_squared({x:0, y:0}, {x:3, y:4});
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Float(25.0))));
    }

    #[test]
    fn test_snippet_simple_vowel_counter() {
        let source = r#"
            count_vowels(s: string): int = {
                mut count = 0;
                mut i = 0;
                while (i < s.length()) {
                    mut char_str = s.substring(i, 1);
                    if (char_str == "a" || char_str == "e" || char_str == "i" || char_str == "o" || char_str == "u") {
                        count = count + 1;
                    };
                    i = i + 1;
                };
                return count;
            };
            count_vowels("hello world");
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_collatz_conjecture_step_function() {
        let source = r#"
            collatz_step(n: int): int = {
                if (n % 2 == 0) {
                    n / 2
                } else {
                    return n * 3 + 1;
                }
            };
            collatz_step(10);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(5))));
    }

    #[test]
    fn test_snippet_convert_celsius_to_fahrenheit() {
        let source = r#"
            celsius_to_fahrenheit(celsius: float): float = {
                celsius * 9.0 / 5.0 + 32.0
            };
            celsius_to_fahrenheit(0.0);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Float(32.0))));
    }

    #[test]
    fn test_snippet_find_unique_elements_first_element() {
        let source = r#"
            contains(lst: [int], target: int): bool = {
                for element in lst {
                    if (element == target) {
                        return true;
                    }
                };
                return false;
            }

            unique_elements(lst: [int]): [int] = {
                mut uniques: [int] = [];
                for element in lst {
                    if (!contains(uniques, element)) {
                        uniques = uniques.append(element);
                    };
                };
                return uniques;
            };
            unique_elements([1, 2, 2, 3, 1])[0];
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_snippet_sum_until_five_or_max() {
        let source = r#"
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
            sum_until_five_or_max(10);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(10))));
    }

    #[test]
    fn test_snippet_match_day_name_default() {
        let source = r#"
            get_day_name(day_num: int): string = {
                match (day_num) {
                    | 1 => "Monday",
                    | 2 => "Tuesday",
                    | 3 => "Wednesday",
                    | 4 => "Thursday",
                    | 5 => "Friday",
                    | 6 => "Saturday",
                    | _ => "Sunday"
                }
            };
            get_day_name(3);
        "#;
        assert_interpret_output_and_dump_ast!(
            source,
            Ok(Some(Value::String("Wednesday".to_string())))
        );
    }

    #[test]
    fn test_snippet_calculate_nth_power_iterative() {
        let source = r#"
            power(base, exponent: int) = {
                if (exponent < 0) {
                    return 0;
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
            power(5, 3);
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(125))));
    }

    #[test]
    fn test_snippet_map_records_to_list_of_fields_first_element() {
        let source = r#"
            type Point = { x: int, y: int };

            map_points_to_x(points: [Point]): [int] = {
                mut x_coords: [int] = [];
                for p in points {
                    x_coords = x_coords.push(p.x);
                }
                return x_coords;
            };

            list_of_points = [{x:1, y:10}, {x:3, y:30}, {x:5, y:50}];
            map_points_to_x(list_of_points)[1];
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_aoc_2025_day1_1() {
        let source = r#"
            solve(): int = {
                // Hardcoded example turns (until we implement I/O)
                turns = [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82];

                mut res = 0;
                mut dial = 50;

                for turn in turns {
                    dial = dial + turn;
                    dial = dial % 100;

                    if (dial == 0) {
                        res = res + 1;
                    }
                }

                res
            };

            solve();
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_snippet_aoc_2025_day1_1_input_parsing() {
        let source = r#"
            get_file_content(): string = {
                "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"
            }

            // Parse a line like "R60" or "L30" into a turn value
            // R becomes positive, L becomes negative
            parse_turn(line: string): int = {
                direction = line.char_at(0);
                len = line.length();
                value_str = line.substring(1, len - 1);
                value = value_str.parse_int();

                if (direction == "L") {
                    -value
                } else {
                    value
                }
            }

            // Parse all lines into a list of turns
            get_turns(content: string): [int] = {
                lines = content.split("\n");
                mut turns: [int] = [];

                for line in lines {
                    trimmed = line.trim();
                    if (trimmed.length() > 0) {
                        turn = parse_turn(trimmed);
                        turns = turns.push(turn);
                    }
                }

                turns
            }

            solve(): int = {
                content = get_file_content();
                turns = get_turns(content);

                mut res = 0;
                mut dial = 50;

                for turn in turns {
                    dial = dial + turn;
                    dial = dial % 100;

                    // Check if dial reached zero
                    if (dial == 0) {
                        res = res + 1;
                    }
                }

                // Return the count of times dial reached zero
                res
            }

            solve();
        "#;
        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(3))));
    }

    #[test]
    fn test_file_processing_line_by_line() {
        let source = r#"
            // Create a file with numbers
            file = open("/tmp/test_numbers.txt", "w");
            file.write_line("10");
            file.write_line("20");
            file.write_line("30");
            file.close();

            // Read and sum the numbers
            file2 = open("/tmp/test_numbers.txt", "r");
            lines = file2.read_lines();
            file2.close();

            mut sum = 0;
            for line in lines {
                num = line.trim().parse_int();
                sum = sum + num;
            };

            sum;
        "#;

        // Cleanup before test
        std::fs::remove_file("/tmp/test_numbers.txt").ok();

        let result = interpret_source_with_ast(source).result;

        // Cleanup after test
        std::fs::remove_file("/tmp/test_numbers.txt").ok();

        assert_eq!(result, Ok(Some(Value::Integer(60))));
    }
    #[test]
    fn test_file_error_invalid_mode() {
        let source = r#"
            file = open("test.txt", "invalid");
        "#;

        let result = interpret_source_with_ast(source).result;
        assert!(matches!(result, Err(RuntimeError::Type(_))));
    }

    #[test]
    fn test_args_out_of_bounds() {
        let source = r#"
            args.get(100);
        "#;

        let mut reporter = Reporter::new();
        let tokens = Lexer::new(source, &mut reporter).tokenize().unwrap();
        let mut parser = Parser::new(&tokens, &mut reporter);
        let program = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new_with_args(vec!["program".to_string()]);
        let result = interpreter.interpret(&program);

        assert_eq!(result, Ok(Some(Value::Unit)));
    }

    #[test]
    fn test_args_missing_option() {
        let source = r#"
            args.get_option("--missing");
        "#;

        let mut reporter = Reporter::new();
        let tokens = Lexer::new(source, &mut reporter).tokenize().unwrap();
        let mut parser = Parser::new(&tokens, &mut reporter);
        let program = parser.parse_program().unwrap();

        let mut interpreter = Interpreter::new_with_args(vec!["program".to_string()]);
        let result = interpreter.interpret(&program);

        assert_eq!(result, Ok(Some(Value::Unit)));
    }

    #[test]
    fn test_hashmap_has_key() {
        let source = r#"
            mut scores = Map();
            scores.insert("Alice", 100);
            scores.insert("Bob", 85);

            res: int = if (scores.has("Alice")) {
                1
            } else {
                0
            };
            res
        "#;

        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(1))));
    }

    #[test]
    fn test_hashmap_get() {
        let source = r#"
            mut scores = Map();
            scores.insert("Alice", 100);
            scores.insert("Bob", 85);

            scores.get("Alice");
        "#;

        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(100))));
    }

    #[test]
    fn test_hashmap_entries() {
        let source = r#"
                mut scores = Map();
                scores.insert("Alice", 100);
                scores.insert("Bob", 85);

                res: int = 0;
                for entry in scores.entries() {
                    if (entry.key == "Alice") {
                        res += entry.value;
                    }
                    if (entry.key == "Bob") {
                        res += entry.value;
                    }
                    if (entry.key == "Mallory") {
                        res += 12345;
                    }
                }
                res;
            "#;

        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(185))));
    }

    #[test]
    fn test_hashmap_keys() {
        let source = r#"
                mut scores = Map();
                scores.insert("Alice", 100);
                scores.insert("Bob", 85);

                res: int = 0;
                keys: [string] = scores.keys();
                keys.length() == 2 && keys.contains("Alice") && keys.contains("Bob");
            "#;

        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_hashmap_values() {
        let source = r#"
                    mut scores = Map();
                    scores.insert("Alice", 100);
                    scores.insert("Bob", 85);

                    res: int = 0;
                    values: [int] = scores.values();
                    values.contains(100) && values.contains(85);
                "#;

        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Boolean(true))));
    }

    #[test]
    fn test_hashmap_method_chaining() {
        let source = r#"
            mut scores = Map();
            scores.insert("Alice", 100).insert("Bob", 85);

            scores.get("Bob") + scores.get("Alice");
        "#;

        assert_interpret_output_and_dump_ast!(source, Ok(Some(Value::Integer(185))));
    }
}
