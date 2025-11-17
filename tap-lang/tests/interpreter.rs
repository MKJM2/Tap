use tap_lang::environment::Environment;
use tap_lang::interpreter::{Interpreter, Value};
use tap_lang::lexer::Lexer;
use tap_lang::parser::Parser;

// --- TEST HELPERS ---

/// Helper for tests that are expected to succeed.
/// It lexes, parses, and interprets the source, panicking with a rich error if any step fails.
/// Returns the final value of the last expression, if any.
fn eval_source(source: &str) -> Option<Value> {
    let tokens = Lexer::new(source).tokenize().unwrap_or_else(|e| {
        panic!(
            "Lexing failed for source:\n{}\nError: {:?}",
            source.trim(),
            e
        );
    });

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap_or_else(|e| {
        panic!(
            "Parsing failed for source:\n{}\nError Report:\n{:?}",
            source.trim(),
            e
        );
    });

    let interpreter = Interpreter::new();
    let mut env = Environment::new();
    interpreter
        .interpret(&program, &mut env)
        .unwrap_or_else(|e| {
            panic!(
                "Interpretation failed for source:\n{}\nRuntime Error: {}",
                source.trim(),
                e
            );
        })
}

/// Helper for tests that are expected to fail at runtime.
/// It will panic if parsing fails or if the interpreter *succeeds*.
fn expect_runtime_error(source: &str) {
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap();
    let interpreter = Interpreter::new();
    let mut env = Environment::new();
    let result = interpreter.interpret(&program, &mut env);
    assert!(
        result.is_err(),
        "Expected a runtime error, but execution succeeded for source:\n{}",
        source
    );
}

// --- EXPRESSION & OPERATOR TESTS ---

#[test]
fn test_literal_evaluation() {
    assert_eq!(eval_source("42;").unwrap(), Value::Integer(42));
    assert_eq!(
        eval_source("\"hello\";").unwrap(),
        Value::String("hello".to_string())
    );
    assert_eq!(eval_source("true;").unwrap(), Value::Boolean(true));
    assert_eq!(eval_source("false;").unwrap(), Value::Boolean(false));
}

#[test]
fn test_arithmetic_expressions() {
    assert_eq!(eval_source("10 + 5;").unwrap(), Value::Integer(15));
    assert_eq!(eval_source("10 - 5;").unwrap(), Value::Integer(5));
    assert_eq!(eval_source("10 * 5;").unwrap(), Value::Integer(50));
    assert_eq!(eval_source("10 / 5;").unwrap(), Value::Integer(2));
}

#[test]
fn test_operator_precedence() {
    assert_eq!(eval_source("5 + 2 * 10;").unwrap(), Value::Integer(25));
    assert_eq!(eval_source("(5 + 2) * 10;").unwrap(), Value::Integer(70));
}

#[test]
fn test_comparison_expressions() {
    assert_eq!(eval_source("10 > 5;").unwrap(), Value::Boolean(true));
    assert_eq!(eval_source("10 < 5;").unwrap(), Value::Boolean(false));
    assert_eq!(eval_source("10 == 10;").unwrap(), Value::Boolean(true));
    assert_eq!(eval_source("10 != 5;").unwrap(), Value::Boolean(true));
}

#[test]
fn test_division_by_zero_error() {
    expect_runtime_error("10 / 0;");
}

// --- VARIABLE & SCOPING TESTS ---

#[test]
fn test_variable_assignment_and_retrieval() {
    let source = "x = 42; x;";
    assert_eq!(eval_source(source).unwrap(), Value::Integer(42));
}

// --- CONTROL FLOW TESTS ---

#[test]
fn test_if_statement() {
    assert_eq!(eval_source("if true { 1; }").unwrap(), Value::Integer(1));
    assert_eq!(eval_source("if false { 1; }"), None);
}

#[test]
fn test_if_else_statement() {
    assert_eq!(
        eval_source("if true { 1; } else { 2; }").unwrap(),
        Value::Integer(1)
    );
    assert_eq!(
        eval_source("if false { 1; } else { 2; }").unwrap(),
        Value::Integer(2)
    );
}

#[test]
fn test_if_as_expression() {
    let source = "x = if 1 > 0 { 100; } else { 200; }; x;";
    assert_eq!(eval_source(source).unwrap(), Value::Integer(100));
}

// --- FUNCTION AND CLOSURE TESTS ---

#[test]
fn test_basic_function_invocation() {
    let source = r#"
        func multiply(a, b) {
            return a * b;
        }
        multiply(3, 4);
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(12));
}

#[test]
fn test_implicit_return_from_function() {
    let source = r#"
        func add(a, b) {
            a + b;
        }
        add(7, 8);
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(15));
}

#[test]
fn test_recursive_function_factorial() {
    let source = r#"
        func factorial(n) {
            if n == 0 {
                return 1;
            }
            return n * factorial(n - 1);
        }
        factorial(5);
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(120));
}

#[test]
fn test_lambda_evaluation() {
    let source = r"(\x. x + 1)(5);";
    assert_eq!(eval_source(source).unwrap(), Value::Integer(6));
}

#[test]
fn test_closure_captures_environment() {
    let source = r#"
        x = 10;
        f = \y. x + y;
        f(5);
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(15));
}

#[test]
fn test_nested_lambdas_and_currying() {
    let source = r#"
        add_x = \x. \y. x + y;
        add_five = add_x(5);
        add_five(3);
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(8));
}

// --- DATA STRUCTURE TESTS ---

#[test]
fn test_struct_instantiation_and_access() {
    let source = r#"
        Point : struct { x: int, y: int };
        p = Point { x: 1, y: 2 };
        p.x;
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(1));
}

#[test]
fn test_struct_field_modification() {
    let source = r#"
        Point : struct { x: int, y: int };
        p = Point { x: 1, y: 2 };
        p.y = 99;
        p.y;
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(99));
}

#[test]
fn test_function_with_struct_arg() {
    let source = r#"
        Point : struct { x: int, y: int };
        func get_x(p) {
            return p.x;
        }
        p = Point { x: 10, y: 20 };
        get_x(p);
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(10));
}

#[test]
fn test_enum_variant_creation() {
    let source = r#"
        Color : enum { Red, Green, Blue };
        c = Color::Red;
        c;
    "#;
    let result = eval_source(source).unwrap();
    // **FIXED**: Correctly destructure the wrapped EnumVariant struct
    if let Value::EnumVariant(variant) = result {
        assert_eq!(variant.enum_name, "Color");
        assert_eq!(variant.variant_name, "Red");
    } else {
        panic!("Expected an enum variant, got {:?}", result);
    }
}

#[test]
fn test_struct_with_enum_field() {
    let source = r#"
        Status : enum { Active, Inactive };
        User : struct { name: str, status: Status };
        u = User { name: "Alice", status: Status::Active };
        u.status;
    "#;
    let result = eval_source(source).unwrap();
    // **FIXED**: Correctly destructure the wrapped EnumVariant struct
    if let Value::EnumVariant(variant) = result {
        assert_eq!(variant.enum_name, "Status");
        assert_eq!(variant.variant_name, "Active");
    } else {
        panic!("Expected an enum variant, got {:?}", result);
    }
}

// --- LIST TESTS (assuming list support is partial) ---

#[test]
fn test_list_creation_and_access() {
    let source = r#"
        my_list = [10, 20, 30];
        my_list[1];
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(20));
}

#[test]
fn test_list_access_out_of_bounds() {
    let source = r#"
        my_list = [10, 20, 30];
        my_list[99];
    "#;
    expect_runtime_error(source);
}

#[test]
#[ignore] // Enable when list modification is implemented
fn test_list_modification() {
    let source = r#"
        my_list = [10, 20, 30];
        my_list[1] = 25;
        my_list[1];
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(25));
}

// --- FUTURE FEATURE TESTS (IGNORED) ---

#[test]
#[ignore]
fn test_match_statement_with_enums() {
    let source = r#"
        Status : enum { Active, Inactive };
        s = Status::Active;
        result = match s {
            Status::Active => 1,
            Status::Inactive => 0
        };
        result;
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(1));
}

#[test]
#[ignore]
fn test_while_loop() {
    let source = r#"
        x = 5;
        y = 0;
        while x > 0 {
            y = y + x;
            x = x - 1;
        }
        y; // 5 + 4 + 3 + 2 + 1 = 15
    "#;
    assert_eq!(eval_source(source).unwrap(), Value::Integer(15));
}
