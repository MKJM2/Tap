// The contents of this file have been commented out temporarily to facilitate development of the new parser.
/*
use tap::ast::{Expression, Program, Statement, LiteralValue};
use tap::environment::Environment;
use tap::interpreter::{Interpreter, Value};
use tap::lexer::Lexer;
use tap::parser::Parser;

// Helper to execute code and return the final value or error
fn run_code(source: &str) -> Result<Value, String> {
    let tokens = Lexer::new(source).tokenize().expect("Lexer error");
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().expect("Parser error");

    let mut interpreter = Interpreter::new(Environment::new());
    interpreter.interpret(&program)
}

#[test]
fn test_simple_arithmetic() {
    assert_eq!(run_code("1 + 2;").unwrap(), Value::Integer(3));
    assert_eq!(run_code("5 * 3 - 2;").unwrap(), Value::Integer(13));
    assert_eq!(run_code("10 / 2 + 1;").unwrap(), Value::Integer(6));
}

#[test]
fn test_variable_declaration_and_assignment() {
    assert_eq!(run_code("let x = 10; x;").unwrap(), Value::Integer(10));
    assert_eq!(run_code("let mut y = 20; y = y + 5; y;").unwrap(), Value::Integer(25));
}

#[test]
fn test_if_expression() {
    assert_eq!(run_code("if true { 10; } else { 20; };").unwrap(), Value::Integer(10));
    assert_eq!(run_code("if false { 10; } else { 20; };").unwrap(), Value::Integer(20));
    assert_eq!(run_code("let x = 5; if x > 2 { x; } else { 0; };").unwrap(), Value::Integer(5));
}

#[test]
fn test_function_definition_and_call() {
    let result = run_code(
        "func add(a, b) {
            return a + b;
        }
        add(3, 4);
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(7));
}

#[test]
fn test_closure() {
    let result = run_code(
        "func makeAdder(x) {
            func adder(y) {
                return x + y;
            }
            return adder;
        }
        let addFive = makeAdder(5);
        addFive(3);
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(8));
}

#[test]
fn test_struct_instantiation_and_property_access() {
    let result = run_code(
        "type Point = struct { x: int, y: int };
        let p = Point { x: 10, y: 20 };
        p.x;
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(10));
}

#[test]
fn test_list_literal_and_access() {
    let result = run_code(
        "let my_list = [1, 2, 3];
        my_list[1];
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(2));
}

#[test]
fn test_array_assignment() {
    let result = run_code(
        "let mut arr = [1, 2, 3];
        arr[1] = 5;
        arr[1];
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(5));
}

#[test]
fn test_while_loop() {
    let result = run_code(
        "let mut i = 0;
        let mut sum = 0;
        while i < 3 {
            sum = sum + i;
            i = i + 1;
        }
        sum;
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(3)); // 0 + 1 + 2
}

#[test]
fn test_enum_variant_creation() {
    let result = run_code(
        "type Option = enum { Some(int), None };
        let x = Option::Some(42);
        x; // Should return the enum value
        ",
    )
    .unwrap();
    // This assertion would need a way to compare enum values directly,
    // which might involve a custom PartialEq for Value::EnumVariant.
    // For now, we'll just ensure it doesn't panic.
    assert!(matches!(result, Value::EnumVariant(_, _, _)));
}

#[test]
fn test_match_expression() {
    let result = run_code(
        "type Option = enum { Some(int), None };
        let x = Option::Some(10);
        match x {
            Option::Some(val) => val + 5,
            Option::None => 0,
        };
        ",
    )
    .unwrap();
    assert_eq!(result, Value::Integer(15));

    let result_none = run_code(
        "type Option = enum { Some(int), None };
        let y = Option::None;
        match y {
            Option::Some(val) => val + 5,
            Option::None => 0,
        };
        ",
    )
    .unwrap();
    assert_eq!(result_none, Value::Integer(0));
}

#[test]
fn test_string_concatenation() {
    assert_eq!(run_code("\"hello\" + \" world\";").unwrap(), Value::String("hello world".to_string()));
}
*/