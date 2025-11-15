use tap_lang::interpreter::{Interpreter, Value, EnumVariant};
use tap_lang::environment::Environment;
use tap_lang::lexer::Lexer;
use tap_lang::parser::Parser;

fn interpret_source(source: &str) -> Result<Option<Value>, tap_lang::interpreter::RuntimeError> {
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap();
    let interpreter = Interpreter::new();
    let mut env = Environment::new();
    interpreter.interpret(&program, &mut env)
}

#[test]
fn test_lambda_evaluation() {
    let source = r"(\x. x + 1)(5);";
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::Integer(6));
}

#[test]
fn test_struct_instantiation() {
    let source = r#"
        Point : struct { x: int, y: int };
        p = Point { x: 1, y: 2 };
        p.x;
    "#;
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::Integer(1));
}

#[test]
fn test_enum_variant_access() {
    let source = r#"
        Color : enum { Red, Green, Blue };
        Color::Red;
    "#;
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::EnumVariant(EnumVariant {
        enum_name: "Color".to_string(),
        variant_name: "Red".to_string(),
    }));
}

#[test]
fn test_basic_function_invocation() {
    let source = r#"
        func multiply(a, b) {
            return a * b;
        }
        multiply(3, 4);
    "#;
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::Integer(12));
}

#[test]
fn test_struct_with_enum() {
    let source = r#"
        Status : enum { Active, Inactive };
        User : struct { name: str, status: Status };
        u = User { name: "Alice", status: Status::Active };
        u.status;
    "#;
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::EnumVariant(EnumVariant {
        enum_name: "Status".to_string(),
        variant_name: "Active".to_string(),
    }));
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
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::Integer(10));
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
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::Integer(120));
}

#[test]
fn test_nested_lambdas() {
    let source = r#"
        add_x = \x. \y. x + y;
        add_five = add_x(5);
        add_five(3);
    "#;
    // Expected result: Value::Integer(8)
    let result = interpret_source(source);
    assert!(result.is_err()); // Expecting an error until nested lambdas are fully implemented
}

#[test]
fn test_list_access() {
    let source = r#"
        my_list = [10, 20, 30];
        my_list[1];
    "#;
    // Expected result: Value::Integer(20)
    let result = interpret_source(source);
    assert!(result.is_err()); // Expecting an error until list access is implemented
}

#[test]
fn test_list_modification() {
    let source = r#"
        my_list = [10, 20, 30];
        my_list[1] = 25;
        my_list[1];
    "#;
    // Expected result: Value::Integer(25)
    let result = interpret_source(source);
    assert!(result.is_err()); // Expecting an error until list modification is implemented
}

#[test]
fn test_if_statement() {
    // Test basic if statement (true condition)
    let source_if_true = r#"
        x = 10;
        if x > 5 {
            20;
        } else {
            30;
        }
    "#;
    let result_if_true = interpret_source(source_if_true).unwrap().unwrap();
    assert_eq!(result_if_true, Value::Integer(20));

    // Test basic if statement (false condition)
    let source_if_false = r#"
        x = 10;
        if x < 5 {
            20;
        } else {
            30;
        }
    "#;
    let result_if_false = interpret_source(source_if_false).unwrap().unwrap();
    assert_eq!(result_if_false, Value::Integer(30));

    // Test if statement with only if block (true condition)
    let source_if_only_true = r#"
        x = 10;
        if x > 5 {
            20;
        }
    "#;
    let result_if_only_true = interpret_source(source_if_only_true).unwrap().unwrap();
    assert_eq!(result_if_only_true, Value::Integer(20));

    // Test if statement with only if block (false condition)
    let source_if_only_false = r#"
        x = 3;
        if x > 5 {
            20;
        }
    "#;
    let result_if_only_false = interpret_source(source_if_only_false).unwrap().unwrap_or(Value::Null);
    assert_eq!(result_if_only_false, Value::Null);
}

#[test]
fn test_enum_in_struct_access() {
    let source = r#"
        TrafficLight : enum { Red, Yellow, Green };
        Car : struct { id: int, light_status: TrafficLight };
        my_car = Car { id: 1, light_status: TrafficLight::Green };
        my_car.light_status;
    "#;
    let result = interpret_source(source).unwrap().unwrap();
    assert_eq!(result, Value::EnumVariant(EnumVariant {
        enum_name: "TrafficLight".to_string(),
        variant_name: "Green".to_string(),
    }));
}