use tap_lang::interpreter::{Interpreter, Value};
use tap_lang::environment::Environment;
use tap_lang::lexer::Lexer;
use tap_lang::parser::Parser;

fn interpret_source(source: &str) -> Result<Value, tap_lang::interpreter::RuntimeError> {
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap();
    let interpreter = Interpreter::new();
    let mut env = Environment::new();
    interpreter.interpret(&program, &mut env)
}

#[test]
fn test_lambda_evaluation() {
    let source = "(|x| x + 1)(5);";
    let result = interpret_source(source).unwrap();
    assert_eq!(result, Value::Integer(6));
}

#[test]
fn test_struct_instantiation() {
    let source = "
        struct Point { x: int, y: int };
        p = Point { x: 1, y: 2 };
        p.x;
    ";
    let result = interpret_source(source).unwrap();
    assert_eq!(result, Value::Integer(1));
}
