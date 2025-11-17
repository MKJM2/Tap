use tap_lang::ast::{
    EnumDecl, Expression, FunctionDef, LiteralValue, Operator, Program, Statement, StructDecl,
    TypeAnnotation,
};
use tap_lang::lexer::Lexer;
use tap_lang::parser::Parser;

// --- TEST HELPER ---
// This helper function reduces boilerplate in all tests.
// It handles lexing and parsing, and provides a rich error report if parsing fails.
fn parse_test_source(source: &str) -> Program {
    let tokens = Lexer::new(source)
        .tokenize()
        .unwrap_or_else(|_| panic!("Lexing failed for source: {}", source));

    let mut parser = Parser::new(&tokens);
    parser.parse_program().unwrap_or_else(|e| {
        panic!(
            "Parsing failed for source: \"{}\"\n\nError Report:\n{:?}",
            source.trim(),
            e
        )
    })
}

// --- EXPRESSION PARSING TESTS ---

#[test]
fn test_parse_simple_binary_expression() {
    let program = parse_test_source("1 + 2;");
    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::Binary { left, op, right }) => {
            assert_eq!(**left, Expression::Literal(LiteralValue::Integer(1)));
            assert_eq!(*op, Operator::Add);
            assert_eq!(**right, Expression::Literal(LiteralValue::Integer(2)));
        }
        _ => panic!("Expected a binary expression statement."),
    }
}

#[test]
fn test_parse_operator_precedence() {
    let program = parse_test_source("1 + 2 * 3;");
    let stmt = &program.statements[0];
    // Should parse as 1 + (2 * 3)
    match stmt {
        Statement::Expression(Expression::Binary { left, op, right }) => {
            assert_eq!(*op, Operator::Add);
            assert_eq!(**left, Expression::Literal(LiteralValue::Integer(1)));
            // The right side should be another binary expression
            match &**right {
                Expression::Binary {
                    left: inner_left,
                    op: inner_op,
                    right: inner_right,
                } => {
                    assert_eq!(**inner_left, Expression::Literal(LiteralValue::Integer(2)));
                    assert_eq!(*inner_op, Operator::Multiply);
                    assert_eq!(**inner_right, Expression::Literal(LiteralValue::Integer(3)));
                }
                _ => panic!("Expected nested binary expression for precedence."),
            }
        }
        _ => panic!("Expected a binary expression statement."),
    }
}

#[test]
fn test_parse_parenthesized_expression() {
    let program = parse_test_source("(1 + 2) * 3;");
    let stmt = &program.statements[0];
    // Should parse as (1 + 2) * 3
    match stmt {
        Statement::Expression(Expression::Binary { left, op, right }) => {
            assert_eq!(*op, Operator::Multiply);
            assert_eq!(**right, Expression::Literal(LiteralValue::Integer(3)));
            match &**left {
                Expression::Binary { .. } => { /* Correct */ }
                _ => panic!("Expected left side to be a binary expression."),
            }
        }
        _ => panic!("Expected a binary expression statement."),
    }
}

#[test]
fn test_parse_function_call() {
    let program = parse_test_source("my_function(1, \"hello\");");
    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::FunctionCall { callee, args }) => {
            assert_eq!(**callee, Expression::Identifier("my_function".to_string()));
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Expression::Literal(LiteralValue::Integer(1)));
            assert_eq!(
                args[1],
                Expression::Literal(LiteralValue::String("hello".to_string()))
            );
        }
        _ => panic!("Expected a function call expression."),
    }
}

#[test]
fn test_parse_nested_function_call() {
    let program = parse_test_source("func_a(func_b(1));");
    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::FunctionCall {
            callee,
            args: outer_args,
        }) => {
            assert_eq!(**callee, Expression::Identifier("func_a".to_string()));
            assert_eq!(outer_args.len(), 1);
            match &outer_args[0] {
                Expression::FunctionCall {
                    callee: inner_callee,
                    args: inner_args,
                } => {
                    assert_eq!(**inner_callee, Expression::Identifier("func_b".to_string()));
                    assert_eq!(inner_args.len(), 1);
                }
                _ => panic!("Expected nested function call."),
            }
        }
        _ => panic!("Expected a function call expression."),
    }
}

#[test]
fn test_parse_lambda_expression() {
    let program = parse_test_source("\\x. x + 1;");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Expression(Expression::Lambda { args, body }) => {
            assert_eq!(args, &["x"]);
            match &**body {
                Expression::Binary { .. } => { /* Correct */ }
                _ => panic!("Expected binary expression in lambda body"),
            }
        }
        _ => panic!("Expected lambda expression"),
    }
}

#[test]
fn test_parse_struct_instantiation() {
    let program = parse_test_source("Point { x: 10, y: 20 };");
    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::StructInstantiation { name, fields }) => {
            assert_eq!(name, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[0].1, Expression::Literal(LiteralValue::Integer(10)));
            assert_eq!(fields[1].0, "y");
            assert_eq!(fields[1].1, Expression::Literal(LiteralValue::Integer(20)));
        }
        _ => panic!("Expected struct instantiation expression."),
    }
}

#[test]
fn test_parse_property_access() {
    let program = parse_test_source("my_car.make;");
    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::Get { object, name }) => {
            assert_eq!(**object, Expression::Identifier("my_car".to_string()));
            assert_eq!(name, "make");
        }
        _ => panic!("Expected property access expression."),
    }
}

#[test]
fn test_parse_chained_property_access() {
    let program = parse_test_source("my_car.owner.name;");
    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::Get { object, name }) => {
            assert_eq!(name, "name");
            match &**object {
                Expression::Get {
                    object: inner_object,
                    name: inner_name,
                } => {
                    assert_eq!(**inner_object, Expression::Identifier("my_car".to_string()));
                    assert_eq!(inner_name, "owner");
                }
                _ => panic!("Expected nested Get expression."),
            }
        }
        _ => panic!("Expected property access expression."),
    }
}

// --- STATEMENT PARSING TESTS ---

#[test]
fn test_parse_if_statement() {
    let program = parse_test_source("if true { 1; }");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert_eq!(
                **condition,
                Expression::Literal(LiteralValue::Boolean(true))
            );
            assert_eq!(then_branch.len(), 1);
            assert!(else_branch.is_none());
        }
        _ => panic!("Expected an if statement."),
    }
}

#[test]
fn test_parse_if_else_statement() {
    let program = parse_test_source("if x < 10 { 1; } else { 2; }");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::If { else_branch, .. } => {
            assert!(else_branch.is_some());
            assert_eq!(else_branch.as_ref().unwrap().len(), 1);
        }
        _ => panic!("Expected an if-else statement."),
    }
}

#[test]
fn test_parse_return_statement() {
    let program = parse_test_source("return 42;");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Return(value) => {
            assert_eq!(*value, Expression::Literal(LiteralValue::Integer(42)));
        }
        _ => panic!("Expected a return statement."),
    }
}

#[test]
fn test_parse_variable_declaration_and_assignment() {
    let program = parse_test_source("x : int = 10;");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::Assignment { name, value } => {
            assert_eq!(name, "x");
            assert_eq!(*value, Expression::Literal(LiteralValue::Integer(10)));
        }
        _ => panic!("Expected an assignment statement, parser should handle this form."),
    }
}

#[test]
fn test_parse_function_definition() {
    let source = "func fib(n) { return n; }";
    let program = parse_test_source(source);
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::FunctionDef(FunctionDef { name, args, body }) => {
            assert_eq!(name, "fib");
            assert_eq!(args, &["n".to_string()]);
            assert_eq!(body.len(), 1);
        }
        _ => panic!("Expected a function definition."),
    }
}

// --- TYPE DECLARATION TESTS ---

#[test]
fn test_parse_struct_declaration() {
    let program = parse_test_source("Point : struct { x: int, y: int };");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::StructDecl(StructDecl { name, fields }) => {
            assert_eq!(name, "Point");
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0], ("x".to_string(), TypeAnnotation::Int));
            assert_eq!(fields[1], ("y".to_string(), TypeAnnotation::Int));
        }
        _ => panic!("Expected struct declaration"),
    }
}

#[test]
fn test_parse_enum_declaration() {
    let program = parse_test_source("Color : enum { Red, Green, Blue };");
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::EnumDecl(EnumDecl { name, variants }) => {
            assert_eq!(name, "Color");
            assert_eq!(variants, &["Red", "Green", "Blue"]);
        }
        _ => panic!("Expected enum declaration"),
    }
}

// NOTE: The current parser only supports simple enum variants (identifiers).
// This test is a placeholder for when it supports variants with associated data.
#[test]
#[ignore]
fn test_parse_enum_declaration_with_data() {
    let program = parse_test_source("Option : enum { Some(int), None };");
    assert_eq!(program.statements.len(), 1);
    // Add assertions here when the parser is updated.
}

// --- FUTURE FEATURE TESTS (IGNORED) ---

#[test]
#[ignore]
fn test_parse_while_loop() {
    let program = parse_test_source("while x > 0 { x = x - 1; }");
    assert_eq!(program.statements.len(), 1);
    // Add assertions here when the parser is updated.
}

#[test]
#[ignore]
fn test_parse_for_loop() {
    let program = parse_test_source("for i in [1, 2, 3] { print(i); }");
    assert_eq!(program.statements.len(), 1);
    // Add assertions here when the parser is updated.
}

#[test]
#[ignore]
fn test_parse_match_statement() {
    let source = "match opt { Some(x) => x + 1, None => 0 };";
    let program = parse_test_source(source);
    assert_eq!(program.statements.len(), 1);
    // Add assertions here when the parser is updated.
}
