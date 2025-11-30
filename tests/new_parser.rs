// This file will contain tests for the new parser based on the updated grammar and AST.

use tap::ast::{
    Program,
    Span,
    TopStatement,
    Expression,
    LiteralValue,
    PrimaryExpression,
    BinaryExpression,
    BinaryOperator,
};
use tap::lexer::Lexer;
use tap::parser::Parser;
use tap::diagnostics::Reporter;

// --- TEST HELPER ---
// This helper function reduces boilerplate in all tests.
// It handles lexing and parsing, and provides a rich error report if parsing fails.
fn parse_test_source(source: &str) -> Program {
    let mut reporter = Reporter::new();
    let tokens = Lexer::new(source, &mut reporter)
        .tokenize()
        .unwrap_or_else(|_| panic!("Lexing failed for source: {}", source));

    let mut parser = Parser::new(&tokens, &mut reporter);
    let program = parser.parse_program().unwrap_or_else(|e| {
        panic!(
            "Parsing failed for source: \"{}\"\n\nError Report:\n{:?}",
            source.trim(),
            e
        )
    });

    if reporter.has_errors() {
        panic!(
            "Parsing failed for source: \"{}\"\n\nReporter Errors:\n{:?}",
            source.trim(),
            reporter.diagnostics
        );
    }

    program
}

#[test]
fn test_parse_simple_expression_statement() {
    let source = "1 + 2;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    
    let expected_span = Span::new(0, 6); // "1 + 2;"

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            assert_eq!(expr_stmt.span, expected_span);
            match &expr_stmt.expression {
                Expression::Binary(BinaryExpression { left, operator, right, span }) => {
                    assert_eq!(*span, Span::new(0, 5)); // "1 + 2"
                    match &**left {
                        Expression::Primary(PrimaryExpression::Literal(LiteralValue::Integer(val), lit_span)) => {
                            assert_eq!(*val, 1);
                            assert_eq!(*lit_span, Span::new(0, 1));
                        },
                        _ => panic!("Expected integer literal for left operand"),
                    }
                    assert_eq!(*operator, BinaryOperator::Add);
                    match &**right {
                        Expression::Primary(PrimaryExpression::Literal(LiteralValue::Integer(val), lit_span)) => {
                            assert_eq!(*val, 2);
                            assert_eq!(*lit_span, Span::new(4, 5));
                        },
                        _ => panic!("Expected integer literal for right operand"),
                    }
                },
                _ => panic!("Expected binary expression"),
            }
        },
        _ => panic!("Expected an expression statement"),
    }
}

#[test]
fn test_parse_function_definition() {
    let source = "fn my_function() = { 1 + 2; };";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the function definition
}

#[test]
fn test_parse_function_definition_with_parameters() {
    let source = "fn add(a: Int, b: Int): Int = { a + b; };";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the function definition with parameters
}

#[test]
fn test_parse_function_definition_with_return_type() {
    let source = "fn get_answer(question: String): Int = { 42; };";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the function definition with a return type
}

#[test]
fn test_parse_struct_definition() {
    let source = "type EmptyStruct = {};";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the struct definition
}

#[test]
fn test_parse_struct_definition_with_fields() {
    let source = "type Point = { x: Int, y: Int };";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the struct definition with fields
}

#[test]
fn test_parse_enum_definition() {
    let source = "type Color = Red | Green | Blue;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the enum definition
}

#[test]
fn test_parse_enum_definition_with_variants() {
    let source = "type MaybeInt = Some(Int) | None;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);
    // TODO: Add assertions to check the structure of the enum definition with variants
}