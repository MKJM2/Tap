use tap_lang::ast::{
    EnumDecl, Expression, LiteralValue, Operator, Statement, StructDecl, TypeAnnotation,
};
use tap_lang::lexer::Lexer;
use tap_lang::parser::Parser;

#[test]
fn test_parse_lambda_expression() {
    let source = "\\x. x + 1;";
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap();

    assert_eq!(program.statements.len(), 1);

    let stmt = &program.statements[0];
    match stmt {
        Statement::Expression(Expression::Lambda { args, body }) => {
            assert_eq!(args, &["x"]);
            match &**body {
                Expression::Binary { left, op, right } => {
                    assert_eq!(op, &Operator::Add);
                    assert_eq!(**left, Expression::Identifier("x".to_string()));
                    assert_eq!(**right, Expression::Literal(LiteralValue::Integer(1)));
                }
                _ => panic!("Expected binary expression in lambda body"),
            }
        }
        _ => panic!("Expected lambda expression"),
    }
}

#[test]
fn test_parse_struct_declaration() {
    let source = "Point : struct { x: int, y: int };";
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap();

    assert_eq!(program.statements.len(), 1);

    let stmt = &program.statements[0];
    match stmt {
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
    let source = "Color : enum { Red, Green, Blue };";
    let tokens = Lexer::new(source).tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program().unwrap();

    assert_eq!(program.statements.len(), 1);

    let stmt = &program.statements[0];
    match stmt {
        Statement::EnumDecl(EnumDecl { name, variants }) => {
            assert_eq!(name, "Color");
            assert_eq!(variants.len(), 3);
            assert_eq!(variants[0], "Red");
            assert_eq!(variants[1], "Green");
            assert_eq!(variants[2], "Blue");
        }
        _ => panic!("Expected enum declaration"),
    }
}
