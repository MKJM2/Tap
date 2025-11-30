// This file will contain tests for the new parser based on the updated grammar and AST.

use tap::ast::{
    BinaryExpression, BinaryOperator, Expression, ExpressionOrBlock, LetStatement, LiteralValue,
    Pattern, PrimaryExpression, Program, Span, TopStatement, Type, TypeConstructor, TypePrimary,
};
use tap::diagnostics::Reporter;
use tap::lexer::Lexer;
use tap::parser::Parser;

// --- TEST HELPER ---

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
                Expression::Binary(BinaryExpression {
                    left,
                    operator,
                    right,
                    span,
                }) => {
                    assert_eq!(*span, Span::new(0, 5)); // "1 + 2"

                    match &**left {
                        Expression::Primary(PrimaryExpression::Literal(
                            LiteralValue::Integer(val),
                            lit_span,
                        )) => {
                            assert_eq!(*val, 1);
                            assert_eq!(*lit_span, Span::new(0, 1));
                        }
                        _ => panic!("Expected integer literal for left operand"),
                    }

                    assert_eq!(*operator, BinaryOperator::Add);

                    match &**right {
                        Expression::Primary(PrimaryExpression::Literal(
                            LiteralValue::Integer(val),
                            lit_span,
                        )) => {
                            assert_eq!(*val, 2);
                            assert_eq!(*lit_span, Span::new(4, 5));
                        }
                        _ => panic!("Expected integer literal for right operand"),
                    }
                }
                _ => panic!("Expected binary expression"),
            }
        }
        _ => panic!("Expected an expression statement"),
    }
}

#[test]
fn test_parse_function_definition() {
    // Note: No semicolon required for function definitions
    let source = "my_function(): int = { 1 + 2 }";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Function(func_binding)) => {
            assert_eq!(func_binding.name, "my_function");
            assert!(func_binding.params.is_empty());

            if let Type::Primary(TypePrimary::Named(name, _)) = &func_binding.return_type {
                assert_eq!(name, "int");
            } else {
                panic!("Expected named type for return type");
            }

            if let Some(expr) = &func_binding.body.final_expression {
                if let Expression::Binary(BinaryExpression { left, .. }) = &**expr {
                    if let Expression::Primary(PrimaryExpression::Literal(
                        LiteralValue::Integer(val),
                        _,
                    )) = &**left
                    {
                        assert_eq!(*val, 1);
                    }
                }
            } else {
                panic!("Expected final expression in function body");
            }
        }
        _ => panic!("Expected a function definition statement"),
    }
}

#[test]
fn test_parse_function_definition_with_parameters() {
    let source = "add(a: int, b: int): int = { a + b }";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Function(func_binding)) => {
            assert_eq!(func_binding.name, "add");
            assert_eq!(func_binding.params.len(), 2);

            assert_eq!(func_binding.params[0].name, "a");
            if let Type::Primary(TypePrimary::Named(name, _)) = &func_binding.params[0].ty {
                assert_eq!(name, "int");
            } else {
                panic!("Expected named type for parameter a");
            }

            assert_eq!(func_binding.params[1].name, "b");
            if let Type::Primary(TypePrimary::Named(name, _)) = &func_binding.params[1].ty {
                assert_eq!(name, "int");
            } else {
                panic!("Expected named type for parameter b");
            }

            if let Type::Primary(TypePrimary::Named(name, _)) = &func_binding.return_type {
                assert_eq!(name, "int");
            } else {
                panic!("Expected named type for return type");
            }
        }
        _ => panic!("Expected a function definition statement"),
    }
}

#[test]
fn test_parse_variable_bindings() {
    let source = "
    x: int = 5;
    mut y = 10;
    name = \"Alice\";
    ";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 3);

    // 1. x: int = 5;
    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "x");
            assert_eq!(bind.mutable, false);
            assert!(bind.type_annotation.is_some());
        }
        _ => panic!("Expected variable binding for x"),
    }

    // 2. mut y = 10;
    match &program.statements[1] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "y");
            assert_eq!(bind.mutable, true);
            assert!(bind.type_annotation.is_none());
        }
        _ => panic!("Expected variable binding for y"),
    }

    // 3. name = "Alice";
    match &program.statements[2] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "name");
            assert_eq!(bind.mutable, false);
        }
        _ => panic!("Expected variable binding for name"),
    }
}

#[test]
fn test_parse_struct_definition() {
    let source = "type EmptyStruct = {};";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "EmptyStruct");
            match &decl.constructor {
                TypeConstructor::Record(record_type) => {
                    assert!(record_type.fields.is_empty());
                }
                _ => panic!("Expected record constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_struct_definition_with_fields() {
    let source = "type Point = { x: int, y: int };";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "Point");
            match &decl.constructor {
                TypeConstructor::Record(record_type) => {
                    assert_eq!(record_type.fields.len(), 2);
                    assert_eq!(record_type.fields[0].name, "x");
                    assert_eq!(record_type.fields[1].name, "y");
                }
                _ => panic!("Expected record constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_enum_definition() {
    let source = "type Color = Red | Green | Blue;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "Color");
            match &decl.constructor {
                TypeConstructor::Sum(sum_type) => {
                    assert_eq!(sum_type.variants.len(), 3);
                    assert_eq!(sum_type.variants[0].name, "Red");
                    assert!(sum_type.variants[0].ty.is_none());
                    assert_eq!(sum_type.variants[1].name, "Green");
                    assert_eq!(sum_type.variants[2].name, "Blue");
                }
                _ => panic!("Expected sum constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_enum_definition_with_variants() {
    let source = "type MaybeInt = Some(int) | None;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "MaybeInt");
            match &decl.constructor {
                TypeConstructor::Sum(sum_type) => {
                    assert_eq!(sum_type.variants.len(), 2);

                    // Some(int)
                    assert_eq!(sum_type.variants[0].name, "Some");
                    assert!(sum_type.variants[0].ty.is_some());

                    // None
                    assert_eq!(sum_type.variants[1].name, "None");
                    assert!(sum_type.variants[1].ty.is_none());
                }
                _ => panic!("Expected sum constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_control_flow_if() {
    let source = "
    val = if (x > 0) {
        true
    } else {
        false
    };
    ";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            match &bind.value {
                Expression::If(if_expr) => {
                    // Check condition
                    match &*if_expr.condition {
                        Expression::Binary(_) => {}
                        _ => panic!("Expected binary expression in condition"),
                    }
                    // Check then block
                    assert!(if_expr.then_branch.final_expression.is_some());
                    // Check else block
                    assert!(if_expr.else_branch.is_some());
                }
                _ => panic!("Expected if expression"),
            }
        }
        _ => panic!("Expected variable binding"),
    }
}

#[test]
fn test_parse_control_flow_while() {
    let source = "while (i < 10) { i += 1; }";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => match &expr_stmt.expression {
            Expression::While(while_expr) => {
                match &*while_expr.condition {
                    Expression::Binary(_) => {}
                    _ => panic!("Expected binary condition"),
                }
                assert!(!while_expr.body.statements.is_empty());
            }
            _ => panic!("Expected while expression"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_match_expression() {
    let source = "
    match (val) {
        | Some(x) => x,
        | None => 0,
        | _ => -1
    };
    ";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Match(match_expr) => {
                    assert_eq!(match_expr.arms.len(), 3);

                    // Check first arm: | Some(x) => x
                    let arm1 = &match_expr.arms[0];
                    match &arm1.pattern {
                        // Use struct variant syntax
                        Pattern::Variant { name, patterns, .. } => {
                            assert_eq!(name, "Some");
                            if let Some(pats) = patterns {
                                assert_eq!(pats.len(), 1);
                            } else {
                                panic!("Expected parameters for Some variant");
                            }
                        }
                        _ => panic!("Expected variant pattern"),
                    }

                    // Check third arm: | _ => -1
                    let arm3 = &match_expr.arms[2];
                    match &arm3.pattern {
                        Pattern::Wildcard(_) => {}
                        _ => panic!("Expected wildcard pattern"),
                    }
                }
                _ => panic!("Expected match expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_lists_and_indexing() {
    let source = "[1, 2, 3][0];";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Postfix(postfix) => {
                    // Check the primary: [1, 2, 3]
                    // postfix.primary is Box<Expression>
                    match &*postfix.primary {
                        Expression::Primary(primary) => {
                            match primary {
                                PrimaryExpression::List(list_lit) => {
                                    // ListLiteral contains `elements`
                                    assert_eq!(list_lit.elements.len(), 3);
                                }
                                _ => panic!("Expected List primary expression"),
                            }
                        }
                        _ => panic!("Expected Primary Expression inside Postfix"),
                    }

                    // Check the operation: [0]
                    assert_eq!(postfix.operators.len(), 1);
                }
                _ => panic!("Expected postfix expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_lambda() {
    let source = "f = (x: int) => x + 1;";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            match &bind.value {
                Expression::Lambda(lambda) => {
                    assert_eq!(lambda.params.len(), 1);
                    assert_eq!(lambda.params[0].name, "x");
                    // Body expression
                    match &lambda.body {
                        ExpressionOrBlock::Expression(expr) => {
                            if let Expression::Binary(_) = **expr {
                                // OK
                            } else {
                                panic!("Expected binary expression in lambda body");
                            }
                        }
                        _ => panic!("Expected expression body for lambda"),
                    }
                }
                _ => panic!("Expected lambda expression"),
            }
        }
        _ => panic!("Expected variable binding"),
    }
}

#[test]
fn test_operator_precedence() {
    // Multiplication should bind tighter than addition
    let source = "1 + 2 * 3;";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Binary(bin_expr) => {
                    // The top-level operation should be Add
                    assert_eq!(bin_expr.operator, BinaryOperator::Add);

                    // Left should be 1
                    match &*bin_expr.left {
                        Expression::Primary(PrimaryExpression::Literal(
                            LiteralValue::Integer(1),
                            _,
                        )) => {}
                        _ => panic!("Left side should be 1"),
                    }

                    // Right should be a Binary Expression (2 * 3)
                    match &*bin_expr.right {
                        Expression::Binary(inner_bin) => {
                            assert_eq!(inner_bin.operator, BinaryOperator::Multiply);
                        }
                        _ => panic!("Right side should be a multiplication expression"),
                    }
                }
                _ => panic!("Expected binary expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}
