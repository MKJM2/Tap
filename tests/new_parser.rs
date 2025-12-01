// This file will contain tests for the new parser based on the updated grammar and AST.

use tap::ast::*;
use tap::diagnostics::Reporter;
use tap::lexer::Lexer;
use tap::parser::Parser;

// --- TEST HELPER ---

fn assert_parses(source: &str) -> Program {
    let mut reporter = Reporter::new();
    let tokens = match Lexer::new(source, &mut reporter).tokenize() {
        Ok(tokens) => tokens,
        Err(_) => {
            panic!(
                "Lexing failed for source: \"{}\"\n\nLexer Errors:\n{:?}",
                source.trim(),
                reporter.diagnostics
            );
        }
    };

    let mut parser = Parser::new(&tokens, &mut reporter);
    match parser.parse_program() {
        Ok(program) => {
            if reporter.has_errors() {
                panic!(
                    "Parsing failed with reporter errors for source: \"{}\"\n\nTokens:\n{:?}\n\nParser Errors:\n{:?}",
                    source.trim(),
                    tokens,
                    reporter.diagnostics
                );
            }
            program
        }
        Err(e) => {
            panic!(
                "Parsing failed for source: \"{}\"\n\nTokens:\n{:?}\n\nError Report:\n{:?}",
                source.trim(),
                tokens,
                e
            );
        }
    }
}

fn parse_test_source(source: &str) -> Program {
    assert_parses(source)
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
fn test_parse_sum_type_with_payloads() {
    // Tests: <identifier> "(" <type> ")"
    let source = "type Result = Ok(int) | Err(string);";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "Result");
            match &decl.constructor {
                TypeConstructor::Sum(sum_type) => {
                    assert_eq!(sum_type.variants.len(), 2);

                    // Check "Ok(int)"
                    assert_eq!(sum_type.variants[0].name, "Ok");
                    assert!(sum_type.variants[0].ty.is_some());
                    // Verify the inner type is int (assuming you have a Type enum)
                    // matches!(sum_type.variants[0].ty, Some(Type::Primary(name)) if name == "int")

                    // Check "Err(string)"
                    assert_eq!(sum_type.variants[1].name, "Err");
                    assert!(sum_type.variants[1].ty.is_some());
                }
                _ => panic!("Expected sum constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_sum_type_with_nested_record() {
    // Tests: <variant> holding a <record_type>
    // type Action = Move({x: int, y: int}) | Quit;
    let source = "type Action = Move({x: int, y: int}) | Quit;";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            match &decl.constructor {
                TypeConstructor::Sum(sum_type) => {
                    let move_variant = &sum_type.variants[0];
                    assert_eq!(move_variant.name, "Move");

                    // Verify the payload is a Record Type
                    match &move_variant.ty {
                        Some(Type::Primary(TypePrimary::Record(record_type))) => {
                            assert_eq!(record_type.fields.len(), 2);
                            assert_eq!(record_type.fields[0].name, "x");
                            assert_eq!(record_type.fields[1].name, "y");
                        }
                        _ => panic!("Expected Record type inside Move variant"),
                    }
                }
                _ => panic!("Expected sum constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_mixed_sum_type() {
    // Tests mixing: <identifier> | <identifier> "(" <type> ")"
    let source = "type OptionInt = Some(int) | None;";
    let program = parse_test_source(source);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "OptionInt");
            match &decl.constructor {
                TypeConstructor::Sum(sum_type) => {
                    assert_eq!(sum_type.variants.len(), 2);

                    // Some(int)
                    assert_eq!(sum_type.variants[0].name, "Some");
                    assert!(sum_type.variants[0].ty.is_some());

                    // None
                    assert_eq!(sum_type.variants[1].name, "None");
                    assert!(sum_type.variants[1].ty.is_none()); // Should be None
                }
                _ => panic!("Expected sum constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_simple_enum_definition() {
    let source = "type A = B;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "A");
            match &decl.constructor {
                TypeConstructor::Sum(sum_type) => {
                    assert_eq!(sum_type.variants.len(), 1);
                    assert_eq!(sum_type.variants[0].name, "B");
                    assert!(sum_type.variants[0].ty.is_none());
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
fn test_parse_control_flow_simple_if() {
    let source = "
    if (x > 0) {
        true
    };
    ";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::If(if_expr) => {
                    // Check condition
                    match &*if_expr.condition {
                        Expression::Binary(_) => {}
                        _ => panic!("Expected binary expression in condition"),
                    }
                    // Check then block
                    assert!(if_expr.then_branch.final_expression.is_some());
                    // Ensure else block is absent
                    assert!(if_expr.else_branch.is_none());
                }
                _ => panic!("Expected if expression"),
            }
        }
        _ => panic!("Expected expression statement"),
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
fn test_parse_for_expression() {
    let source = "mut sum = 0; for i in [1, 2, 3] { sum += i; }; sum;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 3);

    match &program.statements[1] {
        TopStatement::Expression(expr_stmt) => match &expr_stmt.expression {
            Expression::For(for_expr) => {
                match &for_expr.pattern {
                    Pattern::Identifier(name, _) => assert_eq!(name, "i"),
                    _ => panic!("Expected identifier pattern for iterator"),
                }
                match &*for_expr.iterable {
                    Expression::Primary(PrimaryExpression::List(list_lit)) => {
                        assert_eq!(list_lit.elements.len(), 3);
                    }
                    _ => panic!("Expected list literal for iterable"),
                }
                assert_eq!(for_expr.body.statements.len(), 1);
            }
            _ => panic!("Expected for expression"),
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

#[test]
fn test_parse_complex_struct_definition() {
    let source = "
    type User = {
        id: int,
        username: string,
        is_active: bool,
    };
    ";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::TypeDecl(decl) => {
            assert_eq!(decl.name, "User");
            match &decl.constructor {
                TypeConstructor::Record(record_type) => {
                    assert_eq!(record_type.fields.len(), 3);
                    assert_eq!(record_type.fields[0].name, "id");
                    if let Type::Primary(TypePrimary::Named(name, _)) = &record_type.fields[0].ty {
                        assert_eq!(name, "int");
                    } else {
                        panic!("Expected named type for field 'id'");
                    }
                    assert_eq!(record_type.fields[1].name, "username");
                    if let Type::Primary(TypePrimary::Named(name, _)) = &record_type.fields[1].ty {
                        assert_eq!(name, "string");
                    } else {
                        panic!("Expected named type for field 'username'");
                    }
                    assert_eq!(record_type.fields[2].name, "is_active");
                    if let Type::Primary(TypePrimary::Named(name, _)) = &record_type.fields[2].ty {
                        assert_eq!(name, "bool");
                    } else {
                        panic!("Expected named type for field 'is_active'");
                    }
                }
                _ => panic!("Expected record constructor"),
            }
        }
        _ => panic!("Expected type declaration"),
    }
}

#[test]
fn test_parse_block_expression() {
    let source = "
    x = {
        a = 1;
        b = 2;
        a + b
    };
    ";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "x");
            match &bind.value {
                Expression::Block(block) => {
                    assert_eq!(block.statements.len(), 2);
                    assert!(block.final_expression.is_some());
                }
                _ => panic!("Expected block expression"),
            }
        }
        _ => panic!("Expected variable binding"),
    }
}

#[test]
fn test_parse_nested_if_expression() {
    let source = "
    result = if (x > 0) {
        if (y > 0) {
            1
        } else {
            -1
        }
    } else {
        0
    };
    ";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "result");
            match &bind.value {
                Expression::If(if_expr) => {
                    assert!(if_expr.else_branch.is_some());
                    match &if_expr.then_branch.final_expression {
                        Some(expr) => match &**expr {
                            Expression::If(_) => {
                                // Nested if expression is present.
                            }
                            _ => panic!("Expected nested if expression"),
                        },
                        None => panic!("Expected final expression in then branch"),
                    }
                }
                _ => panic!("Expected if expression"),
            }
        }
        _ => panic!("Expected variable binding"),
    }
}

#[test]
fn test_parse_function_call_with_arguments() {
    let source = "add(1, 2);";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Postfix(postfix) => {
                    // Check the primary: add
                    match &*postfix.primary {
                        Expression::Primary(PrimaryExpression::Identifier(ident, _)) => {
                            assert_eq!(ident, "add");
                        }
                        _ => panic!("Expected identifier 'add' for primary expression"),
                    }

                    assert_eq!(postfix.operators.len(), 1);
                    match &postfix.operators[0] {
                        PostfixOperator::Call { args, .. } => {
                            assert_eq!(args.len(), 2);
                            match &args[0] {
                                Expression::Primary(PrimaryExpression::Literal(
                                    LiteralValue::Integer(val),
                                    _,
                                )) => {
                                    assert_eq!(*val, 1);
                                }
                                _ => panic!("Expected integer literal '1' for first argument"),
                            }
                            match &args[1] {
                                Expression::Primary(PrimaryExpression::Literal(
                                    LiteralValue::Integer(val),
                                    _,
                                )) => {
                                    assert_eq!(*val, 2);
                                }
                                _ => panic!("Expected integer literal '2' for second argument"),
                            }
                        }
                        _ => panic!("Expected Call postfix operator"),
                    }
                }
                _ => panic!("Expected postfix expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_unary_expression() {
    let source = "-1;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => match &expr_stmt.expression {
            Expression::Unary(unary_expr) => {
                assert_eq!(unary_expr.operator, UnaryOperator::Minus);
                match &*unary_expr.right {
                    Expression::Primary(PrimaryExpression::Literal(
                        LiteralValue::Integer(val),
                        _,
                    )) => {
                        assert_eq!(*val, 1);
                    }
                    _ => panic!("Expected integer literal '1' for unary expression"),
                }
            }
            _ => panic!("Expected unary expression"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_parenthesized_expression() {
    let source = "(1 + 2) * 3;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Binary(bin_expr) => {
                    assert_eq!(bin_expr.operator, tap::ast::BinaryOperator::Multiply);
                    match &*bin_expr.left {
                        Expression::Primary(PrimaryExpression::Parenthesized(expr, _)) => {
                            match &**expr {
                                Expression::Binary(_) => {
                                    // Correctly parsed as a binary expression inside parentheses.
                                }
                                _ => panic!("Expected binary expression inside parentheses"),
                            }
                        }
                        _ => panic!("Expected parenthesized expression on the left side"),
                    }
                }
                _ => panic!("Expected binary expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_boolean_expression() {
    let source = "true == false;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => match &expr_stmt.expression {
            Expression::Binary(bin_expr) => {
                assert_eq!(bin_expr.operator, tap::ast::BinaryOperator::Equal);
                match &*bin_expr.left {
                    Expression::Primary(PrimaryExpression::Literal(
                        LiteralValue::Boolean(val),
                        _,
                    )) => {
                        assert_eq!(*val, true);
                    }
                    _ => panic!("Expected boolean literal 'true' on the left side"),
                }
                match &*bin_expr.right {
                    Expression::Primary(PrimaryExpression::Literal(
                        LiteralValue::Boolean(val),
                        _,
                    )) => {
                        assert_eq!(*val, false);
                    }
                    _ => panic!("Expected boolean literal 'false' on the right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_string_literal_expression() {
    let source = "\"hello world\";";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => match &expr_stmt.expression {
            Expression::Primary(PrimaryExpression::Literal(LiteralValue::String(val), _)) => {
                assert_eq!(*val, "hello world");
            }
            _ => panic!("Expected string literal expression"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_float_literal_expression() {
    let source = "3.14;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => match &expr_stmt.expression {
            Expression::Primary(PrimaryExpression::Literal(LiteralValue::Float(val), _)) => {
                assert_eq!(*val, 3.14);
            }
            _ => panic!("Expected float literal expression"),
        },
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_none_literal_expression() {
    let source = "None;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Primary(PrimaryExpression::Literal(LiteralValue::None, _)) => {
                    // Successfully parsed None literal
                }
                _ => panic!("Expected None literal expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_record_literal() {
    let source = "point = { x: 1, y: 2 };";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "point");
            match &bind.value {
                Expression::Primary(PrimaryExpression::Record(record_lit)) => {
                    assert_eq!(record_lit.fields.len(), 2);
                    assert_eq!(record_lit.fields[0].name, "x");
                    match &record_lit.fields[0].value {
                        Expression::Primary(PrimaryExpression::Literal(
                            LiteralValue::Integer(val),
                            _,
                        )) => {
                            assert_eq!(*val, 1);
                        }
                        _ => panic!("Expected integer literal for field 'x'"),
                    }
                    assert_eq!(record_lit.fields[1].name, "y");
                    match &record_lit.fields[1].value {
                        Expression::Primary(PrimaryExpression::Literal(
                            LiteralValue::Integer(val),
                            _,
                        )) => {
                            assert_eq!(*val, 2);
                        }
                        _ => panic!("Expected integer literal for field 'y'"),
                    }
                }
                _ => panic!("Expected record literal expression"),
            }
        }
        _ => panic!("Expected variable binding"),
    }
}

#[test]
fn test_parse_field_access() {
    let source = "point.x;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Postfix(postfix) => {
                    // Check the primary: point
                    match &*postfix.primary {
                        Expression::Primary(PrimaryExpression::Identifier(ident, _)) => {
                            assert_eq!(ident, "point");
                        }
                        _ => panic!("Expected identifier 'point' for primary expression"),
                    }

                    assert_eq!(postfix.operators.len(), 1);
                    match &postfix.operators[0] {
                        PostfixOperator::FieldAccess { name, .. } => {
                            assert_eq!(name, "x");
                        }
                        _ => panic!("Expected Field access postfix operator"),
                    }
                }
                _ => panic!("Expected postfix expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_path_resolution() {
    let source = "Option::Some;";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Postfix(postfix) => {
                    // Check the primary: Option
                    match &*postfix.primary {
                        Expression::Primary(PrimaryExpression::Identifier(ident, _)) => {
                            assert_eq!(ident, "Option");
                        }
                        _ => panic!("Expected identifier 'Option' for primary expression"),
                    }

                    assert_eq!(postfix.operators.len(), 1);
                    match &postfix.operators[0] {
                        PostfixOperator::TypePath { name, .. } => {
                            assert_eq!(name, "Some");
                        }
                        _ => panic!("Expected TypePath resolution postfix operator"),
                    }
                }
                _ => panic!("Expected postfix expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_simple_method_invocation() {
    let source = "circle.radius();";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Postfix(postfix) => {
                    // Check the primary: circle
                    match &*postfix.primary {
                        Expression::Primary(PrimaryExpression::Identifier(ident, _)) => {
                            assert_eq!(ident, "circle");
                        }
                        _ => panic!("Expected identifier 'circle' for primary expression"),
                    }

                    assert_eq!(postfix.operators.len(), 2);
                    match &postfix.operators[0] {
                        PostfixOperator::FieldAccess { name, .. } => {
                            assert_eq!(name, "radius");
                        }
                        _ => panic!("Expected Field access postfix operator"),
                    }
                    match &postfix.operators[1] {
                        PostfixOperator::Call { args, .. } => {
                            assert!(args.is_empty());
                        }
                        _ => panic!("Expected Call postfix operator"),
                    }
                }
                _ => panic!("Expected postfix expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_method_invocation_with_arguments() {
    let source = "rect.resize(10, 20);";
    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(expr_stmt) => {
            match &expr_stmt.expression {
                Expression::Postfix(postfix) => {
                    // Check the primary: rect
                    match &*postfix.primary {
                        Expression::Primary(PrimaryExpression::Identifier(ident, _)) => {
                            assert_eq!(ident, "rect");
                        }
                        _ => panic!("Expected identifier 'rect' for primary expression"),
                    }

                    assert_eq!(postfix.operators.len(), 2);
                    match &postfix.operators[0] {
                        PostfixOperator::FieldAccess { name, .. } => {
                            assert_eq!(name, "resize");
                        }
                        _ => panic!("Expected Field access postfix operator"),
                    }
                    match &postfix.operators[1] {
                        PostfixOperator::Call { args, .. } => {
                            assert_eq!(args.len(), 2);
                            match &args[0] {
                                Expression::Primary(PrimaryExpression::Literal(
                                    LiteralValue::Integer(val),
                                    _,
                                )) => {
                                    assert_eq!(*val, 10);
                                }
                                _ => panic!("Expected integer literal for first argument"),
                            }
                            match &args[1] {
                                Expression::Primary(PrimaryExpression::Literal(
                                    LiteralValue::Integer(val),
                                    _,
                                )) => {
                                    assert_eq!(*val, 20);
                                }
                                _ => panic!("Expected integer literal for second argument"),
                            }
                        }
                        _ => panic!("Expected Call postfix operator"),
                    }
                }
                _ => panic!("Expected postfix expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]

fn test_parse_method_definition() {
    let source = "c = { r: 5, area: () => this.r * this.r * 3.14 };";

    let program = parse_test_source(source);

    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Variable(bind)) => {
            assert_eq!(bind.name, "c");

            match &bind.value {
                Expression::Primary(PrimaryExpression::Record(record_lit)) => {
                    assert_eq!(record_lit.fields.len(), 2);

                    assert_eq!(record_lit.fields[0].name, "r");

                    match &record_lit.fields[1].value {
                        Expression::Lambda(lambda) => {
                            assert!(lambda.params.is_empty());

                            match &lambda.body {
                                ExpressionOrBlock::Expression(expr) => {
                                    if let Expression::Binary(bin_expr) = &**expr {
                                        // this.r * this.r * 3.14

                                        assert_eq!(bin_expr.operator, BinaryOperator::Multiply);
                                    } else {
                                        panic!("Expected binary expression in lambda body");
                                    }
                                }

                                _ => panic!("Expected expression body for lambda"),
                            }
                        }

                        _ => panic!("Expected lambda expression for field 'area'"),
                    }
                }

                _ => panic!("Expected record literal expression"),
            }
        }

        _ => panic!("Expected variable binding"),
    }
}

#[test]
fn test_parse_return_statement() {
    let source = r#"
    foo(): int = {
        return 42;
    }
    "#;
    let program = assert_parses(source);
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::LetStmt(LetStatement::Function(func)) => {
            assert_eq!(func.name, "foo");
            assert_eq!(func.params.len(), 0);
            match &func.return_type {
                Type::Primary(TypePrimary::Named(name, _)) => assert_eq!(name, "int"),
                _ => panic!("Expected return type 'int'"),
            }
            // Check block contains a single statement: return 42;
            assert_eq!(func.body.statements.len(), 1);
            match &func.body.statements[0] {
                Statement::Return(Some(expr), _) => match expr {
                    Expression::Primary(PrimaryExpression::Literal(
                        LiteralValue::Integer(val),
                        _,
                    )) => {
                        assert_eq!(*val, 42);
                    }
                    _ => panic!("Expected integer literal in return"),
                },
                Statement::Return(None, _) => {
                    panic!("Expected return with value, got return without value");
                }
                Statement::Let(_) => panic!("Expected return statement, got let"),
                Statement::Expression(_) => panic!("Expected return statement, got expression"),
                Statement::Break(_) => panic!("Unexpected break statement"),
                Statement::Continue(_) => panic!("Unexpected continue statement"),
            }
            assert!(func.body.final_expression.is_none());
        }
        _ => panic!("Expected function binding"),
    }
}

#[test]
fn test_parse_break_and_continue_statements() {
    let source = r#"
    while (true) {
        break;
        continue;
    }
    "#;
    let program = assert_parses(source);
    assert_eq!(program.statements.len(), 1);

    match &program.statements[0] {
        TopStatement::Expression(ExpressionStatement { expression, .. }) => {
            match expression {
                Expression::While(while_expr) => {
                    // Condition should be 'true'
                    match &*while_expr.condition {
                        Expression::Primary(PrimaryExpression::Literal(
                            LiteralValue::Boolean(true),
                            _,
                        )) => {}
                        _ => panic!("Expected 'true' condition in while"),
                    }
                    // Block should contain break and continue
                    assert_eq!(while_expr.body.statements.len(), 2);
                    match &while_expr.body.statements[0] {
                        Statement::Break(_) => {}
                        _ => panic!("Expected break statement"),
                    }
                    match &while_expr.body.statements[1] {
                        Statement::Continue(_) => {}
                        _ => panic!("Expected continue statement"),
                    }
                }
                _ => panic!("Expected while expression"),
            }
        }
        _ => panic!("Expected expression statement"),
    }
}

#[test]
fn test_parse_generic_type_list() {
    let source = r#"
    type Map = Map[string, int];
    type Pair = Pair[int, float];
    "#;
    let program = assert_parses(source);
    assert_eq!(program.statements.len(), 2);

    // First: type Map = Map[string, int];
    match &program.statements[0] {
        TopStatement::TypeDecl(TypeDeclaration {
            name, constructor, ..
        }) => {
            assert_eq!(name, "Map");
            match constructor {
                TypeConstructor::Alias(Type::Primary(TypePrimary::Generic {
                    name: generic_name,
                    args,
                    ..
                })) => {
                    assert_eq!(generic_name, "Map");
                    assert_eq!(args.len(), 2);
                    match &args[0] {
                        Type::Primary(TypePrimary::Named(type_name, _)) => {
                            assert_eq!(type_name, "string")
                        }
                        _ => panic!("Expected first generic arg to be 'string'"),
                    }
                    match &args[1] {
                        Type::Primary(TypePrimary::Named(type_name, _)) => {
                            assert_eq!(type_name, "int")
                        }
                        _ => panic!("Expected second generic arg to be 'int'"),
                    }
                }
                _ => panic!("Expected generic type alias for Map"),
            }
        }
        _ => panic!("Expected type declaration for Map"),
    }

    // Second: type Pair = Pair[int, float];
    match &program.statements[1] {
        TopStatement::TypeDecl(TypeDeclaration {
            name, constructor, ..
        }) => {
            assert_eq!(name, "Pair");
            match constructor {
                TypeConstructor::Alias(Type::Primary(TypePrimary::Generic {
                    name: generic_name,
                    args,
                    ..
                })) => {
                    assert_eq!(generic_name, "Pair");
                    assert_eq!(args.len(), 2);
                    match &args[0] {
                        Type::Primary(TypePrimary::Named(type_name, _)) => {
                            assert_eq!(type_name, "int")
                        }
                        _ => panic!("Expected first generic arg to be 'int'"),
                    }
                    match &args[1] {
                        Type::Primary(TypePrimary::Named(type_name, _)) => {
                            assert_eq!(type_name, "float")
                        }
                        _ => panic!("Expected second generic arg to be 'float'"),
                    }
                }
                _ => panic!("Expected generic type alias for Pair"),
            }
        }
        _ => panic!("Expected type declaration for Pair"),
    }
}
