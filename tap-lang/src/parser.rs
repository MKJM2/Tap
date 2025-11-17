use crate::ast::{Expression, LiteralValue, Operator, Program, Statement, TypeAnnotation};
use crate::lexer::{Token, TokenType};
use color_eyre::eyre::{self, WrapErr, eyre};
use std::collections::HashSet;
use thiserror::Error;

/// The parser, responsible for turning a vector of tokens into an Abstract Syntax Tree (AST).
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    type_context: HashSet<String>,
}

/// Represents the specific, low-level error that occurred during parsing.
/// This is intended to be wrapped by `eyre` for a full contextual report.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParseError {
    #[error(
        "Unexpected token in {context}: expected {expected}, but found '{found}' at line {line}"
    )]
    UnexpectedToken {
        context: String,
        expected: String,
        found: String,
        found_type: TokenType,
        line: usize,
    },

    #[error("Unexpected end of file while parsing {context}")]
    UnexpectedEof { context: String },
}

impl<'a> Parser<'a> {
    /// Creates a new `Parser`.
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            current: 0,
            type_context: HashSet::new(),
        }
    }

    /// Parses the tokens into a program, returning a rich eyre::Result.
    pub fn parse_program(&mut self) -> eyre::Result<Program> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(
                self.parse_statement()
                    .wrap_err("Failed to parse top-level statement")?,
            );
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> eyre::Result<Statement> {
        if self.match_token(&TokenType::KeywordIf) {
            return self
                .parse_if_statement()
                .wrap_err("Failed to parse if-statement");
        }
        if self.match_token(&TokenType::KeywordFunc) {
            return self
                .parse_function_def()
                .wrap_err("Failed to parse function definition");
        }
        if self.match_token(&TokenType::KeywordReturn) {
            return self
                .parse_return_statement()
                .wrap_err("Failed to parse return statement");
        }

        if self.check(&TokenType::Identifier) && self.peek_next_is(&TokenType::Colon) {
            // This path handles declarations like `x: int;`, `s: struct {..}`, etc.
            self.parse_declaration_statement()
                .wrap_err("Failed to parse declaration statement")
        } else {
            // **MODIFIED**: Let the expression parser handle dot notation now.
            // It's more robust and handles both `p.y` (get) and `p.y = 99` (set).
            self.parse_expression_or_assignment_statement()
                .wrap_err("Failed to parse expression or assignment statement")
        }
    }

    fn parse_declaration_statement(&mut self) -> eyre::Result<Statement> {
        let name = self
            .consume(&TokenType::Identifier, "declaration", "identifier")?
            .lexeme
            .clone();
        self.consume(&TokenType::Colon, "declaration", "':' after identifier")?;

        if self.match_token(&TokenType::KeywordStruct) {
            return self
                .parse_struct_declaration_after_name(name.clone())
                .wrap_err_with(|| format!("In declaration of struct '{}'", name));
        }
        if self.match_token(&TokenType::KeywordEnum) {
            return self
                .parse_enum_declaration_after_name(name.clone())
                .wrap_err_with(|| format!("In declaration of enum '{}'", name));
        }

        let type_annotation = self.parse_type_annotation()?;

        if self.match_token(&TokenType::Assign) {
            let value = self.parse_expression()?;
            self.consume(
                &TokenType::Semicolon,
                "variable assignment",
                "';' after value",
            )?;
            Ok(Statement::Assignment { name, value })
        } else {
            self.consume(
                &TokenType::Semicolon,
                "variable declaration",
                "';' after type",
            )?;
            Ok(Statement::VarDecl {
                name,
                type_annotation,
            })
        }
    }

    fn parse_expression_or_assignment_statement(&mut self) -> eyre::Result<Statement> {
        let expr = self.parse_expression()?;

        // **MODIFIED**: After parsing an expression, check if it's a property assignment.
        if self.match_token(&TokenType::Assign) {
            let value = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "assignment", "';' after value")?;

            // Check if the left side was a valid assignment target
            if let Expression::Get { object, name } = expr {
                return Ok(Statement::PropertyAssignment {
                    object: *object,
                    property: name,
                    value,
                });
            } else if let Expression::Identifier(name) = expr {
                return Ok(Statement::Assignment { name, value });
            } else {
                return Err(eyre!("Invalid assignment target."));
            }
        }

        self.consume(&TokenType::Semicolon, "expression statement", "';'")?;
        Ok(Statement::Expression(expr))
    }

    // **REMOVED**: `parse_dot_notation_statement` is no longer needed.
    // The logic is now integrated into `parse_expression_or_assignment_statement`.

    fn parse_enum_declaration_after_name(&mut self, name: String) -> eyre::Result<Statement> {
        self.consume(&TokenType::OpenBrace, "enum declaration", "'{'")?;
        let mut variants = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                variants.push(
                    self.consume(&TokenType::Identifier, "enum variant", "identifier")?
                        .lexeme
                        .clone(),
                );
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "enum declaration", "'}'")?;
        self.consume(&TokenType::Semicolon, "enum declaration", "';'")?;

        self.type_context.insert(name.clone());

        Ok(Statement::EnumDecl(crate::ast::EnumDecl { name, variants }))
    }

    fn parse_struct_declaration_after_name(&mut self, name: String) -> eyre::Result<Statement> {
        self.consume(&TokenType::OpenBrace, "struct declaration", "'{'")?;
        let mut fields = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let field_name = self
                    .consume(&TokenType::Identifier, "struct field", "field name")?
                    .lexeme
                    .clone();
                self.consume(&TokenType::Colon, "struct field", "':'")?;
                let type_annotation = self.parse_type_annotation()?;
                fields.push((field_name, type_annotation));
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "struct declaration", "'}'")?;
        self.consume(&TokenType::Semicolon, "struct declaration", "';'")?;

        self.type_context.insert(name.clone());

        Ok(Statement::StructDecl(crate::ast::StructDecl {
            name,
            fields,
        }))
    }

    fn parse_type_annotation(&mut self) -> eyre::Result<TypeAnnotation> {
        let mut type_ann = self
            .parse_primary_type()
            .wrap_err("Failed to parse base type annotation")?;

        while self.match_token(&TokenType::Arrow) {
            let right = self
                .parse_type_annotation()
                .wrap_err("Failed to parse return type in function type")?;
            type_ann = TypeAnnotation::Function {
                from: Box::new(type_ann),
                to: Box::new(right),
            };
        }

        Ok(type_ann)
    }

    fn parse_primary_type(&mut self) -> eyre::Result<TypeAnnotation> {
        if self.match_token(&TokenType::KeywordInt) {
            Ok(TypeAnnotation::Int)
        } else if self.match_token(&TokenType::KeywordStr) {
            Ok(TypeAnnotation::Str)
        } else if self.match_token(&TokenType::OpenBracket) {
            let inner_type = self.parse_type_annotation()?;
            self.consume(&TokenType::CloseBracket, "array type", "']'")?;
            Ok(TypeAnnotation::Array(Box::new(inner_type)))
        } else if self.check(&TokenType::Identifier) {
            let type_name = self.peek().unwrap().lexeme.clone();
            if self.type_context.contains(&type_name) {
                self.advance();
                Ok(TypeAnnotation::UserDefined(type_name))
            } else {
                Err(self.unexpected_token("type annotation", "a known type name"))
            }
        } else {
            Err(self.unexpected_token(
                "type annotation",
                "a valid type (int, str, [T], or user-defined type)",
            ))
        }
    }

    fn parse_function_def(&mut self) -> eyre::Result<Statement> {
        let name = self
            .consume(&TokenType::Identifier, "function def", "function name")?
            .lexeme
            .clone();
        self.consume(&TokenType::OpenParen, "function def", "'(' after name")?;

        let mut args = Vec::new();
        if !self.check(&TokenType::CloseParen) {
            loop {
                args.push(
                    self.consume(&TokenType::Identifier, "function parameter", "name")?
                        .lexeme
                        .clone(),
                );
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            &TokenType::CloseParen,
            "function def",
            "')' after parameters",
        )?;

        let body = self
            .parse_block()
            .wrap_err_with(|| format!("Failed to parse body of function '{}'", name))?;

        Ok(Statement::FunctionDef(crate::ast::FunctionDef {
            name,
            args,
            body,
        }))
    }

    // --- Expression Parsing Logic (largely unchanged) ---

    fn parse_expression(&mut self) -> eyre::Result<Expression> {
        self.parse_comparison()
    }

    fn parse_binary_expr<F>(
        &mut self,
        next_parser: F,
        types: &[TokenType],
    ) -> eyre::Result<Expression>
    where
        F: Fn(&mut Self) -> eyre::Result<Expression>,
    {
        let mut expr = next_parser(self)?;

        while self.match_tokens(types) {
            let op = Operator::try_from(&self.previous().token_type).unwrap();
            let right = next_parser(self)?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(
            Self::parse_term,
            &[
                TokenType::Equal,
                TokenType::NotEqual,
                TokenType::GreaterThan,
                TokenType::GreaterThanEqual,
                TokenType::LessThan,
                TokenType::LessThanEqual,
            ],
        )
    }

    fn parse_term(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(Self::parse_factor, &[TokenType::OpPlus, TokenType::OpMinus])
    }

    fn parse_factor(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(
            Self::parse_primary,
            &[TokenType::OpMult, TokenType::OpDivide],
        )
    }

    fn parse_primary(&mut self) -> eyre::Result<Expression> {
        if self.is_at_end() {
            return Err(eyre!(ParseError::UnexpectedEof {
                context: "primary expression".to_string()
            }));
        }

        let mut expr = match self.peek().unwrap().token_type.clone() {
            TokenType::Integer(val) => {
                self.advance();
                Expression::Literal(LiteralValue::Integer(val))
            }
            TokenType::String(val) => {
                self.advance();
                Expression::Literal(LiteralValue::String(val))
            }
            TokenType::KeywordTrue => {
                self.advance();
                Expression::Literal(LiteralValue::Boolean(true))
            }
            TokenType::KeywordFalse => {
                self.advance();
                Expression::Literal(LiteralValue::Boolean(false))
            }
            TokenType::Identifier => {
                if self.peek_next_is(&TokenType::OpenBrace) {
                    return self
                        .parse_struct_instantiation()
                        .wrap_err("Failed to parse struct instantiation");
                }
                if self.peek_next_is(&TokenType::ColonColon) {
                    let enum_name = self.advance().lexeme.clone();
                    self.advance(); // consume '::'
                    let variant_name = self
                        .consume(&TokenType::Identifier, "enum variant", "name")?
                        .lexeme
                        .clone();
                    return Ok(Expression::EnumVariant {
                        enum_name,
                        variant_name,
                    });
                }
                self.advance();
                Expression::Identifier(self.previous().lexeme.clone())
            }
            TokenType::OpenParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&TokenType::CloseParen, "grouped expression", "')'")?;
                expr
            }
            TokenType::OpenBracket => self.parse_list_literal()?,
            TokenType::Lambda => self.parse_lambda_expression()?,
            TokenType::KeywordIf => self.parse_if_expression()?,
            _ => return Err(self.unexpected_token("expression", "literal, identifier, or '('")),
        };

        loop {
            if self.match_token(&TokenType::OpenParen) {
                expr = self.parse_call_expression(expr)?;
            } else if self.match_token(&TokenType::Period) {
                let name = self
                    .consume(&TokenType::Identifier, "property access", "property name")?
                    .lexeme
                    .clone();
                expr = Expression::Get {
                    object: Box::new(expr),
                    name,
                };
            } else if self.match_token(&TokenType::OpenBracket) {
                let index_expr = self.parse_expression()?;
                self.consume(&TokenType::CloseBracket, "array access", "']'")?;
                expr = Expression::ArrayAccess {
                    array: Box::new(expr),
                    index: Box::new(index_expr),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> eyre::Result<Expression> {
        self.consume(&TokenType::KeywordIf, "if-expression", "'if'")?;
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.match_token(&TokenType::KeywordElse) {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Expression::If {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        })
    }

    fn parse_struct_instantiation(&mut self) -> eyre::Result<Expression> {
        let name = self
            .consume(
                &TokenType::Identifier,
                "struct instantiation",
                "struct name",
            )?
            .lexeme
            .clone();
        self.consume(&TokenType::OpenBrace, "struct instantiation", "'{'")?;
        let mut fields = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let field_name = self
                    .consume(&TokenType::Identifier, "struct field", "field name")?
                    .lexeme
                    .clone();
                self.consume(&TokenType::Colon, "struct field", "':'")?;
                let value = self.parse_expression()?;
                fields.push((field_name, value));
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "struct instantiation", "'}'")?;
        Ok(Expression::StructInstantiation { name, fields })
    }

    fn parse_call_expression(&mut self, callee: Expression) -> eyre::Result<Expression> {
        let mut args = Vec::new();
        if !self.check(&TokenType::CloseParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            &TokenType::CloseParen,
            "function call",
            "')' after arguments",
        )?;
        Ok(Expression::FunctionCall {
            callee: Box::new(callee),
            args,
        })
    }

    fn parse_lambda_expression(&mut self) -> eyre::Result<Expression> {
        self.consume(&TokenType::Lambda, "lambda", "'\'")?;
        let mut args = Vec::new();
        if !self.check(&TokenType::OpOr) {
            loop {
                args.push(
                    self.consume(&TokenType::Identifier, "lambda parameter", "name")?
                        .lexeme
                        .clone(),
                );
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::Period, "lambda", "'.' after parameters")?;
        let body = self.parse_expression()?;
        Ok(Expression::Lambda {
            args,
            body: Box::new(body),
        })
    }

    fn parse_list_literal(&mut self) -> eyre::Result<Expression> {
        self.consume(&TokenType::OpenBracket, "list literal", "'['")?;
        let mut elements = Vec::new();
        if !self.check(&TokenType::CloseBracket) {
            loop {
                elements.push(self.parse_expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBracket, "list literal", "']'")?;
        Ok(Expression::List(elements))
    }

    fn parse_if_statement(&mut self) -> eyre::Result<Statement> {
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.match_token(&TokenType::KeywordElse) {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Statement::If {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        })
    }

    fn parse_return_statement(&mut self) -> eyre::Result<Statement> {
        let value = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "return statement", "';'")?;
        Ok(Statement::Return(value))
    }

    fn parse_block(&mut self) -> eyre::Result<Vec<Statement>> {
        self.consume(&TokenType::OpenBrace, "block", "'{'")?;
        let mut statements = Vec::new();
        while !self.check(&TokenType::CloseBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        self.consume(&TokenType::CloseBrace, "block", "'}'")?;
        Ok(statements)
    }

    // --- Helper Functions ---
    // (Unchanged)
    fn consume(
        &mut self,
        token_type: &TokenType,
        context: &str,
        expected: &str,
    ) -> eyre::Result<&Token> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        Err(self.unexpected_token(context, expected))
    }

    fn unexpected_token(&self, context: &str, expected: &str) -> eyre::Report {
        let peeked = self.peek();
        let (found, found_type, line) = peeked
            .map(|t| (t.lexeme.clone(), t.token_type.clone(), t.line))
            .unwrap_or_else(|| {
                let line = if self.tokens.is_empty() {
                    0
                } else {
                    self.tokens.last().unwrap().line
                };
                ("<EOF>".to_string(), TokenType::EndOfFile, line)
            });

        eyre!(ParseError::UnexpectedToken {
            context: context.to_string(),
            expected: expected.to_string(),
            found,
            found_type,
            line,
        })
    }

    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_tokens(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().unwrap().token_type)
            == std::mem::discriminant(token_type)
    }

    fn peek_next_is(&self, token_type: &TokenType) -> bool {
        self.tokens.get(self.current + 1).map_or(false, |t| {
            std::mem::discriminant(&t.token_type) == std::mem::discriminant(token_type)
        })
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            .map_or(true, |t| t.token_type == TokenType::EndOfFile)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

// Assume Operator has a TryFrom implementation for convenience
impl TryFrom<&TokenType> for Operator {
    type Error = eyre::Report;

    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::OpPlus => Ok(Operator::Add),
            TokenType::OpMinus => Ok(Operator::Subtract),
            TokenType::OpMult => Ok(Operator::Multiply),
            TokenType::OpDivide => Ok(Operator::Divide),
            TokenType::Equal => Ok(Operator::Equal),
            TokenType::NotEqual => Ok(Operator::NotEqual),
            TokenType::GreaterThan => Ok(Operator::GreaterThan),
            TokenType::GreaterThanEqual => Ok(Operator::GreaterThanEqual),
            TokenType::LessThan => Ok(Operator::LessThan),
            TokenType::LessThanEqual => Ok(Operator::LessThanEqual),
            _ => Err(eyre!("Cannot convert {:?} to a binary operator", value)),
        }
    }
}

// --- Tests (Unchanged) ---
#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    /// Test helper to lex and parse source, panicking with a clean `eyre` report on failure.
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

    #[test]
    fn test_simple_expression() {
        let program = parse_test_source("5;");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Literal(LiteralValue::Integer(5))) => {}
            _ => panic!("Expected an integer literal expression"),
        }
    }

    #[test]
    fn test_assignment() {
        let program = parse_test_source("x = 5;");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment { name, value } => {
                assert_eq!(name, "x");
                assert_eq!(*value, Expression::Literal(LiteralValue::Integer(5)));
            }
            _ => panic!("Expected assignment statement"),
        }
    }

    #[test]
    fn test_function_definition() {
        let program = parse_test_source("func my_func(a, b) { a + b; }");
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::FunctionDef(def) => {
                assert_eq!(def.name, "my_func");
                assert_eq!(def.args, vec!["a".to_string(), "b".to_string()]);
                assert_eq!(def.body.len(), 1);
            }
            _ => panic!("Expected function definition"),
        }
    }
}
