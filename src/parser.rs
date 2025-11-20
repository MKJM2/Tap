use crate::ast::{
    Expression, LiteralValue, MatchArm, Operator, Pattern, Program, Statement, TypeAnnotation,
};
use crate::lexer::{Span, Token, TokenType};
use color_eyre::eyre::{self, WrapErr, eyre};
use std::collections::HashSet;
use thiserror::Error;

/// The parser, responsible for turning a vector of tokens into an Abstract Syntax Tree (AST).
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    type_context: HashSet<String>,
    source: &'a str,
}

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
        span: Span,
    },

    #[error("Unexpected end of file while parsing {context}")]
    UnexpectedEof { context: String, span: Span },
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], source: &'a str) -> Self {
        Parser {
            tokens,
            current: 0,
            type_context: HashSet::new(),
            source,
        }
    }

    pub fn parse_program(&mut self) -> eyre::Result<Program> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            if self.match_token(&TokenType::Semicolon) {
                continue;
            }
            statements.push(
                self.parse_statement()
                    .wrap_err("Failed to parse top-level statement")?,
            );
        }
        Ok(Program { statements })
    }

    // --- Statement Parsing ---

    fn parse_statement(&mut self) -> eyre::Result<Statement> {
        if self.match_token(&TokenType::KeywordType) {
            return self
                .parse_type_declaration()
                .wrap_err("Failed to parse type declaration");
        }
        if self.match_token(&TokenType::KeywordBreak) {
            self.consume(&TokenType::Semicolon, "break statement", "';'")?;
            return Ok(Statement::Break);
        }
        if self.match_token(&TokenType::KeywordContinue) {
            self.consume(&TokenType::Semicolon, "continue statement", "';'")?;
            return Ok(Statement::Continue);
        }
        if self.check(&TokenType::KeywordIf) {
            let if_expr = self.parse_if_expression()?;
            return Ok(Statement::Expression(if_expr));
        }
        if self.check(&TokenType::KeywordMatch) {
            let match_expr = self.parse_match_expression()?;
            return Ok(Statement::Expression(match_expr));
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
        if self.match_token(&TokenType::KeywordWhile) {
            return self
                .parse_while_loop()
                .wrap_err("Failed to parse while loop");
        }
        if self.match_token(&TokenType::KeywordFor) {
            return self.parse_for_loop().wrap_err("Failed to parse for loop");
        }

        // Declaration vs Assignment/Expression
        if self.check(&TokenType::Identifier) && self.peek_next_is(&TokenType::Colon) {
            self.parse_declaration_statement()
                .wrap_err("Failed to parse declaration statement")
        } else {
            self.parse_expression_or_assignment_statement()
                .wrap_err("Failed to parse expression or assignment statement")
        }
    }

    fn parse_type_declaration(&mut self) -> eyre::Result<Statement> {
        let name = self
            .consume(&TokenType::Identifier, "type declaration", "type name")?
            .lexeme
            .clone();
        self.consume(&TokenType::Assign, "type declaration", "'='")?;

        if self.match_token(&TokenType::KeywordStruct) {
            self.parse_struct_declaration_after_name(name)
        } else if self.match_token(&TokenType::KeywordEnum) {
            self.parse_enum_declaration_after_name(name)
        } else {
            Err(self.unexpected_token(
                "type declaration",
                "'struct' or 'enum'",
            ))
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
            // Semicolon is optional after an expression in a declaration
            self.match_token(&TokenType::Semicolon);
            Ok(Statement::VarDecl {
                name,
                type_annotation,
                value: Some(value),
            })
        } else {
            self.consume(
                &TokenType::Semicolon,
                "variable declaration",
                "';' after type",
            )?;
            Ok(Statement::VarDecl {
                name,
                type_annotation,
                value: None,
            })
        }
    }

    fn parse_expression_or_assignment_statement(&mut self) -> eyre::Result<Statement> {
        let expr = self.parse_expression()?;

        if self.match_token(&TokenType::Assign) {
            let value = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "assignment", "';' after value")?;

            // Check L-Value validity
            if let Expression::Get { object, name } = expr {
                return Ok(Statement::PropertyAssignment {
                    object: *object,
                    property: name,
                    value,
                });
            } else if let Expression::Identifier(name) = expr {
                return Ok(Statement::Assignment { name, value });
            } else if let Expression::ArrayAccess { array, index } = expr {
                // Assuming you added ArrayAssignment to Statement AST
                return Ok(Statement::ArrayAssignment {
                    array: *array,
                    index: *index,
                    value,
                });
            } else {
                return Err(eyre!("Invalid assignment target."));
            }
        }

        if self.check(&TokenType::CloseBrace) {
            return Ok(Statement::Expression(expr));
        }

        if self.is_at_end() {
            return Ok(Statement::Expression(expr));
        }

        self.consume(&TokenType::Semicolon, "expression statement", "';'")?;
        Ok(Statement::Expression(expr))
    }

    // --- Control Flow Implementations ---

    fn parse_while_loop(&mut self) -> eyre::Result<Statement> {
        // "while" already consumed
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Statement::While {
            condition: Box::new(condition),
            body,
        })
    }

    fn parse_for_loop(&mut self) -> eyre::Result<Statement> {
        // "for" already consumed
        let iterator = self
            .consume(&TokenType::Identifier, "for loop", "iterator name")?
            .lexeme
            .clone();
        self.consume(&TokenType::KeywordIn, "for loop", "'in'")?;
        let iterable = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Statement::For {
            iterator,
            iterable: Box::new(iterable),
            body,
        })
    }

    fn parse_match_expression(&mut self) -> eyre::Result<Expression> {
        self.consume(&TokenType::KeywordMatch, "match statement", "'match'")?;
        let value = self.parse_expression()?;
        self.consume(&TokenType::OpenBrace, "match statement", "'{'")?;

        let mut arms = Vec::new();
        while !self.check(&TokenType::CloseBrace) && !self.is_at_end() {
            let pattern = self.parse_pattern()?;
            self.consume(&TokenType::ArrowFat, "match arm", "'=>'")?; // Ensure Token::ArrowFat (=>) exists
            let expr = self.parse_expression()?;

            // Optional comma after expression
            self.match_token(&TokenType::Comma);

            arms.push(MatchArm {
                pattern,
                body: expr,
            });
        }
        self.consume(&TokenType::CloseBrace, "match statement", "'}'")?;

        Ok(Expression::Match {
            value: Box::new(value),
            arms,
        })
    }

    fn parse_pattern(&mut self) -> eyre::Result<Pattern> {
        // Very basic pattern matching support
        if self.match_token(&TokenType::Underscore) {
            return Ok(Pattern::Wildcard);
        }
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::Integer(i) => {
                    self.advance();
                    Ok(Pattern::Literal(LiteralValue::Integer(i)))
                }
                TokenType::String(ref s) => {
                    let s = s.clone();
                    self.advance();
                    Ok(Pattern::Literal(LiteralValue::String(s)))
                }
                TokenType::Identifier => {
                    let name = self.advance().lexeme.clone();
                    // Check for Enum variant pattern: MyEnum::Variant(x) or Some(x)
                    if self.match_token(&TokenType::OpenParen) {
                        let mut sub_patterns = Vec::new();
                        if !self.check(&TokenType::CloseParen) {
                            loop {
                                sub_patterns.push(self.parse_pattern()?);
                                if !self.match_token(&TokenType::Comma) {
                                    break;
                                }
                            }
                        }
                        self.consume(&TokenType::CloseParen, "pattern", "')'")?;
                        Ok(Pattern::EnumVariant {
                            enum_name: "".to_string(), // enum name is unknown here
                            variant: name,
                            vars: sub_patterns,
                        })
                    } else if self.match_token(&TokenType::ColonColon) {
                        let variant = self
                            .consume(&TokenType::Identifier, "pattern", "variant name")?
                            .lexeme
                            .clone();
                        let mut sub_patterns = Vec::new();
                        if self.match_token(&TokenType::OpenParen) {
                            if !self.check(&TokenType::CloseParen) {
                                loop {
                                    sub_patterns.push(self.parse_pattern()?);
                                    if !self.match_token(&TokenType::Comma) {
                                        break;
                                    }
                                }
                            }
                            self.consume(&TokenType::CloseParen, "pattern", "')'")?;
                        }
                        Ok(Pattern::EnumVariant {
                            enum_name: name,
                            variant,
                            vars: sub_patterns,
                        })
                    } else {
                        // Just a variable capture
                        Ok(Pattern::Identifier(name))
                    }
                }
                _ => Err(self.unexpected_token("pattern", "literal, identifier, or '_'")),
            }
        } else {
            Err(self.unexpected_token("pattern", "literal, identifier, or '_'"))
        }
    }

    fn parse_return_statement(&mut self) -> eyre::Result<Statement> {
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
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

    // --- Expression Parsing (Stratified) ---

    pub fn parse_expression(&mut self) -> eyre::Result<Expression> {
        self.parse_logic_or()
    }

    fn parse_logic_or(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(Self::parse_logic_and, &[TokenType::OpLor])
    }

    fn parse_logic_and(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(Self::parse_equality, &[TokenType::OpLand])
    }

    fn parse_equality(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(
            Self::parse_comparison,
            &[TokenType::Equal, TokenType::NotEqual],
        )
    }

    fn parse_comparison(&mut self) -> eyre::Result<Expression> {
        self.parse_binary_expr(
            Self::parse_term,
            &[
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
            Self::parse_unary,
            &[TokenType::OpMult, TokenType::OpDivide, TokenType::OpMod],
        )
    }

    fn parse_unary(&mut self) -> eyre::Result<Expression> {
        if self.match_tokens(&[TokenType::OpMinus, TokenType::Bang]) {
            let op = Operator::try_from(&self.previous().token_type).unwrap();
            let right = self.parse_unary()?;
            return Ok(Expression::Unary {
                op,
                right: Box::new(right),
            });
        }
        self.parse_call_member()
    }

    // The "Loop" for postfix operations (call, index, dot)
    fn parse_call_member(&mut self) -> eyre::Result<Expression> {
        let mut expr = self.parse_primary()?;

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

    // Primary atoms (no recursion into operators on the left)
    fn parse_primary(&mut self) -> eyre::Result<Expression> {
        if self.is_at_end() {
            return Err(eyre!(ParseError::UnexpectedEof {
                context: "primary expression".to_string(),
                span: Span::new(0, 0), // Simplify span logic for EOF
            }));
        }

        let token = self.peek().unwrap();
        match token.token_type {
            TokenType::Integer(val) => {
                self.advance();
                Ok(Expression::Literal(LiteralValue::Integer(val)))
            }
            TokenType::Float(val) => {
                // Added Floats
                self.advance();
                Ok(Expression::Literal(LiteralValue::Float(val)))
            }
            TokenType::String(ref val) => {
                let val = val.clone();
                self.advance();
                Ok(Expression::Literal(LiteralValue::String(val)))
            }
            TokenType::KeywordTrue => {
                self.advance();
                Ok(Expression::Literal(LiteralValue::Boolean(true)))
            }
            TokenType::KeywordFalse => {
                self.advance();
                Ok(Expression::Literal(LiteralValue::Boolean(false)))
            }
            TokenType::KeywordUnit => {
                // Added Unit literal
                self.advance();
                Ok(Expression::Literal(LiteralValue::Unit))
            }
            TokenType::Identifier => {
                // Lookahead for Struct Instantiation or Enum::Variant
                let token = self.peek().unwrap();
                if self.peek_next_is(&TokenType::OpenBrace)
                    && self.type_context.contains(&token.lexeme)
                {
                    self.parse_struct_instantiation()
                } else if self.peek_next_is(&TokenType::ColonColon) {
                    let enum_name = self.advance().lexeme.clone();
                    self.advance(); // consume ::
                    let variant_name = self
                        .consume(&TokenType::Identifier, "enum", "variant")?
                        .lexeme
                        .clone();
                    Ok(Expression::EnumVariant {
                        enum_name,
                        variant_name,
                    })
                } else {
                    self.advance();
                    Ok(Expression::Identifier(self.previous().lexeme.clone()))
                }
            }
            TokenType::OpenParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&TokenType::CloseParen, "group", "')'")?;
                Ok(expr)
            }
            TokenType::OpenBracket => self.parse_list_literal(),
            TokenType::Lambda => self.parse_lambda_expression(),
            TokenType::KeywordIf => self.parse_if_expression(),
            TokenType::KeywordMatch => self.parse_match_expression(),
            TokenType::OpenBrace => self.parse_block_expression(),
            _ => Err(self.unexpected_token("expression", "literal, identifier, or '('")),
        }
    }

    // --- Helpers for complex expressions ---

    fn parse_block_expression(&mut self) -> eyre::Result<Expression> {
        let statements = self.parse_block()?;
        Ok(Expression::Block(statements))
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
        self.consume(&TokenType::CloseParen, "function call", "')'")?;
        Ok(Expression::FunctionCall {
            callee: Box::new(callee),
            args,
        })
    }

    fn parse_lambda_expression(&mut self) -> eyre::Result<Expression> {
        self.consume(&TokenType::Lambda, "lambda", "'\\'")?;
        let mut args = Vec::new();
        // Parse args until we hit a dot
        if !self.check(&TokenType::Period) {
            loop {
                args.push(
                    self.consume(&TokenType::Identifier, "lambda param", "name")?
                        .lexeme
                        .clone(),
                );
                // Optional comma for lambda args? syntax said "ident {ident} ."
                // Assuming spaces/logic separator, but let's support optional comma
                if self.check(&TokenType::Period) {
                    break;
                }
                self.match_token(&TokenType::Comma);
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
        self.consume(&TokenType::OpenBracket, "list", "'['")?;
        let mut elements = Vec::new();
        if !self.check(&TokenType::CloseBracket) {
            loop {
                elements.push(self.parse_expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBracket, "list", "']'")?;
        Ok(Expression::List(elements))
    }

    fn parse_if_expression(&mut self) -> eyre::Result<Expression> {
        self.consume(&TokenType::KeywordIf, "if expr", "'if'")?;
        let condition = self.parse_expression()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.match_token(&TokenType::KeywordElse) {
            Some(if self.check(&TokenType::KeywordIf) {
                // Handle "else if" by wrapping it in a block or expression
                // For simplicity here, we treat it as a block containing one expression/statement
                vec![Statement::Expression(self.parse_if_expression()?)]
            } else {
                self.parse_block()?
            })
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
            .consume(&TokenType::Identifier, "struct", "name")?
            .lexeme
            .clone();
        self.consume(&TokenType::OpenBrace, "struct", "'{'")?;
        let mut fields = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let field_name = self
                    .consume(&TokenType::Identifier, "field", "name")?
                    .lexeme
                    .clone();
                self.consume(&TokenType::Colon, "field", "':'")?;
                let val = self.parse_expression()?;
                fields.push((field_name, val));
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "struct", "'}'")?;
        Ok(Expression::StructInstantiation { name, fields })
    }

    // --- Type & Definition Parsing ---

    fn parse_function_def(&mut self) -> eyre::Result<Statement> {
        let name = self
            .consume(&TokenType::Identifier, "func", "name")?
            .lexeme
            .clone();
        self.consume(&TokenType::OpenParen, "func", "'('")?;
        let mut args = Vec::new();
        if !self.check(&TokenType::CloseParen) {
            loop {
                let arg_name = self
                    .consume(&TokenType::Identifier, "param", "name")?
                    .lexeme
                    .clone();
                // Optional type annotation for args
                if self.match_token(&TokenType::Colon) {
                    self.parse_type_annotation()?; // Just consume it for now, or store it if AST supports it
                }
                args.push(arg_name);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseParen, "func", "')'")?;

        // Optional return type
        if self.match_token(&TokenType::Arrow) {
            self.parse_type_annotation()?;
        }

        let body = self
            .parse_block()
            .wrap_err_with(|| format!("body of '{}'", name))?;
        Ok(Statement::FunctionDef(crate::ast::FunctionDef {
            name,
            args,
            body,
        }))
    }

    fn parse_struct_declaration_after_name(&mut self, name: String) -> eyre::Result<Statement> {
        self.consume(&TokenType::OpenBrace, "struct", "'{'")?;
        let mut fields = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let field_name = self
                    .consume(&TokenType::Identifier, "field", "name")?
                    .lexeme
                    .clone();
                self.consume(&TokenType::Colon, "field", "':'")?;
                let type_ann = self.parse_type_annotation()?;
                fields.push((field_name, type_ann));
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "struct", "'}'")?;
        self.consume(&TokenType::Semicolon, "struct", "';'")?;
        self.type_context.insert(name.clone());
        Ok(Statement::StructDecl(crate::ast::StructDecl {
            name,
            fields,
        }))
    }

    fn parse_enum_declaration_after_name(&mut self, name: String) -> eyre::Result<Statement> {
        self.consume(&TokenType::OpenBrace, "enum", "'{'")?;
        let mut variants = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let variant_name = self
                    .consume(&TokenType::Identifier, "variant", "name")?
                    .lexeme
                    .clone();

                let mut types = Vec::new();
                if self.match_token(&TokenType::OpenParen) {
                    loop {
                        types.push(self.parse_type_annotation()?);
                        if !self.match_token(&TokenType::Comma) {
                            break;
                        }
                    }
                    self.consume(&TokenType::CloseParen, "enum variant", "')'")?;
                }

                variants.push(crate::ast::EnumVariant {
                    name: variant_name,
                    types,
                });

                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "enum", "'}'")?;
        self.consume(&TokenType::Semicolon, "enum", "';'")?;
        self.type_context.insert(name.clone());
        Ok(Statement::EnumDecl(crate::ast::EnumDecl { name, variants }))
    }

    fn parse_type_annotation(&mut self) -> eyre::Result<TypeAnnotation> {
        let mut type_ann = self.parse_primary_type()?;
        while self.match_token(&TokenType::Arrow) {
            let right = self.parse_type_annotation()?;
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
            let inner = self.parse_type_annotation()?;
            self.consume(&TokenType::CloseBracket, "type", "']'")?;
            Ok(TypeAnnotation::Array(Box::new(inner)))
        } else if self.check(&TokenType::Identifier) {
            let name = self.advance().lexeme.clone();
            // In a real compiler, we might validate `name` exists in `type_context`
            Ok(TypeAnnotation::UserDefined(name))
        } else {
            Err(self.unexpected_token("type", "int, str, [T], or identifier"))
        }
    }

    // --- Utility ---

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
        if let Some(token) = peeked {
            eyre!(ParseError::UnexpectedToken {
                context: context.to_string(),
                expected: expected.to_string(),
                found: token.lexeme.clone(),
                found_type: token.token_type.clone(),
                line: token.line,
                span: token.span,
            })
        } else {
            eyre!(ParseError::UnexpectedEof {
                context: context.to_string(),
                span: if self.tokens.is_empty() {
                    Span::new(0, 0)
                } else {
                    let l = self.tokens.last().unwrap();
                    Span::new(l.span.hi, l.span.hi)
                }
            })
        }
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

impl TryFrom<&TokenType> for Operator {
    type Error = eyre::Report;
    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::OpPlus => Ok(Operator::Add),
            TokenType::OpMinus => Ok(Operator::Subtract),
            TokenType::OpMult => Ok(Operator::Multiply),
            TokenType::OpDivide => Ok(Operator::Divide),
            TokenType::OpMod => Ok(Operator::Modulo),
            TokenType::Equal => Ok(Operator::Equal),
            TokenType::NotEqual => Ok(Operator::NotEqual),
            TokenType::GreaterThan => Ok(Operator::GreaterThan),
            TokenType::GreaterThanEqual => Ok(Operator::GreaterThanEqual),
            TokenType::LessThan => Ok(Operator::LessThan),
            TokenType::LessThanEqual => Ok(Operator::LessThanEqual),
            TokenType::OpLand => Ok(Operator::And),
            TokenType::OpLor => Ok(Operator::Or),
            TokenType::Bang => Ok(Operator::Not),
            _ => Err(eyre!("Cannot convert {:?} to a binary operator", value)),
        }
    }
}
