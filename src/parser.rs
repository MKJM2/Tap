use crate::ast::*;
use crate::diagnostics::{Diagnostic, DiagnosticKind, Reporter};
use crate::lexer::{Token, TokenType};

/// Structured parse error used as the error type in parser `Result`s.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub context: Option<String>,
}

impl ParseError {
    fn new(message: String, span: Span, context: Option<String>) -> Self {
        ParseError {
            message,
            span,
            context,
        }
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    reporter: &'a mut Reporter,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], reporter: &'a mut Reporter) -> Self {
        Parser {
            tokens,
            current: 0,
            reporter,
        }
    }

    fn error(&mut self, span: Span, message: String, context: Option<&str>) -> ParseError {
        let diagnostic = if let Some(ctx) = context {
            Diagnostic::new(DiagnosticKind::Error, message.clone(), span)
                .with_context(ctx.to_string())
        } else {
            Diagnostic::new(DiagnosticKind::Error, message.clone(), span)
        };

        self.reporter.add_diagnostic(diagnostic);
        ParseError::new(message, span, context.map(|s| s.to_string()))
    }

    fn consume(
        &mut self,
        expected: TokenType,
        message: &str,
        context: Option<&str>,
    ) -> Result<&Token, ParseError> {
        if self.check(expected.clone()) {
            Ok(self.advance())
        } else {
            let found = self.peek().clone();
            let full_message = format!("{} Found {:?} instead.", message, found.token_type);
            Err(self.error(found.span, full_message, context))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();
        let start_span = self.peek().span;

        while !self.is_at_end() {
            match self.parse_top_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
        }

        let end_span = self.previous().span;
        let program_span = Span::new(start_span.start, end_span.end);

        Ok(Program::new(statements, program_span))
    }

    fn parse_top_statement(&mut self) -> Result<TopStatement, ParseError> {
        // Handle type declarations
        if self.check(TokenType::KeywordType) {
            return self.parse_type_declaration().map(TopStatement::TypeDecl);
        }

        // Handle while loops
        if self.check(TokenType::KeywordWhile) {
            let expr = self.parse_while_statement()?;
            let span = expr.span();
            return Ok(TopStatement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Handle for loops (contextual keyword)
        if self.is_contextual_keyword("for") {
            let expr = self.parse_for_statement()?;
            let span = expr.span();
            return Ok(TopStatement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Disambiguate function definition vs function call
        if self.peek().token_type.is_identifier()
            && self.peek_next().token_type == TokenType::OpenParen
        {
            if self.looks_like_function_definition() {
                return self.parse_function_statement().map(TopStatement::LetStmt);
            }
        }

        // Handle let/mut
        if self.peek().token_type == TokenType::KeywordMut {
            return self.parse_let_statement().map(TopStatement::LetStmt);
        }
        if self.peek().token_type.is_identifier()
            && (self.peek_next().token_type == TokenType::Assign
                || self.peek_next().token_type == TokenType::Colon)
        {
            return self.parse_let_statement().map(TopStatement::LetStmt);
        }

        self.parse_expression_statement()
            .map(TopStatement::Expression)
    }

    fn is_contextual_keyword(&self, keyword: &str) -> bool {
        if let TokenType::Identifier(name) = &self.tokens[self.current].token_type {
            name == keyword
        } else {
            false
        }
    }

    fn looks_like_function_definition(&self) -> bool {
        let idx = self.current + 2; // Skip identifier and OpenParen

        // Empty params: `()`
        if idx < self.tokens.len() && self.tokens[idx].token_type == TokenType::CloseParen {
            return true;
        }

        // Check for `identifier :`
        if idx < self.tokens.len() && self.tokens[idx].token_type.is_identifier() {
            if idx + 1 < self.tokens.len() && self.tokens[idx + 1].token_type == TokenType::Colon {
                return true;
            }
        }

        false
    }

    fn parse_type_declaration(&mut self) -> Result<TypeDeclaration, ParseError> {
        self.consume(
            TokenType::KeywordType,
            "Expected 'type' keyword.",
            Some("while parsing a type declaration"),
        )?;

        let name_token = self.peek().clone();
        let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
            self.advance();
            s
        } else {
            let msg = format!("Expected type name, but found {:?}.", name_token.token_type);
            return Err(self.error(
                name_token.span,
                msg,
                Some("while parsing a type declaration"),
            ));
        };

        self.consume(
            TokenType::Assign,
            "Expected '=' after type name.",
            Some("while parsing a type declaration"),
        )?;

        let constructor = self.parse_type_constructor()?;

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after type declaration.",
            Some("while parsing a type declaration"),
        )?;

        let span = Span::new(name_token.span.start, self.previous().span.end);

        Ok(TypeDeclaration {
            name,
            constructor,
            span,
        })
    }

    fn parse_type_constructor(&mut self) -> Result<TypeConstructor, ParseError> {
        let saved_pos = self.current;

        // Case 1: RecordType (starts with '{')
        if self.check(TokenType::OpenBrace) {
            let record = self.parse_record_type()?;
            return Ok(TypeConstructor::Record(record));
        }

        // Case 2: Try to parse a SumConstructor
        match self.parse_variant() {
            Ok(first_variant) => {
                if self.check(TokenType::Pipe) {
                    let mut variants = vec![first_variant.clone()];
                    while self.match_token(&[TokenType::Pipe]) {
                        variants.push(self.parse_variant()?);
                    }
                    let end_span = variants
                        .last()
                        .map(|v| v.span)
                        .unwrap_or(first_variant.span);
                    let sum_span = Span::new(first_variant.span.start, end_span.end);
                    return Ok(TypeConstructor::Sum(SumConstructor {
                        variants,
                        span: sum_span,
                    }));
                } else {
                    let span = first_variant.span;
                    return Ok(TypeConstructor::Sum(SumConstructor {
                        variants: vec![first_variant],
                        span,
                    }));
                }
            }
            Err(_) => {
                self.current = saved_pos;
            }
        }

        // Case 3: Simple Type Alias
        let alias_type = self.parse_type()?;
        Ok(TypeConstructor::Alias(alias_type))
    }

    fn parse_variant(&mut self) -> Result<Variant, ParseError> {
        let name_token = self.peek().clone();
        let name = match &name_token.token_type {
            TokenType::Identifier(s) => {
                self.advance();
                s.clone()
            }
            TokenType::KeywordNone => {
                self.advance();
                "None".to_string()
            }
            _ => {
                let msg = format!(
                    "Expected variant name, but found {:?}.",
                    name_token.token_type
                );
                return Err(self.error(
                    name_token.span,
                    msg,
                    Some("while parsing a sum type variant"),
                ));
            }
        };

        let ty = if self.check(TokenType::OpenParen) {
            self.advance();
            let inner_type = self.parse_type()?;
            self.consume(
                TokenType::CloseParen,
                "Expected ')' after variant payload type.",
                Some("while parsing a sum type variant"),
            )?;
            Some(inner_type)
        } else {
            None
        };

        let end_span = self.previous().span;
        let span = Span::new(name_token.span.start, end_span.end);

        Ok(Variant { name, ty, span })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // Handle while loops
        if self.check(TokenType::KeywordWhile) {
            let expr = self.parse_while_statement()?;
            let span = expr.span();
            return Ok(Statement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Handle for loops
        if self.is_contextual_keyword("for") {
            let expr = self.parse_for_statement()?;
            let span = expr.span();
            return Ok(Statement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Function definitions
        if self.peek().token_type.is_identifier()
            && self.peek_next().token_type == TokenType::OpenParen
            && self.looks_like_function_definition()
        {
            return self.parse_function_statement().map(Statement::Let);
        }

        // Let/mut bindings
        if self.peek().token_type == TokenType::KeywordMut {
            return self.parse_let_statement().map(Statement::Let);
        }
        if self.peek().token_type.is_identifier()
            && (self.peek_next().token_type == TokenType::Assign
                || self.peek_next().token_type == TokenType::Colon)
        {
            return self.parse_let_statement().map(Statement::Let);
        }

        self.parse_expression_statement().map(Statement::Expression)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let mutable = self.match_token(&[TokenType::KeywordMut]);

        let name = if let TokenType::Identifier(s) = self.peek().token_type.clone() {
            self.advance();
            s
        } else {
            let token = self.peek().clone();
            let msg = format!(
                "Expected identifier for variable name in let statement, but found {:?}.",
                token.token_type
            );
            return Err(self.error(token.span, msg, Some("while parsing a let statement")));
        };

        let type_annotation = if self.match_token(&[TokenType::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(
            TokenType::Assign,
            "Expected '=' after identifier in let statement.",
            Some("while parsing a let statement"),
        )?;

        let value = self.parse_expression()?;

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after expression in let statement.",
            Some("while parsing a let statement"),
        )?;

        Ok(LetStatement::Variable(VariableBinding {
            mutable,
            name,
            type_annotation,
            value,
            span: Span { start: 0, end: 0 },
        }))
    }

    fn parse_function_statement(&mut self) -> Result<LetStatement, ParseError> {
        let mutable = self.match_token(&[TokenType::KeywordMut]);

        let name_token = self.peek().clone();
        let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
            self.advance();
            s
        } else {
            let msg = format!(
                "Expected identifier for function name, but found {:?}.",
                name_token.token_type
            );
            return Err(self.error(
                name_token.span,
                msg,
                Some("while parsing a function declaration"),
            ));
        };

        let params = self.parse_parameters()?;
        let return_type = if self.match_token(&[TokenType::Colon]) {
            self.parse_type()?
        } else {
            Type::Primary(TypePrimary::Named(
                "unit".to_string(),
                Span { start: 0, end: 0 },
            ))
        };

        self.consume(
            TokenType::Assign,
            "Expected '=' after function signature.",
            Some("while parsing a function declaration"),
        )?;

        let body = self.parse_block()?;
        let span = Span::new(name_token.span.start, body.span.end);

        Ok(LetStatement::Function(FunctionBinding {
            mutable,
            name,
            params,
            return_type,
            body,
            span,
        }))
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let token = self.peek().clone();
        let span = token.span;

        match &token.token_type {
            TokenType::Identifier(name) => {
                // Check for generic type: Foo[...]
                let name = name.clone();
                self.advance();
                if self.check(TokenType::OpenBracket) {
                    self.advance(); // consume '['
                    let mut args = Vec::new();
                    while !self.check(TokenType::CloseBracket) && !self.is_at_end() {
                        args.push(self.parse_type()?);
                        if !self.match_token(&[TokenType::Comma]) {
                            break;
                        }
                    }
                    self.consume(
                        TokenType::CloseBracket,
                        "Expected ']' after generic type arguments.",
                        Some("while parsing a generic type"),
                    )?;
                    Ok(Type::Primary(TypePrimary::Generic {
                        name,
                        args,
                        span: Span::new(span.start, self.previous().span.end),
                    }))
                } else {
                    Ok(Type::Primary(TypePrimary::Named(name, span)))
                }
            }
            TokenType::OpenBracket => {
                self.advance(); // consume '['
                let inner_type = self.parse_type()?;
                self.consume(
                    TokenType::CloseBracket,
                    "Expected ']' after list type.",
                    Some("while parsing a list type"),
                )?;
                Ok(Type::Primary(TypePrimary::List(
                    Box::new(inner_type),
                    Span::new(span.start, self.previous().span.end),
                )))
            }
            TokenType::OpenBrace => {
                let record_type = self.parse_record_type()?;
                Ok(Type::Primary(TypePrimary::Record(record_type)))
            }
            _ => {
                let msg = format!("Expected type, but found {:?}.", token.token_type);
                Err(self.error(span, msg, Some("while parsing a type annotation")))
            }
        }
    }

    fn parse_record_type(&mut self) -> Result<RecordType, ParseError> {
        let start_span = self
            .consume(
                TokenType::OpenBrace,
                "Expected '{' to start a record type.",
                Some("while parsing a record type"),
            )?
            .span;

        let mut fields = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let name_token = self.peek().clone();
            let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                self.advance();
                s
            } else {
                let msg = format!(
                    "Expected identifier for field name in record type, but found {:?}.",
                    name_token.token_type
                );
                return Err(self.error(name_token.span, msg, Some("while parsing a record type")));
            };

            let name_span = self.previous().span;

            self.consume(
                TokenType::Colon,
                "Expected ':' after field name in record type.",
                Some("while parsing a record type"),
            )?;

            let type_ = self.parse_type()?;
            let field_span = Span::new(name_span.start, type_.span().end);

            fields.push(FieldDeclaration {
                name,
                ty: type_,
                span: field_span,
            });

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end a record type.",
                Some("while parsing a record type"),
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(RecordType { fields, span })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let expr = self.parse_expression()?;
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after expression.",
            Some("while parsing an expression statement"),
        )?;
        let span = Span::new(expr.span().start, self.previous().span.end);
        Ok(ExpressionStatement {
            expression: expr,
            span,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        // Check for lambda expression
        if self.check(TokenType::OpenParen) {
            if self.peek_next().token_type == TokenType::CloseParen {
                if self
                    .tokens
                    .get(self.current + 2)
                    .map_or(false, |t| t.token_type == TokenType::FatArrow)
                {
                    return self.parse_function_expression();
                }
            } else if self.peek_next().token_type.is_identifier() {
                if self
                    .tokens
                    .get(self.current + 2)
                    .map_or(false, |t| t.token_type == TokenType::Colon)
                {
                    return self.parse_function_expression();
                }
            }
        }

        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_logical_or_expression()?;

        if self.match_token(&[
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::StarEqual,
            TokenType::SlashEqual,
        ]) {
            let op_token = self.previous().clone();
            let right = self.parse_assignment_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match op_token.token_type {
                TokenType::PlusEqual => BinaryOperator::AddAssign,
                TokenType::MinusEqual => BinaryOperator::SubtractAssign,
                TokenType::StarEqual => BinaryOperator::MultiplyAssign,
                TokenType::SlashEqual => BinaryOperator::DivideAssign,
                _ => unreachable!(),
            };
            return Ok(Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            }));
        }

        Ok(expr)
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_logical_and_expression()?;
        while self.match_token(&[TokenType::PipePipe]) {
            let right = self.parse_logical_and_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_logical_and_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_equality_expression()?;
        while self.match_token(&[TokenType::AmpAmp]) {
            let right = self.parse_equality_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_comparison_expression()?;
        while self.match_token(&[TokenType::Equal, TokenType::NotEqual]) {
            let operator_token = self.previous().clone();
            let right = self.parse_comparison_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Equal => BinaryOperator::Equal,
                TokenType::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_comparison_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_additive_expression()?;
        while self.match_token(&[
            TokenType::LessThan,
            TokenType::LessThanEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanEqual,
        ]) {
            let operator_token = self.previous().clone();
            let right = self.parse_additive_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::LessThan => BinaryOperator::LessThan,
                TokenType::LessThanEqual => BinaryOperator::LessThanEqual,
                TokenType::GreaterThan => BinaryOperator::GreaterThan,
                TokenType::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_additive_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_multiplicative_expression()?;
        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let operator_token = self.previous().clone();
            let right = self.parse_multiplicative_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_unary_expression()?;
        while self.match_token(&[TokenType::Star, TokenType::Slash]) {
            let operator_token = self.previous().clone();
            let right = self.parse_unary_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            expr = Expression::Binary(BinaryExpression {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span,
            });
        }
        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator_token = self.previous().clone();
            let right = self.parse_unary_expression()?;
            let span = Span::new(operator_token.span.start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Bang => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Minus,
                _ => unreachable!(),
            };
            return Ok(Expression::Unary(UnaryExpression {
                operator,
                right: Box::new(right),
                span,
            }));
        }
        self.parse_postfix_expression()
    }

    fn parse_postfix_expression(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_primary_expression()?;

        let mut operators = Vec::new();
        while self.match_token(&[
            TokenType::Dot,
            TokenType::DoubleColon,
            TokenType::OpenParen,
            TokenType::OpenBracket,
        ]) {
            let operator_token = self.previous().clone();
            let operator = match operator_token.token_type {
                TokenType::Dot => {
                    let name_token = self.peek().clone();
                    let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                        self.advance();
                        s
                    } else {
                        let msg = format!(
                            "Expected identifier after '.', but found {:?}.",
                            name_token.token_type
                        );
                        return Err(self.error(
                            name_token.span,
                            msg,
                            Some("while parsing a field access expression"),
                        ));
                    };
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::FieldAccess { name, span }
                }
                TokenType::DoubleColon => {
                    let name_token = self.peek().clone();
                    let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                        self.advance();
                        s
                    } else {
                        let msg = format!(
                            "Expected identifier after '::', but found {:?}.",
                            name_token.token_type
                        );
                        return Err(self.error(
                            name_token.span,
                            msg,
                            Some("while parsing a type path expression"),
                        ));
                    };
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::TypePath { name, span }
                }
                TokenType::OpenParen => {
                    let mut args = Vec::new();
                    while !self.check(TokenType::CloseParen) && !self.is_at_end() {
                        args.push(self.parse_expression()?);
                        if !self.match_token(&[TokenType::Comma]) {
                            break;
                        }
                    }
                    self.consume(
                        TokenType::CloseParen,
                        "Expected ')' after arguments.",
                        Some("while parsing a function call"),
                    )?;
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::Call { args, span }
                }
                TokenType::OpenBracket => {
                    let index = self.parse_expression()?;
                    self.consume(
                        TokenType::CloseBracket,
                        "Expected ']' after index.",
                        Some("while parsing a list indexing expression"),
                    )?;
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::ListAccess {
                        index: Box::new(index),
                        span,
                    }
                }
                _ => unreachable!(),
            };
            operators.push(operator);
        }

        if operators.is_empty() {
            Ok(expr)
        } else {
            let span = Span::new(expr.span().start, self.previous().span.end);
            Ok(Expression::Postfix(crate::ast::PostfixExpression {
                primary: Box::new(expr),
                operators,
                span,
            }))
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.peek().clone();
        let span = token.span;

        match &token.token_type {
            TokenType::KeywordIf => self.parse_if_expression(),
            TokenType::KeywordMatch => self.parse_match_expression(),
            TokenType::Integer(i) => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Literal(
                    LiteralValue::Integer(*i),
                    span,
                )))
            }
            TokenType::Float(f) => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Literal(
                    LiteralValue::Float(*f),
                    span,
                )))
            }
            TokenType::String(s) => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Literal(
                    LiteralValue::String(s.clone()),
                    span,
                )))
            }
            TokenType::KeywordTrue => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Literal(
                    LiteralValue::Boolean(true),
                    span,
                )))
            }
            TokenType::KeywordFalse => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Literal(
                    LiteralValue::Boolean(false),
                    span,
                )))
            }
            TokenType::KeywordNone => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Literal(
                    LiteralValue::None,
                    span,
                )))
            }
            TokenType::KeywordThis => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::This(span)))
            }
            TokenType::OpenParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(
                    TokenType::CloseParen,
                    "Expected ')' after expression.",
                    Some("while parsing a parenthesized expression"),
                )?;
                let end_span = self.previous().span;
                let full_span = Span::new(span.start, end_span.end);
                Ok(Expression::Primary(PrimaryExpression::Parenthesized(
                    Box::new(expr),
                    full_span,
                )))
            }
            TokenType::Identifier(name) => {
                self.advance();
                Ok(Expression::Primary(PrimaryExpression::Identifier(
                    name.clone(),
                    span,
                )))
            }
            TokenType::OpenBrace => self.parse_brace_expression(),
            TokenType::OpenBracket => self.parse_list_literal(),
            _ => {
                let msg = format!("Expected expression, but found {:?}.", token.token_type);
                Err(self.error(span, msg, Some("while parsing an expression")))
            }
        }
    }

    fn parse_brace_expression(&mut self) -> Result<Expression, ParseError> {
        let saved_pos = self.current;
        self.advance(); // consume '{'

        if self.check(TokenType::CloseBrace) {
            self.current = saved_pos;
            return self.parse_block_expression();
        }

        if self.peek().token_type.is_identifier() && self.peek_next().token_type == TokenType::Colon
        {
            self.current = saved_pos;
            self.parse_record_literal()
        } else {
            self.current = saved_pos;
            self.parse_block_expression()
        }
    }

    fn parse_block_expression(&mut self) -> Result<Expression, ParseError> {
        let block = self.parse_block()?;
        Ok(Expression::Block(block))
    }

    fn parse_list_literal(&mut self) -> Result<Expression, ParseError> {
        let start_span = self
            .consume(
                TokenType::OpenBracket,
                "Expected '[' to start list literal.",
                Some("while parsing a list literal"),
            )?
            .span;

        let mut elements = Vec::new();

        while !self.check(TokenType::CloseBracket) && !self.is_at_end() {
            elements.push(self.parse_expression()?);
            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBracket,
                "Expected ']' to end list literal.",
                Some("while parsing a list literal"),
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(Expression::Primary(PrimaryExpression::List(ListLiteral {
            elements,
            span,
        })))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let start_span = self
            .consume(
                TokenType::KeywordIf,
                "Expected 'if' keyword.",
                Some("while parsing an if expression"),
            )?
            .span;

        self.consume(
            TokenType::OpenParen,
            "Expected '(' after 'if'.",
            Some("while parsing an if expression"),
        )?;

        let condition = self.parse_expression()?;

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after if condition.",
            Some("while parsing an if expression"),
        )?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.match_token(&[TokenType::KeywordElse]) {
            if self.check(TokenType::KeywordIf) {
                let else_if_expr = self.parse_if_expression()?;
                Some(Box::new(else_if_expr))
            } else {
                Some(Box::new(Expression::Block(self.parse_block()?)))
            }
        } else {
            None
        };

        let end_span = else_branch
            .as_ref()
            .map(|e| e.span().end)
            .unwrap_or(then_branch.span.end);
        let span = Span::new(start_span.start, end_span);

        Ok(Expression::If(IfExpression {
            condition: Box::new(condition),
            then_branch,
            else_branch,
            span,
        }))
    }

    fn parse_match_expression(&mut self) -> Result<Expression, ParseError> {
        let start_span = self
            .consume(
                TokenType::KeywordMatch,
                "Expected 'match' keyword.",
                Some("while parsing a match expression"),
            )?
            .span;

        self.consume(
            TokenType::OpenParen,
            "Expected '(' after 'match'.",
            Some("while parsing a match expression"),
        )?;

        let value = self.parse_expression()?;

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after match scrutinee.",
            Some("while parsing a match expression"),
        )?;

        self.consume(
            TokenType::OpenBrace,
            "Expected '{' to start match arms.",
            Some("while parsing a match expression"),
        )?;

        let mut arms = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            self.consume(
                TokenType::Pipe,
                "Expected '|' before match arm.",
                Some("while parsing a match expression"),
            )?;

            let pattern = self.parse_pattern()?;

            self.consume(
                TokenType::FatArrow,
                "Expected '=>' after pattern.",
                Some("while parsing a match arm"),
            )?;

            let body_expr = self.parse_expression()?;
            let body_span = body_expr.span();
            let body = ExpressionOrBlock::Expression(Box::new(body_expr));

            arms.push(MatchArm {
                pattern: pattern.clone(),
                body,
                span: Span::new(pattern.span().start, body_span.end),
            });

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end match expression.",
                Some("while parsing a match expression"),
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(Expression::Match(MatchExpression {
            value: Box::new(value),
            arms,
            span,
        }))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let token = self.peek().clone();

        // Check for wildcard using KeywordUnderscore
        if token.token_type == TokenType::KeywordUnderscore {
            self.advance();
            return Ok(Pattern::Wildcard(token.span));
        }

        // Check for None keyword
        if token.token_type == TokenType::KeywordNone {
            self.advance();
            return Ok(Pattern::Identifier("None".to_string(), token.span));
        }

        // Literal patterns
        match &token.token_type {
            TokenType::Integer(i) => {
                self.advance();
                return Ok(Pattern::Literal(LiteralValue::Integer(*i), token.span));
            }
            TokenType::Float(f) => {
                self.advance();
                return Ok(Pattern::Literal(LiteralValue::Float(*f), token.span));
            }
            TokenType::String(s) => {
                self.advance();
                return Ok(Pattern::Literal(
                    LiteralValue::String(s.clone()),
                    token.span,
                ));
            }
            TokenType::KeywordTrue => {
                self.advance();
                return Ok(Pattern::Literal(LiteralValue::Boolean(true), token.span));
            }
            TokenType::KeywordFalse => {
                self.advance();
                return Ok(Pattern::Literal(LiteralValue::Boolean(false), token.span));
            }
            TokenType::KeywordNone => {
                self.advance();
                return Ok(Pattern::Literal(LiteralValue::None, token.span));
            }
            TokenType::Identifier(name) => {
                self.advance();

                // Check for variant with payload: Some(x)
                if self.check(TokenType::OpenParen) {
                    self.advance();
                    let mut patterns = Vec::new();
                    while !self.check(TokenType::CloseParen) && !self.is_at_end() {
                        patterns.push(self.parse_pattern()?);
                        if !self.match_token(&[TokenType::Comma]) {
                            break;
                        }
                    }
                    self.consume(
                        TokenType::CloseParen,
                        "Expected ')' after pattern.",
                        Some("while parsing a pattern"),
                    )?;
                    let end_span = self.previous().span;
                    let span = Span::new(token.span.start, end_span.end);
                    return Ok(Pattern::Variant {
                        name: name.clone(),
                        patterns: Some(patterns),
                        span,
                    });
                } else {
                    return Ok(Pattern::Identifier(name.clone(), token.span));
                }
            }
            _ => {}
        }

        let msg = format!("Expected pattern, but found {:?}.", token.token_type);
        Err(self.error(token.span, msg, Some("while parsing a pattern")))
    }

    fn parse_while_statement(&mut self) -> Result<Expression, ParseError> {
        let start_token = self.advance().clone(); // consume "while"

        self.consume(
            TokenType::OpenParen,
            "Expected '(' after 'while'.",
            Some("while parsing a while loop"),
        )?;

        let condition = self.parse_expression()?;

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after while condition.",
            Some("while parsing a while loop"),
        )?;

        let body = self.parse_block()?;
        let span = Span::new(start_token.span.start, body.span.end);

        Ok(Expression::While(WhileExpression {
            condition: Box::new(condition),
            body,
            span,
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Expression, ParseError> {
        let start_token = self.advance().clone(); // consume "for"

        let pattern = self.parse_pattern()?;

        // Consume "in" as contextual keyword
        if !self.is_contextual_keyword("in") {
            let token = self.peek().clone();
            let msg = format!(
                "Expected 'in' after loop variable, but found {:?}.",
                token.token_type
            );
            return Err(self.error(token.span, msg, Some("while parsing a for loop")));
        }
        self.advance();

        let iterable = self.parse_expression()?;
        let body = self.parse_block()?;
        let span = Span::new(start_token.span.start, body.span.end);

        Ok(Expression::For(ForExpression {
            pattern,
            iterable: Box::new(iterable),
            body,
            span,
        }))
    }

    fn parse_record_literal(&mut self) -> Result<Expression, ParseError> {
        let start_span = self
            .consume(
                TokenType::OpenBrace,
                "Expected '{' to start a record literal.",
                Some("while parsing a record literal"),
            )?
            .span;

        let mut fields = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let name_token = self.peek().clone();
            let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                self.advance();
                s
            } else {
                let msg = format!(
                    "Expected identifier for field name in record literal, but found {:?}.",
                    name_token.token_type
                );
                return Err(self.error(
                    name_token.span,
                    msg,
                    Some("while parsing a record literal"),
                ));
            };

            let name_span = self.previous().span;

            self.consume(
                TokenType::Colon,
                "Expected ':' after field name in record literal.",
                Some("while parsing a record literal"),
            )?;

            let value = self.parse_expression()?;
            let field_span = Span::new(name_span.start, value.span().end);

            fields.push(FieldInitializer {
                name,
                value,
                span: field_span,
            });

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end a record literal.",
                Some("while parsing a record literal"),
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(Expression::Primary(PrimaryExpression::Record(
            RecordLiteral { fields, span },
        )))
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParseError> {
        let params = self.parse_parameters()?;
        let return_type = if self.match_token(&[TokenType::Colon]) {
            self.parse_type()?
        } else {
            Type::Primary(TypePrimary::Named(
                "unit".to_string(),
                Span { start: 0, end: 0 },
            ))
        };

        self.consume(
            TokenType::FatArrow,
            "Expected '=>' for lambda expression body.",
            Some("while parsing a lambda expression"),
        )?;

        let body = if self.check(TokenType::OpenBrace) {
            ExpressionOrBlock::Block(self.parse_block()?)
        } else {
            ExpressionOrBlock::Expression(Box::new(self.parse_expression()?))
        };

        let span = Span::new(self.previous().span.start, self.previous().span.end);

        Ok(Expression::Lambda(LambdaExpression {
            params,
            return_type_annotation: Some(return_type),
            body,
            span,
        }))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        self.consume(
            TokenType::OpenParen,
            "Expected '(' to start a parameter list.",
            Some("while parsing a parameter list"),
        )?;

        let mut params = Vec::new();

        while !self.check(TokenType::CloseParen) && !self.is_at_end() {
            let name_token = self.peek().clone();
            let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                self.advance();
                s
            } else {
                let msg = format!(
                    "Expected identifier in parameter list, but found {:?}.",
                    name_token.token_type
                );
                return Err(self.error(
                    name_token.span,
                    msg,
                    Some("while parsing a parameter list"),
                ));
            };

            self.consume(
                TokenType::Colon,
                "Expected ':' after parameter name.",
                Some("while parsing a parameter list"),
            )?;

            let type_ = self.parse_type()?;
            let span = Span::new(name_token.span.start, type_.span().end);

            params.push(Parameter {
                name,
                ty: type_,
                span,
            });

            if !self.match_token(&[TokenType::Comma]) {
                break;
            }
        }

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after parameters.",
            Some("while parsing a parameter list"),
        )?;

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start_span = self
            .consume(
                TokenType::OpenBrace,
                "Expected '{' to start a block.",
                Some("while parsing a block"),
            )?
            .span;

        let mut statements = Vec::new();
        let mut final_expression = None;

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            // Check if this is a statement that needs special parsing (let/function/while/for)
            let is_special_statement = self.peek().token_type == TokenType::KeywordMut
                || self.check(TokenType::KeywordWhile)
                || self.is_contextual_keyword("for")
                || (self.peek().token_type.is_identifier()
                    && self.peek_next().token_type == TokenType::OpenParen
                    && self.looks_like_function_definition())
                || (self.peek().token_type.is_identifier()
                    && (self.peek_next().token_type == TokenType::Assign
                        || self.peek_next().token_type == TokenType::Colon));

            if is_special_statement {
                statements.push(self.parse_statement()?);
                continue;
            }

            // Parse expression and check for semicolon
            let expr = self.parse_expression()?;

            if self.match_token(&[TokenType::Semicolon]) {
                // Expression statement with semicolon
                let span = Span::new(expr.span().start, self.previous().span.end);
                statements.push(Statement::Expression(ExpressionStatement {
                    expression: expr,
                    span,
                }));
            } else if self.check(TokenType::CloseBrace) {
                // Final expression without semicolon
                final_expression = Some(Box::new(expr));
                break;
            } else {
                // Error: expected semicolon or close brace
                let found = self.peek().clone();
                return Err(self.error(
                    found.span,
                    format!("Expected ';' or '}}', but found {:?}", found.token_type),
                    Some("while parsing a block"),
                ));
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end a block.",
                Some("while parsing a block"),
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(Block {
            statements,
            final_expression,
            span,
        })
    }

    // --- Helper methods ---

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn peek_next(&self) -> &Token {
        if self.current + 1 >= self.tokens.len() {
            &self.tokens[self.tokens.len() - 1]
        } else {
            &self.tokens[self.current + 1]
        }
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EndOfFile
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }
}
