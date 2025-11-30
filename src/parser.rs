use crate::ast::{
    BinaryExpression, BinaryOperator, Block, Expression, ExpressionOrBlock, ExpressionStatement,
    FunctionBinding, LambdaExpression, LetStatement, LiteralValue, Parameter, PrimaryExpression,
    Program, Span, Statement, TopStatement, Type, TypePrimary, UnaryExpression, UnaryOperator,
    VariableBinding,
};
use crate::diagnostics::Reporter;
use crate::lexer::{Token, TokenType};

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

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements = Vec::new();
        let start_span = self.peek().span;

        while !self.is_at_end() {
            match self.parse_top_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e), // Propagate the first error
            }
        }

        let end_span = self.previous().span;
        let program_span = Span::new(start_span.start, end_span.end);

        Ok(Program::new(statements, program_span))
    }

    fn parse_top_statement(&mut self) -> Result<TopStatement, String> {
        if self.peek().token_type.is_identifier() && self.peek_next().token_type == TokenType::OpenParen {
            return self.parse_function_statement().map(TopStatement::LetStmt);
        }
        if self.peek().token_type == TokenType::KeywordMut {
            return self.parse_let_statement().map(TopStatement::LetStmt);
        }
        if self.peek().token_type.is_identifier() && (self.peek_next().token_type == TokenType::Assign || self.peek_next().token_type == TokenType::Colon) {
            return self.parse_let_statement().map(TopStatement::LetStmt);
        }

        self.parse_expression_statement()
            .map(TopStatement::Expression)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.peek().token_type.is_identifier() && self.peek_next().token_type == TokenType::OpenParen {
            return self.parse_function_statement().map(Statement::Let);
        }
        if self.peek().token_type == TokenType::KeywordMut {
            return self.parse_let_statement().map(Statement::Let);
        }
        if self.peek().token_type.is_identifier() && (self.peek_next().token_type == TokenType::Assign || self.peek_next().token_type == TokenType::Colon) {
            return self.parse_let_statement().map(Statement::Let);
        }

        self.parse_expression_statement()
            .map(Statement::Expression)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        let mutable = self.match_token(&[TokenType::KeywordMut]);
        let name = if let TokenType::Identifier(s) = self.peek().token_type.clone() {
            self.advance();
            s
        } else {
            return Err("Expected identifier.".to_string());
        };

        let type_annotation = if self.match_token(&[TokenType::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(TokenType::Assign, "Expected '=' after identifier.")?;

        let value = self.parse_expression()?;

        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;

        Ok(LetStatement::Variable(VariableBinding {
            mutable,
            name,
            type_annotation,
            value,
            span: Span { start: 0, end: 0 },
        }))
    }

    fn parse_function_statement(&mut self) -> Result<LetStatement, String> {
        let mutable = self.match_token(&[TokenType::KeywordMut]);
        let name = if let TokenType::Identifier(s) = self.peek().token_type.clone() {
            self.advance();
            s
        } else {
            return Err("Expected identifier for function name.".to_string());
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
        self.consume(TokenType::Assign, "Expected '=' after function signature.")?;
        let body = self.parse_block()?;
        let span = Span::new(self.previous().span.start, body.span.end);

        Ok(LetStatement::Function(FunctionBinding {
            mutable,
            name,
            params,
            return_type,
            body,
            span,
        }))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let token = self.advance();
        let span = token.span;
        let token_type_cloned = token.token_type.clone();

        match &token_type_cloned {
            TokenType::Identifier(name) => {
                Ok(Type::Primary(TypePrimary::Named(name.clone(), span)))
            }
            _ => {
                let error_span = span;
                self.error(
                    error_span,
                    &format!("Expected type, found {:?}", token_type_cloned),
                );
                Err("Expected type".to_string())
            }
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, String> {
        let expr = self.parse_expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
        let span = Span::new(expr.span().start, self.previous().span.end);
        Ok(ExpressionStatement {
            expression: expr,
            span,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        // Check for lambda expression: `(` `)` `=>` or `(` identifier `:`
        if self.check(TokenType::OpenParen) {
            if self.peek_next().token_type == TokenType::CloseParen {
                if self.tokens.get(self.current + 2).map_or(false, |t| t.token_type == TokenType::FatArrow) {
                    return self.parse_function_expression();
                }
            } else if self.peek_next().token_type.is_identifier() {
                 if self.tokens.get(self.current + 2).map_or(false, |t| t.token_type == TokenType::Colon) {
                    return self.parse_function_expression();
                }
            }
        }
        self.parse_logical_or_expression()
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and_expression()?;
        while self.match_token(&[TokenType::PipePipe]) {
            let _operator_token = self.previous().clone();
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

    fn parse_logical_and_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality_expression()?;
        while self.match_token(&[TokenType::AmpAmp]) {
            let _operator_token = self.previous().clone();
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

    fn parse_equality_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison_expression()?;
        while self.match_token(&[TokenType::Equal, TokenType::NotEqual]) {
            let _operator_token = self.previous().clone();
            let right = self.parse_comparison_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match _operator_token.token_type {
                TokenType::Equal => BinaryOperator::Equal,
                TokenType::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(), // Should not happen due to match_token
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

    fn parse_comparison_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_additive_expression()?;
        while self.match_token(&[
            TokenType::LessThan,
            TokenType::LessThanEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanEqual,
        ]) {
            let _operator_token = self.previous().clone();
            let right = self.parse_additive_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match _operator_token.token_type {
                TokenType::LessThan => BinaryOperator::LessThan,
                TokenType::LessThanEqual => BinaryOperator::LessThanEqual,
                TokenType::GreaterThan => BinaryOperator::GreaterThan,
                TokenType::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
                _ => unreachable!(), // Should not happen due to match_token
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

    fn parse_additive_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_multiplicative_expression()?;
        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let _operator_token = self.previous().clone();
            let right = self.parse_multiplicative_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match _operator_token.token_type {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Subtract,
                _ => unreachable!(), // Should not happen due to match_token
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

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary_expression()?;
        while self.match_token(&[TokenType::Star, TokenType::Slash]) {
            let _operator_token = self.previous().clone();
            let right = self.parse_unary_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match _operator_token.token_type {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => unreachable!(), // Should not happen due to match_token
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

    fn parse_unary_expression(&mut self) -> Result<Expression, String> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator_token = self.previous().clone();
            let right = self.parse_unary_expression()?; // Unary operators are right-associative
            let span = Span::new(operator_token.span.start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Bang => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Minus, // For now, treat - as UnaryOperator::Minus
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

    fn parse_postfix_expression(&mut self) -> Result<Expression, String> {
        let expr = self.parse_primary_expression()?;

        // For now, no postfix operators implemented yet, just return primary.
        // This will be expanded later for calls, field access, etc.
        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, String> {
        let token = self.peek().clone();
        let span = token.span;

        match &token.token_type {
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
            TokenType::OpenParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(TokenType::CloseParen, "Expected ')' after expression.")?;
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
            _ => {
                let error_span = span;
                self.error(
                    error_span,
                    &format!("Expected expression, found {:?}", token.token_type),
                );
                Err("Expected expression".to_string())
            }
        }
    }

    fn parse_function_expression(&mut self) -> Result<Expression, String> {
        let params = self.parse_parameters()?;
        let return_type = if self.match_token(&[TokenType::Colon]) {
            self.parse_type()?
        } else {
            Type::Primary(TypePrimary::Named(
                "unit".to_string(),
                Span { start: 0, end: 0 },
            ))
        };

        self.consume(TokenType::FatArrow, "Expected '=>' for lambda expression body.")?;

        let body = if self.check(TokenType::OpenBrace) {
            ExpressionOrBlock::Block(self.parse_block()?)
        } else {
            ExpressionOrBlock::Expression(Box::new(self.parse_expression()?))
        };

        let span = Span::new(self.previous().span.start, self.previous().span.end); // This needs to be improved

        Ok(Expression::Lambda(LambdaExpression {
            params,
            return_type_annotation: Some(return_type),
            body,
            span,
        }))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        self.consume(TokenType::OpenParen, "Expected '(' to start a parameter list.")?;
        let mut params = Vec::new();
        if !self.check(TokenType::CloseParen) {
            loop {
                let name = if let TokenType::Identifier(s) = self.peek().token_type.clone() {
                    self.advance();
                    s
                } else {
                    return Err("Expected identifier in parameter list.".to_string());
                };
                self.consume(TokenType::Colon, "Expected ':' after parameter name.")?;
                let type_ = self.parse_type()?;
                let span = Span::new(self.previous().span.start, type_.span().end);
                params.push(Parameter {
                    name,
                    ty: type_,
                    span,
                });
                if !self.match_token(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(TokenType::CloseParen, "Expected ')' after parameters.")?;
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block, String> {
        self.consume(TokenType::OpenBrace, "Expected '{' to start a block.")?;
        let mut statements = Vec::new();
        let mut final_expression = None;
        let start_span = self.previous().span;

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let statement = self.parse_statement()?;

            if let Statement::Expression(expr_stmt) = statement {
                if self.check(TokenType::CloseBrace) {
                    final_expression = Some(Box::new(expr_stmt.expression));
                    break;
                } else if self.check(TokenType::Semicolon) {
                    self.advance();
                    statements.push(Statement::Expression(expr_stmt));
                } else {
                    final_expression = Some(Box::new(expr_stmt.expression));
                    break;
                }
            } else {
                statements.push(statement);
            }
        }

        self.consume(TokenType::CloseBrace, "Expected '}' to end a block.")?;
        let end_span = self.previous().span;
        let span = Span::new(start_span.start, end_span.end);

        Ok(Block {
            statements,
            final_expression,
            span,
        })
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type.clone()) {
            Ok(self.advance())
        } else {
            let error_span = self.peek().span;
            self.error(error_span, message);
            Err(message.to_string())
        }
    }

    fn error(&mut self, span: Span, message: &str) {
        self.reporter
            .add_diagnostic(crate::diagnostics::Diagnostic::new(
                crate::diagnostics::DiagnosticKind::Error,
                message.to_string(),
                span,
            ));
    }

    // Helper methods for parser (peek, advance, check, consume, etc.) will go here
    fn peek(&mut self) -> &Token {
        // Changed to &mut self
        &self.tokens[self.current]
    }

    fn peek_next(&self) -> &Token {
        if self.current + 1 >= self.tokens.len() {
            &self.tokens[self.tokens.len() - 1] // Return EOF
        } else {
            &self.tokens[self.current + 1]
        }
    }

    fn previous(&mut self) -> &Token {
        // Changed to &mut self
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&mut self) -> bool {
        // Changed to &mut self
        self.peek().token_type == TokenType::EndOfFile
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        // Changed to &mut self
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type.clone()) {
                // Clone because TokenType can be Identifier(String)
                self.advance();
                return true;
            }
        }
        false
    }
}