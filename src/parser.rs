use crate::ast::*;
use crate::diagnostics::{Diagnostic, DiagnosticKind, Reporter};
use crate::lexer::{Token, TokenType};
use std::cell::RefCell;
use std::rc::Rc;

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

/// RAII guard for parse contexts - automatically pops context on drop
pub struct ContextGuard {
    stack: Rc<RefCell<Vec<String>>>,
}

impl ContextGuard {
    fn new(stack: Rc<RefCell<Vec<String>>>, context: &str) -> Self {
        stack.borrow_mut().push(context.to_string());
        ContextGuard { stack }
    }
}

impl Drop for ContextGuard {
    fn drop(&mut self) {
        self.stack.borrow_mut().pop();
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    reporter: &'a mut Reporter,
    parse_stack: Rc<RefCell<Vec<String>>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], reporter: &'a mut Reporter) -> Self {
        Parser {
            tokens,
            current: 0,
            reporter,
            parse_stack: Rc::new(RefCell::new(Vec::new())),
        }
    }

    /// Create an RAII context guard
    fn context(&self, context: &str) -> ContextGuard {
        ContextGuard::new(Rc::clone(&self.parse_stack), context)
    }

    /// Get the current parsing context chain as a string
    fn current_context_chain(&self) -> String {
        let stack = self.parse_stack.borrow();
        if stack.is_empty() {
            "top level".to_string()
        } else {
            stack.join(" -> ")
        }
    }

    fn error(&mut self, span: Span, message: String, _context: Option<&str>) -> ParseError {
        let full_context = self.current_context_chain();
        let diagnostic = Diagnostic::new(DiagnosticKind::Error, message.clone(), span)
            .with_context(full_context.clone());

        self.reporter.add_diagnostic(diagnostic);
        ParseError::new(message, span, Some(full_context))
    }

    fn consume(
        &mut self,
        expected: TokenType,
        message: &str,
        _context: Option<&str>,
    ) -> Result<&Token, ParseError> {
        if self.check(expected.clone()) {
            Ok(self.advance())
        } else {
            let found = self.peek().clone();
            let full_message = format!("{} Found {:?} instead.", message, found.token_type);
            Err(self.error(found.span, full_message, None))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let _ctx = self.context("program");

        let mut statements = Vec::new();
        let start_span = self.peek().span;

        while !self.is_at_end() {
            let stmt = self.parse_top_statement()?;
            statements.push(stmt);
        }

        let end_span = self.previous().span;
        let program_span = Span::new(start_span.start, end_span.end);

        Ok(Program::new(statements, program_span))
    }

    fn parse_top_statement(&mut self) -> Result<TopStatement, ParseError> {
        let _ctx = self.context("top-level statement");

        // Handle type declarations
        if self.check(TokenType::KeywordType) {
            return self.parse_type_declaration().map(TopStatement::TypeDecl);
        }

        // Handle while loops
        if self.check(TokenType::KeywordWhile) {
            let expr = self.parse_while_statement()?;
            self.maybe_consume(&[TokenType::Semicolon]);
            let span = Span::new(expr.span().start, self.previous().span.end);
            return Ok(TopStatement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Handle for loops (contextual keyword)
        if self.check(TokenType::KeywordFor) {
            let expr = self.parse_for_statement()?;
            self.maybe_consume(&[TokenType::Semicolon]);
            let span = Span::new(expr.span().start, self.previous().span.end);
            return Ok(TopStatement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Handle if expressions
        if self.check(TokenType::KeywordIf) {
            let expr = self.parse_if_expression()?;
            self.maybe_consume(&[TokenType::Semicolon]);
            let span = Span::new(expr.span().start, self.previous().span.end);
            return Ok(TopStatement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Handle match expressions
        if self.check(TokenType::KeywordMatch) {
            let expr = self.parse_match_expression()?;
            self.maybe_consume(&[TokenType::Semicolon]);
            let span = Span::new(expr.span().start, self.previous().span.end);
            return Ok(TopStatement::Expression(ExpressionStatement {
                expression: expr,
                span,
            }));
        }

        // Disambiguate function definition vs function call/variable binding
        if self.peek().token_type.is_identifier()
            && self.peek_next().token_type == TokenType::OpenParen
        {
            if self.looks_like_function_definition() {
                let func_stmt = self.parse_function_statement()?;
                self.maybe_consume(&[TokenType::Semicolon]);
                return Ok(TopStatement::LetStmt(func_stmt));
            }
        }

        // Handle let/mut
        if self.peek().token_type == TokenType::KeywordMut {
            return self.parse_let_statement().map(TopStatement::LetStmt);
        }

        // Handle variable binding (identifier with : or =)
        if self.peek().token_type.is_identifier()
            && (self.peek_next().token_type == TokenType::Colon
                || self.peek_next().token_type == TokenType::Assign)
        {
            return self.parse_let_statement().map(TopStatement::LetStmt);
        }

        self.parse_expression_statement()
            .map(TopStatement::Expression)
    }

    fn looks_like_function_definition(&self) -> bool {
        let mut idx = self.current + 2; // Skip identifier and OpenParen

        // Empty params: `()` - check if followed by `:` or `=`
        if idx < self.tokens.len() && self.tokens[idx].token_type == TokenType::CloseParen {
            if idx + 1 < self.tokens.len() {
                let next = &self.tokens[idx + 1].token_type;
                return matches!(next, TokenType::Colon | TokenType::Assign);
            }
            return false;
        }

        // First token after ( should be an identifier or 'this' for a function definition
        if idx < self.tokens.len() {
            if !matches!(
                &self.tokens[idx].token_type,
                TokenType::Identifier(_) | TokenType::KeywordThis
            ) {
                // Not a function definition - might be a call with a literal argument
                return false;
            }
        }

        // Scan through parameters looking for type annotations or return type
        let mut depth = 1; // We're inside the opening paren
        while idx < self.tokens.len() && depth > 0 {
            match &self.tokens[idx].token_type {
                TokenType::OpenParen => depth += 1,
                TokenType::CloseParen => {
                    depth -= 1;
                    if depth == 0 {
                        // Found matching close paren, check what follows
                        if idx + 1 < self.tokens.len() {
                            let next = &self.tokens[idx + 1].token_type;
                            // Function def has `:` (return type) or `=` (body)
                            return matches!(next, TokenType::Colon | TokenType::Assign);
                        }
                        return false;
                    }
                }
                TokenType::Colon if depth == 1 => {
                    // Found a type annotation or return type
                    return true;
                }
                _ => {}
            }
            idx += 1;
        }

        false
    }

    fn parse_type_declaration(&mut self) -> Result<TypeDeclaration, ParseError> {
        let _ctx = self.context("type declaration");

        self.consume(TokenType::KeywordType, "Expected 'type' keyword.", None)?;

        let name_token = self.peek().clone();
        let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
            self.advance();
            s
        } else {
            let msg = format!("Expected type name, but found {:?}.", name_token.token_type);
            return Err(self.error(name_token.span, msg, None));
        };

        self.consume(TokenType::Assign, "Expected '=' after type name.", None)?;

        let constructor = self.parse_type_constructor()?;
        self.maybe_consume(&[TokenType::Semicolon]); // optional after type declaration
        let span = Span::new(name_token.span.start, self.previous().span.end);
        Ok(TypeDeclaration {
            name,
            constructor,
            span,
        })
    }

    fn parse_type_constructor(&mut self) -> Result<TypeConstructor, ParseError> {
        let _ctx = self.context("type constructor");

        // Case 1: RecordType (starts with '{')
        if self.check(TokenType::OpenBrace) {
            let record = self.parse_record_type()?;
            return Ok(TypeConstructor::Record(record));
        }

        // Case 2: List type or other type alias starting with '['
        if self.check(TokenType::OpenBracket) {
            let alias_type = self.parse_type()?;
            return Ok(TypeConstructor::Alias(alias_type));
        }

        // Case 3: Could be a sum type or type alias
        let saved_pos = self.current;

        if self.peek().token_type.is_identifier()
            || self.peek().token_type == TokenType::KeywordNone
        {
            let next = self.peek_next().token_type.clone();

            if next == TokenType::Pipe {
                match self.parse_variant() {
                    Ok(first_variant) => {
                        let mut variants = vec![first_variant.clone()];
                        while self.maybe_consume(&[TokenType::Pipe]) {
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
                    }
                    Err(e) => return Err(e),
                }
            } else if next == TokenType::OpenParen {
                match self.parse_variant() {
                    Ok(first_variant) => {
                        if self.check(TokenType::Pipe) {
                            let mut variants = vec![first_variant.clone()];
                            while self.maybe_consume(&[TokenType::Pipe]) {
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
                        let alias_type = self.parse_type()?;
                        return Ok(TypeConstructor::Alias(alias_type));
                    }
                }
            } else if next == TokenType::OpenBracket {
                let alias_type = self.parse_type()?;
                return Ok(TypeConstructor::Alias(alias_type));
            } else {
                match self.parse_variant() {
                    Ok(variant) => {
                        if self.check(TokenType::Pipe) {
                            let mut variants = vec![variant.clone()];
                            while self.maybe_consume(&[TokenType::Pipe]) {
                                variants.push(self.parse_variant()?);
                            }
                            let end_span = variants.last().map(|v| v.span).unwrap_or(variant.span);
                            let sum_span = Span::new(variant.span.start, end_span.end);
                            return Ok(TypeConstructor::Sum(SumConstructor {
                                variants,
                                span: sum_span,
                            }));
                        } else {
                            let span = variant.span;
                            return Ok(TypeConstructor::Sum(SumConstructor {
                                variants: vec![variant],
                                span,
                            }));
                        }
                    }
                    Err(_) => {
                        self.current = saved_pos;
                        let alias_type = self.parse_type()?;
                        return Ok(TypeConstructor::Alias(alias_type));
                    }
                }
            }
        }

        let alias_type = self.parse_type()?;
        Ok(TypeConstructor::Alias(alias_type))
    }

    fn parse_variant(&mut self) -> Result<Variant, ParseError> {
        let _ctx = self.context("variant");

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
                return Err(self.error(name_token.span, msg, None));
            }
        };

        let ty = if self.check(TokenType::OpenParen) {
            self.advance();
            let inner_type = self.parse_type()?;
            self.consume(
                TokenType::CloseParen,
                "Expected ')' after variant payload type.",
                None,
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
        let _ctx = self.context("statement");

        // Handle return statements
        if self.check(TokenType::KeywordReturn) {
            self.advance();
            let start = self.previous().span.start;
            let expr = if self.check(TokenType::Semicolon) {
                None
            } else {
                Some(self.parse_expression()?)
            };
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after return statement.",
                None,
            )?;
            let span = Span::new(start, self.previous().span.end);
            return Ok(Statement::Return(expr, span));
        }

        // Handle break statements
        if self.check(TokenType::KeywordBreak) {
            let start = self.advance().span.start;
            self.consume(TokenType::Semicolon, "Expected ';' after break.", None)?;
            let span = Span::new(start, self.previous().span.end);
            return Ok(Statement::Break(span));
        }

        // Handle continue statements
        if self.check(TokenType::KeywordContinue) {
            let start = self.advance().span.start;
            self.consume(TokenType::Semicolon, "Expected ';' after continue.", None)?;
            let span = Span::new(start, self.previous().span.end);
            return Ok(Statement::Continue(span));
        }

        // // Handle while loops
        // if self.check(TokenType::KeywordWhile) {
        //     let expr = self.parse_while_statement()?;
        //     self.match_token(&[TokenType::Semicolon]);
        //     let span = expr.span();
        //     return Ok(Statement::Expression(ExpressionStatement {
        //         expression: expr,
        //         span,
        //     }));
        // }

        // // Handle for loops
        // if self.is_contextual_keyword("for") {
        //     let expr = self.parse_for_statement()?;
        //     self.match_token(&[TokenType::Semicolon]);
        //     let span = expr.span();
        //     return Ok(Statement::Expression(ExpressionStatement {
        //         expression: expr,
        //         span,
        //     }));
        // }

        // Handle if expressions
        // if self.check(TokenType::KeywordIf) {
        //     let expr = self.parse_if_expression()?;
        //     self.match_token(&[TokenType::Semicolon]);
        //     let span = expr.span();
        //     return Ok(Statement::Expression(ExpressionStatement {
        //         expression: expr,
        //         span,
        //     }));
        // }

        // Handle match expressions
        // if self.check(TokenType::KeywordMatch) {
        //     let expr = self.parse_match_expression()?;
        //     self.match_token(&[TokenType::Semicolon]);
        //     let span = expr.span();
        //     return Ok(Statement::Expression(ExpressionStatement {
        //         expression: expr,
        //         span,
        //     }));
        // }

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

        // Variable binding with type annotation or assignment
        if self.peek().token_type.is_identifier() {
            let next = self.peek_next().token_type.clone();
            if next == TokenType::Colon {
                return self.parse_let_statement().map(Statement::Let);
            } else if next == TokenType::Assign {
                let saved = self.current;
                if let Ok(let_stmt) = self.parse_let_statement() {
                    return Ok(Statement::Let(let_stmt));
                }
                self.current = saved;
            }
        }

        self.parse_expression_statement().map(Statement::Expression)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let _ctx = self.context("let statement");

        let mutable = self.maybe_consume(&[TokenType::KeywordMut]);

        let name = if let TokenType::Identifier(s) = self.peek().token_type.clone() {
            self.advance();
            s
        } else {
            let token = self.peek().clone();
            let msg = format!(
                "Expected identifier for variable name in let statement, but found {:?}.",
                token.token_type
            );
            return Err(self.error(token.span, msg, None));
        };

        let type_annotation = if self.maybe_consume(&[TokenType::Colon]) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.consume(
            TokenType::Assign,
            "Expected '=' after identifier in let statement.",
            None,
        )?;

        let value = self.parse_expression()?;

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after expression in let statement.",
            None,
        )?;

        let span = Span::new(
            if mutable {
                self.tokens[self.current - (if type_annotation.is_some() { 6 } else { 4 })]
                    .span
                    .start
            } else {
                self.tokens[self.current - (if type_annotation.is_some() { 5 } else { 3 })]
                    .span
                    .start
            },
            self.previous().span.end,
        );

        Ok(LetStatement::Variable(VariableBinding {
            mutable,
            name,
            type_annotation,
            value,
            span,
        }))
    }

    fn parse_function_statement(&mut self) -> Result<LetStatement, ParseError> {
        let _ctx = self.context("function declaration");

        let mutable = self.maybe_consume(&[TokenType::KeywordMut]);

        let name_token = self.peek().clone();
        let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
            self.advance();
            s
        } else {
            let msg = format!(
                "Expected identifier for function name, but found {:?}.",
                name_token.token_type
            );
            return Err(self.error(name_token.span, msg, None));
        };

        let params = self.parse_parameters()?;

        let return_type = if self.maybe_consume(&[TokenType::Colon]) {
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
            None,
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
        let _ctx = self.context("type annotation");

        let primary = self.parse_type_primary()?;

        if self.maybe_consume(&[TokenType::Arrow]) {
            let return_type = self.parse_type()?;
            let span = Span::new(primary.span().start, return_type.span().end);
            return Ok(Type::Function {
                params: vec![Type::Primary(primary)],
                return_type: Box::new(return_type),
                span,
            });
        }

        Ok(Type::Primary(primary))
    }

    fn parse_type_primary(&mut self) -> Result<TypePrimary, ParseError> {
        let token = self.peek().clone();
        let span = token.span;

        match &token.token_type {
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance();
                if self.check(TokenType::OpenBracket) {
                    let _ctx = self.context("generic type");
                    self.advance();
                    let mut args = Vec::new();
                    while !self.check(TokenType::CloseBracket) && !self.is_at_end() {
                        args.push(self.parse_type()?);
                        if !self.maybe_consume(&[TokenType::Comma]) {
                            break;
                        }
                    }
                    self.consume(
                        TokenType::CloseBracket,
                        "Expected ']' after generic type arguments.",
                        None,
                    )?;
                    Ok(TypePrimary::Generic {
                        name,
                        args,
                        span: Span::new(span.start, self.previous().span.end),
                    })
                } else {
                    Ok(TypePrimary::Named(name, span))
                }
            }
            TokenType::OpenBracket => {
                let _ctx = self.context("list type");
                self.advance();
                let inner_type = self.parse_type()?;
                self.consume(
                    TokenType::CloseBracket,
                    "Expected ']' after list type.",
                    None,
                )?;
                Ok(TypePrimary::List(
                    Box::new(inner_type),
                    Span::new(span.start, self.previous().span.end),
                ))
            }
            TokenType::OpenBrace => {
                let record_type = self.parse_record_type()?;
                Ok(TypePrimary::Record(record_type))
            }
            _ => {
                let msg = format!("Expected type, but found {:?}.", token.token_type);
                Err(self.error(span, msg, None))
            }
        }
    }

    fn parse_record_type(&mut self) -> Result<RecordType, ParseError> {
        let _ctx = self.context("record type");

        let start_span = self
            .consume(
                TokenType::OpenBrace,
                "Expected '{' to start a record type.",
                None,
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
                return Err(self.error(name_token.span, msg, None));
            };

            let name_span = self.previous().span;

            if self.check(TokenType::OpenParen) {
                let _ctx = self.context("record method");
                let params = self.parse_parameters()?;

                self.consume(
                    TokenType::Colon,
                    "Expected ':' after method parameters.",
                    None,
                )?;

                let return_type = self.parse_type()?;

                self.consume(
                    TokenType::Assign,
                    "Expected '=' after method signature.",
                    None,
                )?;

                let body = self.parse_block()?;
                let method_span = Span::new(name_span.start, body.span.end);

                fields.push(FieldDeclaration {
                    name: name.clone(),
                    ty: Type::Function {
                        params: params
                            .iter()
                            .map(|p| {
                                Type::Primary(TypePrimary::Named(format!("{}", p.name), p.span))
                            })
                            .collect(),
                        return_type: Box::new(return_type),
                        span: method_span,
                    },
                    span: method_span,
                });
            } else {
                self.consume(
                    TokenType::Colon,
                    "Expected ':' after field name in record type.",
                    None,
                )?;

                let type_ = self.parse_type()?;
                let field_span = Span::new(name_span.start, type_.span().end);

                fields.push(FieldDeclaration {
                    name,
                    ty: type_,
                    span: field_span,
                });
            }

            if !self.maybe_consume(&[TokenType::Comma]) {
                self.maybe_consume(&[TokenType::Semicolon]);
                if !self.check(TokenType::CloseBrace) {
                    break;
                }
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end a record type.",
                None,
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(RecordType { fields, span })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let _ctx = self.context("expression statement");

        let expr = self.parse_expression()?;

        if !self.is_at_end() {
            self.consume(TokenType::Semicolon, "Expected ';' after expression.", None)?;
        } else {
            // Optional semicolon at EOF
            self.maybe_consume(&[TokenType::Semicolon]);
        }

        let span = Span::new(expr.span().start, self.previous().span.end);
        Ok(ExpressionStatement {
            expression: expr,
            span,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let _ctx = self.context("expression");

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
        let expr = self.parse_range_expression()?;

        if self.maybe_consume(&[
            TokenType::Assign,
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::StarEqual,
            TokenType::SlashEqual,
            TokenType::PercentEqual,
        ]) {
            let op_token = self.previous().clone();
            let right = self.parse_assignment_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match op_token.token_type {
                TokenType::Assign => BinaryOperator::Assign,
                TokenType::PlusEqual => BinaryOperator::AddAssign,
                TokenType::MinusEqual => BinaryOperator::SubtractAssign,
                TokenType::StarEqual => BinaryOperator::MultiplyAssign,
                TokenType::SlashEqual => BinaryOperator::DivideAssign,
                TokenType::PercentEqual => BinaryOperator::ModuloAssign,
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

    fn parse_range_expression(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_logical_or_expression()?;

        if self.maybe_consume(&[TokenType::DotDotLess, TokenType::DotDotEqual]) {
            let operator_token = self.previous().clone();
            let inclusive = operator_token.token_type == TokenType::DotDotEqual;
            let end = self.parse_logical_or_expression()?;
            let span = Span::new(expr.span().start, end.span().end);

            return Ok(Expression::Range(RangeExpression {
                start: Box::new(expr),
                end: Box::new(end),
                inclusive,
                span,
            }));
        }

        Ok(expr)
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_logical_and_expression()?;
        while self.maybe_consume(&[TokenType::PipePipe]) {
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
        while self.maybe_consume(&[TokenType::AmpAmp]) {
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
        while self.maybe_consume(&[TokenType::Equal, TokenType::NotEqual]) {
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
        while self.maybe_consume(&[
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
        while self.maybe_consume(&[TokenType::Plus, TokenType::Minus]) {
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
        while self.maybe_consume(&[TokenType::Star, TokenType::Slash, TokenType::Percent]) {
            let operator_token = self.previous().clone();
            let right = self.parse_unary_expression()?;
            let span = Span::new(expr.span().start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                TokenType::Percent => BinaryOperator::Modulo,
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
        if self.maybe_consume(&[TokenType::Bang, TokenType::Minus, TokenType::Plus]) {
            let operator_token = self.previous().clone();
            let right = self.parse_unary_expression()?;
            let span = Span::new(operator_token.span.start, right.span().end);
            let operator = match operator_token.token_type {
                TokenType::Bang => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Minus,
                TokenType::Plus => UnaryOperator::Plus,
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
        while self.maybe_consume(&[
            TokenType::Dot,
            TokenType::DoubleColon,
            TokenType::OpenParen,
            TokenType::OpenBracket,
        ]) {
            let operator_token = self.previous().clone();
            let operator = match operator_token.token_type {
                TokenType::Dot => {
                    let _ctx = self.context("field access");
                    let name_token = self.peek().clone();
                    let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                        self.advance();
                        s
                    } else {
                        let msg = format!(
                            "Expected identifier after '.', but found {:?}.",
                            name_token.token_type
                        );
                        return Err(self.error(name_token.span, msg, None));
                    };
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::FieldAccess { name, span }
                }
                TokenType::DoubleColon => {
                    let _ctx = self.context("type path");
                    let name_token = self.peek().clone();
                    let name = if let TokenType::Identifier(s) = name_token.token_type.clone() {
                        self.advance();
                        s
                    } else {
                        let msg = format!(
                            "Expected identifier after '::', but found {:?}.",
                            name_token.token_type
                        );
                        return Err(self.error(name_token.span, msg, None));
                    };
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::TypePath { name, span }
                }
                TokenType::OpenParen => {
                    let _ctx = self.context("function call");
                    let mut args = Vec::new();
                    while !self.check(TokenType::CloseParen) && !self.is_at_end() {
                        args.push(self.parse_expression()?);
                        if !self.maybe_consume(&[TokenType::Comma]) {
                            break;
                        }
                    }
                    self.consume(TokenType::CloseParen, "Expected ')' after arguments.", None)?;
                    let span = Span::new(operator_token.span.start, self.previous().span.end);
                    PostfixOperator::Call { args, span }
                }
                TokenType::OpenBracket => {
                    let _ctx = self.context("list index");
                    let index = self.parse_expression()?;
                    self.consume(TokenType::CloseBracket, "Expected ']' after index.", None)?;
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
            TokenType::KeywordWhile => self.parse_while_statement(),
            TokenType::KeywordFor => self.parse_for_statement(),
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
                let _ctx = self.context("parenthesized expression");
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(
                    TokenType::CloseParen,
                    "Expected ')' after expression.",
                    None,
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
                Err(self.error(span, msg, None))
            }
        }
    }

    fn parse_brace_expression(&mut self) -> Result<Expression, ParseError> {
        let saved_pos = self.current;
        self.advance();

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
        let _ctx = self.context("list literal");

        let start_span = self
            .consume(
                TokenType::OpenBracket,
                "Expected '[' to start list literal.",
                None,
            )?
            .span;

        let mut elements = Vec::new();

        while !self.check(TokenType::CloseBracket) && !self.is_at_end() {
            elements.push(self.parse_expression()?);
            if !self.maybe_consume(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBracket,
                "Expected ']' to end list literal.",
                None,
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(Expression::Primary(PrimaryExpression::List(ListLiteral {
            elements,
            span,
        })))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        let _ctx = self.context("if expression");

        let start_span = self
            .consume(TokenType::KeywordIf, "Expected 'if' keyword.", None)?
            .span;

        let left_paren = self.maybe_consume(&[TokenType::OpenParen]);

        let condition = self.parse_expression()?;

        if left_paren {
            // If a left parenthesis was consumed, we expect a right parenthesis
            self.consume(
                TokenType::CloseParen,
                "Expected ')' after if condition.",
                None,
            )?;
        } else {
            // If no left parenthesis, we should NOT have a right parenthesis
            if self.maybe_consume(&[TokenType::CloseParen]) {
                let msg = "Unexpected ')' after if condition.".to_string();
                return Err(self.error(self.previous().span, msg, None));
            }
        }

        let then_branch = self.parse_block()?;

        let else_branch = if self.maybe_consume(&[TokenType::KeywordElse]) {
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
        let _ctx = self.context("match expression");

        let start_span = self
            .consume(TokenType::KeywordMatch, "Expected 'match' keyword.", None)?
            .span;

        self.consume(TokenType::OpenParen, "Expected '(' after 'match'.", None)?;

        let value = self.parse_expression()?;

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after match scrutinee.",
            None,
        )?;

        self.consume(
            TokenType::OpenBrace,
            "Expected '{' to start match arms.",
            None,
        )?;

        let mut arms = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let _ctx = self.context("match arm");

            // TODO: playing around with the grammar
            // self.consume(TokenType::Pipe, "Expected '|' before match arm.", None)?;
            // FOR NOW, make the '|' optional
            self.maybe_consume(&[TokenType::Pipe]);

            let pattern = self.parse_pattern()?;

            self.consume(TokenType::FatArrow, "Expected '=>' after pattern.", None)?;

            let body_expr = self.parse_expression()?;
            let body_span = body_expr.span();
            let body = ExpressionOrBlock::Expression(Box::new(body_expr));

            arms.push(MatchArm {
                pattern: pattern.clone(),
                body,
                span: Span::new(pattern.span().start, body_span.end),
            });

            if !self.maybe_consume(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end match expression.",
                None,
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
        let _ctx = self.context("pattern");

        let token = self.peek().clone();

        if token.token_type == TokenType::KeywordUnderscore {
            self.advance();
            return Ok(Pattern::Wildcard(token.span));
        }

        if token.token_type == TokenType::KeywordNone {
            self.advance();
            return Ok(Pattern::Identifier("None".to_string(), token.span));
        }

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

                if self.check(TokenType::OpenParen) {
                    self.advance();
                    let mut patterns = Vec::new();
                    while !self.check(TokenType::CloseParen) && !self.is_at_end() {
                        patterns.push(self.parse_pattern()?);
                        if !self.maybe_consume(&[TokenType::Comma]) {
                            break;
                        }
                    }
                    self.consume(TokenType::CloseParen, "Expected ')' after pattern.", None)?;
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
        Err(self.error(token.span, msg, None))
    }

    fn parse_while_statement(&mut self) -> Result<Expression, ParseError> {
        let _ctx = self.context("while loop");

        let start_token = self.advance().clone();

        self.consume(TokenType::OpenParen, "Expected '(' after 'while'.", None)?;

        let condition = self.parse_expression()?;

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after while condition.",
            None,
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
        let _ctx = self.context("for loop");

        let start_token = self.advance().clone();

        let pattern = self.parse_pattern()?;

        if !self.check(TokenType::KeywordIn) {
            let token = self.peek().clone();
            let msg = format!(
                "Expected 'in' after loop variable, but found {:?}.",
                token.token_type
            );
            return Err(self.error(token.span, msg, None));
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
        let _ctx = self.context("record literal");

        let start_span = self
            .consume(
                TokenType::OpenBrace,
                "Expected '{' to start a record literal.",
                None,
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
                return Err(self.error(name_token.span, msg, None));
            };

            let name_span = self.previous().span;

            self.consume(
                TokenType::Colon,
                "Expected ':' after field name in record literal.",
                None,
            )?;

            let value = self.parse_expression()?;
            let field_span = Span::new(name_span.start, value.span().end);

            fields.push(FieldInitializer {
                name,
                value,
                span: field_span,
            });

            if !self.maybe_consume(&[TokenType::Comma]) {
                break;
            }
        }

        let end_span = self
            .consume(
                TokenType::CloseBrace,
                "Expected '}' to end a record literal.",
                None,
            )?
            .span;

        let span = Span::new(start_span.start, end_span.end);

        Ok(Expression::Primary(PrimaryExpression::Record(
            RecordLiteral { fields, span },
        )))
    }

    fn parse_function_expression(&mut self) -> Result<Expression, ParseError> {
        let _ctx = self.context("lambda expression");

        let params = self.parse_parameters()?;
        let return_type = if self.maybe_consume(&[TokenType::Colon]) {
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
            None,
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
        let _ctx = self.context("parameter list");

        self.consume(
            TokenType::OpenParen,
            "Expected '(' to start a parameter list.",
            None,
        )?;

        let mut params = Vec::new();

        while !self.check(TokenType::CloseParen) && !self.is_at_end() {
            let name_token = self.peek().clone();
            let name = match &name_token.token_type {
                TokenType::Identifier(s) => {
                    self.advance();
                    s.clone()
                }
                TokenType::KeywordThis => {
                    // Allow 'this' as a parameter name
                    self.advance();
                    "this".to_string()
                }
                _ => {
                    let msg = format!(
                        "Expected identifier in parameter list, but found {:?}.",
                        name_token.token_type
                    );
                    return Err(self.error(name_token.span, msg, None));
                }
            };

            // Type annotation is optional
            let type_ = if self.maybe_consume(&[TokenType::Colon]) {
                self.parse_type()?
            } else {
                // No type annotation - use a placeholder or inferred type
                Type::Primary(TypePrimary::Named("inferred".to_string(), name_token.span))
            };

            let span = Span::new(name_token.span.start, type_.span().end);

            params.push(Parameter {
                name,
                ty: type_,
                span,
            });

            if !self.maybe_consume(&[TokenType::Comma]) {
                break;
            }
        }

        self.consume(
            TokenType::CloseParen,
            "Expected ')' after parameters.",
            None,
        )?;

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let _ctx = self.context("block");

        let start_span = self
            .consume(TokenType::OpenBrace, "Expected '{' to start a block.", None)?
            .span;

        let mut statements = Vec::new();
        let mut final_expression = None;

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            // Only these MUST be statements (cannot be final expressions)
            let must_be_statement = self.peek().token_type == TokenType::KeywordReturn
                || self.check(TokenType::KeywordBreak)
                || self.check(TokenType::KeywordContinue);

            if must_be_statement {
                statements.push(self.parse_statement()?);
                continue;
            }

            // Special case: 'mut' keyword starts a let statement
            if self.peek().token_type == TokenType::KeywordMut {
                statements.push(self.parse_statement()?);
                continue;
            }

            // Special case: identifier with : or identifier with ( that looks like function def
            if self.peek().token_type.is_identifier() {
                let next_token = self.peek_next().token_type.clone();
                if next_token == TokenType::Colon
                    || (next_token == TokenType::OpenParen && self.looks_like_function_definition())
                {
                    statements.push(self.parse_statement()?);
                    continue;
                }
            }

            // Try to parse as expression
            let expr = self.parse_expression()?;

            // Check if expression ends with a closing brace (control flow constructs)
            let expr_ends_with_brace = matches!(
                expr,
                Expression::If(_)
                    | Expression::While(_)
                    | Expression::For(_)
                    | Expression::Match(_)
                    | Expression::Block(_)
            );

            // Decide if it's a final expression or a statement
            if self.check(TokenType::CloseBrace) {
                // At end of block - this is the final expression
                final_expression = Some(Box::new(expr));
                break;
            } else if self.maybe_consume(&[TokenType::Semicolon]) {
                // Has semicolon - it's a statement
                let span = Span::new(expr.span().start, self.previous().span.end);
                statements.push(Statement::Expression(ExpressionStatement {
                    expression: expr,
                    span,
                }));
            } else if expr_ends_with_brace {
                // Expression ends with } and is not at end of block
                // Treat as statement (allows while/for/if/match to be followed by more code)
                let span = expr.span();
                statements.push(Statement::Expression(ExpressionStatement {
                    expression: expr,
                    span,
                }));
            } else {
                // No semicolon, not at end, doesn't end with brace - error
                let found = self.peek().clone();
                return Err(self.error(
                    found.span,
                    format!("Expected ';' or '}}', but found {:?}", found.token_type),
                    None,
                ));
            }
        }

        let end_span = self
            .consume(TokenType::CloseBrace, "Expected '}' to end a block.", None)?
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

    // Return true if the current token matches the given type
    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    // Return true if any of the given token types are matched and consumed
    fn maybe_consume(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }
}
