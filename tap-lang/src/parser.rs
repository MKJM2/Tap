use crate::ast::{Expression, LiteralValue, Operator, Program, Statement, TypeAnnotation};
use crate::lexer::{Token, TokenType};

/// The parser, responsible for turning a vector of tokens into an Abstract Syntax Tree (AST).
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

/// Represents an error that can occur during parsing.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: String,
        line: usize,
    },
    UnexpectedEof,
}

impl<'a> Parser<'a> {
    /// Creates a new `Parser`.
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    /// Parses the tokens into a program.
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if self.match_token(&TokenType::KeywordFunc) {
            return self.parse_function_def();
        }

        if self.match_token(&TokenType::KeywordStruct) {
            return self.parse_struct_declaration();
        }

        if self.match_token(&TokenType::KeywordEnum) {
            return self.parse_enum_declaration();
        }

        if self.check(&TokenType::Identifier) && self.peek_next_is(&TokenType::Colon) {
            let name = self.consume(&TokenType::Identifier, "Expect identifier.")?.lexeme.clone();
            self.consume(&TokenType::Colon, "Expect ':' for type annotation.")?;
            let type_annotation = self.parse_type_annotation()?;

            if self.match_token(&TokenType::Assign) {
                let value = self.parse_expression()?;
                self.consume(&TokenType::Semicolon, "Expect ';' after variable declaration.")?;
                return Ok(Statement::Assignment { name, value });
            } else {
                self.consume(&TokenType::Semicolon, "Expect ';' after variable declaration.")?;
                return Ok(Statement::VarDecl { name, type_annotation });
            }
        }

        let expr = self.parse_expression()?;

        let statement = if self.match_token(&TokenType::Assign) {
            let name = match expr {
                Expression::Identifier(name) => name,
                _ => return Err(ParseError::UnexpectedToken {
                    expected: "Identifier".to_string(),
                    found: format!("{:?}", expr),
                    line: self.previous().line,
                }),
            };
            let value = self.parse_expression()?;
            Statement::Assignment { name, value }
        } else if self.match_token(&TokenType::AddAssign) {
            let name = match expr {
                Expression::Identifier(name) => name,
                _ => return Err(ParseError::UnexpectedToken {
                    expected: "Identifier".to_string(),
                    found: format!("{:?}", expr),
                    line: self.previous().line,
                }),
            };
            let value = Expression::Binary {
                left: Box::new(Expression::Identifier(name.clone())),
                op: Operator::Add,
                right: Box::new(self.parse_expression()?),
            };
            Statement::Assignment { name, value }
        } else {
            Statement::Expression(expr)
        };

        self.consume(&TokenType::Semicolon, "Expect ';' after statement.")?;
        Ok(statement)
    }

    fn parse_enum_declaration(&mut self) -> Result<Statement, ParseError> {
        let name = self.consume(&TokenType::Identifier, "Expect enum name.")?.lexeme.clone();
        self.consume(&TokenType::OpenBrace, "Expect '{' before enum variants.")?;
        let mut variants = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                variants.push(self.consume(&TokenType::Identifier, "Expect variant name.")?.lexeme.clone());
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "Expect '}' after enum variants.")?;
        Ok(Statement::EnumDecl(crate::ast::EnumDecl { name, variants }))
    }


    fn parse_struct_declaration(&mut self) -> Result<Statement, ParseError> {
        let name = self.consume(&TokenType::Identifier, "Expect struct name.")?.lexeme.clone();
        self.consume(&TokenType::OpenBrace, "Expect '{' before struct fields.")?;
        let mut fields = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let field_name = self.consume(&TokenType::Identifier, "Expect field name.")?.lexeme.clone();
                self.consume(&TokenType::Colon, "Expect ':' after field name.")?;
                let type_annotation = self.parse_type_annotation()?;
                fields.push((field_name, type_annotation));
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "Expect '}' after struct fields.")?;
        self.consume(&TokenType::Semicolon, "Expect ';' after struct declaration.")?;
        Ok(Statement::StructDecl(crate::ast::StructDecl { name, fields }))
    }


    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, ParseError> {
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

    fn parse_primary_type(&mut self) -> Result<TypeAnnotation, ParseError> {
        if self.match_token(&TokenType::KeywordInt) {
            Ok(TypeAnnotation::Int)
        } else if self.match_token(&TokenType::KeywordStr) {
            Ok(TypeAnnotation::Str)
        } else if self.match_token(&TokenType::OpenBracket) {
            let inner_type = self.parse_type_annotation()?;
            self.consume(&TokenType::CloseBracket, "Expect ']' after type in array type.")?;
            Ok(TypeAnnotation::Array(Box::new(inner_type)))
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "type".to_string(),
                found: format!("{:?}", self.peek()),
                line: self.peek().map_or(0, |t| t.line),
            })
        }
    }

    fn parse_function_def(&mut self) -> Result<Statement, ParseError> {
        let name = self.consume(&TokenType::Identifier, "Expect function name.")?.lexeme.clone();
        self.consume(&TokenType::OpenParen, "Expect '(' after function name.")?;
        let mut args = Vec::new();
        if !self.check(&TokenType::CloseParen) {
            loop {
                // TODO: Add type annotations for args
                args.push(self.consume(&TokenType::Identifier, "Expect parameter name.")?.lexeme.clone());
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseParen, "Expect ')' after parameters.")?;
        self.consume(&TokenType::OpenBrace, "Expect '{' before function body.")?;
        let mut body = Vec::new();
        while !self.check(&TokenType::CloseBrace) {
            body.push(self.parse_statement()?);
        }
        self.consume(&TokenType::CloseBrace, "Expect '}' after function body.")?;

        Ok(Statement::FunctionDef(crate::ast::FunctionDef { name, args, body }))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_term()
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_factor()?;

        while self.match_tokens(&[TokenType::OpPlus, TokenType::OpMinus]) {
            let op = match self.previous().token_type {
                TokenType::OpPlus => Operator::Add,
                TokenType::OpMinus => Operator::Subtract,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary()?;

        while self.match_tokens(&[TokenType::OpMult, TokenType::OpDivide]) {
            let op = match self.previous().token_type {
                TokenType::OpMult => Operator::Multiply,
                TokenType::OpDivide => Operator::Divide,
                _ => unreachable!(),
            };
            let right = self.parse_primary()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let mut expr = if let Some(token) = self.peek() {
            match token.token_type.clone() {
                TokenType::Integer(val) => {
                    self.advance();
                    Expression::Literal(LiteralValue::Integer(val))
                }
                TokenType::String(val) => {
                    self.advance();
                    Expression::Literal(LiteralValue::String(val))
                }
                TokenType::Identifier => {
                    if self.peek_next_is(&TokenType::OpenBrace) {
                        return self.parse_struct_instantiation();
                    }
                    self.advance();
                    Expression::Identifier(self.previous().lexeme.clone())
                }
                TokenType::OpenParen => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    self.consume(&TokenType::CloseParen, "Expect ')' after expression.")?;
                    expr
                }
                TokenType::OpenBracket => self.parse_list_literal()?,
                TokenType::OpOr => self.parse_lambda_expression()?,
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "expression".to_string(),
                        found: format!("{:?}", token.token_type),
                        line: token.line,
                    })
                }
            }
        } else {
            return Err(ParseError::UnexpectedEof);
        };

        loop {
            if self.match_token(&TokenType::OpenParen) {
                expr = self.parse_call_expression(expr)?;
            } else if self.match_token(&TokenType::Period) {
                let name = self.consume(&TokenType::Identifier, "Expect property name after '.'.")?.lexeme.clone();
                expr = Expression::Get {
                    object: Box::new(expr),
                    name,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_struct_instantiation(&mut self) -> Result<Expression, ParseError> {
        let name = self.consume(&TokenType::Identifier, "Expect struct name.")?.lexeme.clone();
        self.consume(&TokenType::OpenBrace, "Expect '{' before struct fields.")?;
        let mut fields = Vec::new();
        if !self.check(&TokenType::CloseBrace) {
            loop {
                let field_name = self.consume(&TokenType::Identifier, "Expect field name.")?.lexeme.clone();
                self.consume(&TokenType::Colon, "Expect ':' after field name.")?;
                let value = self.parse_expression()?;
                fields.push((field_name, value));
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBrace, "Expect '}' after struct fields.")?;
        Ok(Expression::StructInstantiation { name, fields })
    }


    fn parse_call_expression(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        let mut args = Vec::new();
        if !self.check(&TokenType::CloseParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseParen, "Expect ')' after arguments.")?;

        Ok(Expression::FunctionCall {
            callee: Box::new(callee),
            args,
        })
    }


    fn parse_lambda_expression(&mut self) -> Result<Expression, ParseError> {
        self.consume(&TokenType::OpOr, "Expect '|' for lambda expression.")?;
        let mut args = Vec::new();
        if !self.check(&TokenType::OpOr) {
            loop {
                args.push(self.consume(&TokenType::Identifier, "Expect parameter name.")?.lexeme.clone());
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::OpOr, "Expect '|' after lambda parameters.")?;
        let body = self.parse_expression()?;
        Ok(Expression::Lambda {
            args,
            body: Box::new(body),
        })
    }


    fn parse_list_literal(&mut self) -> Result<Expression, ParseError> {
        self.consume(&TokenType::OpenBracket, "Expect '[' for list literal.")?;
        let mut elements = Vec::new();
        if !self.check(&TokenType::CloseBracket) {
            loop {
                elements.push(self.parse_expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::CloseBracket, "Expect ']' after list elements.")?;
        Ok(Expression::List(elements))
    }



    // Helper functions

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

    fn consume(&mut self, token_type: &TokenType, _message: &str) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            return Ok(self.advance());
        }

        // Semicolon relaxation for REPL
        if self.is_at_end() && std::mem::discriminant(token_type) == std::mem::discriminant(&TokenType::Semicolon) {
            // If we expect a semicolon but we are at the end, it's fine
            return Ok(self.peek().unwrap_or(&self.tokens[self.tokens.len() - 1]));
        }

        Err(ParseError::UnexpectedToken {
            expected: format!("{:?}", token_type),
            found: format!("{:?}", self.peek().unwrap().token_type),
            line: self.peek().unwrap().line,
        })
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        std::mem::discriminant(&self.peek().unwrap().token_type) == std::mem::discriminant(token_type)
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
        self.current >= self.tokens.len() || self.peek().unwrap().token_type == TokenType::EndOfFile
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_simple_expression() {
        let source = "5;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(expr) => match expr {
                Expression::Literal(LiteralValue::Integer(5)) => {} //
                _ => panic!("Expected integer literal"),
            },
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_addition() {
        let source = "1 + 5;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        // TODO: Add more assertions
    }

    #[test]
    fn test_assignment() {
        let source = "x = 5;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment { name, value } => {
                assert_eq!(name, "x");
                match value {
                    Expression::Literal(LiteralValue::Integer(5)) => {} //
                    _ => panic!("Expected integer literal"),
                }
            }
            _ => panic!("Expected assignment statement"),
        }
    }

    #[test]
    fn test_function_definition() {
        let source = "func my_func(a, b) { a + b; }";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
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

    #[test]
    fn test_function_call() {
        let source = "my_func(a, b);";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::FunctionCall { callee, args }) => {
                assert_eq!(**callee, Expression::Identifier("my_func".to_string()));
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_typed_assignment() {
        let source = "x : int = 5;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment { name, value } => {
                assert_eq!(name, "x");
                match value {
                    Expression::Literal(LiteralValue::Integer(5)) => {} //
                    _ => panic!("Expected integer literal"),
                }
            }
            _ => panic!("Expected assignment statement"),
        }
    }

    #[test]
    fn test_compound_assignment() {
        let source = "x += 1;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment { name, value } => {
                assert_eq!(name, "x");
                match value {
                    Expression::Binary { left, op, right } => {
                        assert_eq!(*op, Operator::Add);
                    }
                    _ => panic!("Expected binary expression"),
                }
            }
            _ => panic!("Expected assignment statement"),
        }
    }

    #[test]
    fn test_variable_declaration() {
        let source = "x : int;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::VarDecl { name, type_annotation } => {
                assert_eq!(name, "x");
                assert_eq!(*type_annotation, TypeAnnotation::Int);
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_function_type_annotation() {
        let source = "f : int -> int;";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::VarDecl { type_annotation, .. } => {
                assert_eq!(
                    *type_annotation,
                    TypeAnnotation::Function {
                        from: Box::new(TypeAnnotation::Int),
                        to: Box::new(TypeAnnotation::Int),
                    }
                );
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_array_type_annotation() {
        let source = "z : [str];";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::VarDecl { type_annotation, .. } => {
                assert_eq!(
                    *type_annotation,
                    TypeAnnotation::Array(Box::new(TypeAnnotation::Str))
                );
            }
            _ => panic!("Expected variable declaration"),
        }
    }

    #[test]
    fn test_list_literal() {
        let source = "[1, \"two\"];";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::List(elements)) => {
                assert_eq!(elements.len(), 2);
            }
            _ => panic!("Expected list literal expression"),
        }
    }
}
