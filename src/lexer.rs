use std::fmt;

use crate::ast::Span;
use crate::diagnostics::{Diagnostic, DiagnosticKind, Reporter};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    Semicolon,    // ;
    Colon,        // :
    Comma,        // ,
    Dot,          // .
    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]
    Bang,         // !

    // Operators
    Assign,           // =
    Equal,            // ==
    NotEqual,         // !=
    LessThan,         // <
    LessThanEqual,    // <=
    GreaterThan,      // >
    GreaterThanEqual, // >=
    Plus,             // +
    Minus,            // -
    Star,             // *
    Slash,            // /
    AmpAmp,           // &&
    PipePipe,         // ||

    // Compound assignment operators
    PlusEqual,  // +=
    MinusEqual, // -=
    StarEqual,  // *=
    SlashEqual, // /=

    // Fat arrow for lambdas and match arms
    FatArrow, // =>

    // Double colon for type paths
    DoubleColon, // ::

    // Literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),

    // Keywords
    KeywordType,       // type
    KeywordLet,        // let
    KeywordMut,        // mut
    KeywordIf,         // if
    KeywordElse,       // else
    KeywordWhile,      // while
    KeywordMatch,      // match
    KeywordFn,         // fn
    KeywordTrue,       // true
    KeywordFalse,      // false
    KeywordNone,       // None
    KeywordUnderscore, // _ (used in patterns)

    // End of File
    EndOfFile,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Identifier(s) => write!(f, "IDENTIFIER({})", s),
            TokenType::Integer(i) => write!(f, "INTEGER({})", i),
            TokenType::Float(fl) => write!(f, "FLOAT({})", fl),
            TokenType::String(s) => write!(f, "STRING(\"{}\")", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, span: Span) -> Self {
        Token {
            token_type,
            lexeme,
            span,
        }
    }
}

pub struct Lexer<'a> {
    chars: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    reporter: &'a mut Reporter,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str, reporter: &'a mut Reporter) -> Self {
        Lexer {
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1, // Lines are 1-indexed
            reporter,
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, Diagnostic> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(
            TokenType::EndOfFile,
            "".to_string(),
            Span::new(self.current, self.current),
        ));

        if self.reporter.has_errors() {
            Err(self.reporter.diagnostics.remove(0)) // Return first diagnostic if any
        } else {
            Ok(self.tokens)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text: String = self.chars[self.start..self.current].iter().collect();
        let span = Span::new(self.start, self.current);
        self.tokens.push(Token::new(token_type, text, span));
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.chars[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.chars[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            return '\0';
        }
        self.chars[self.current + 1]
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            ';' => self.add_token(TokenType::Semicolon),
            ',' => self.add_token(TokenType::Comma),
            '(' => self.add_token(TokenType::OpenParen),
            ')' => self.add_token(TokenType::CloseParen),
            '{' => self.add_token(TokenType::OpenBrace),
            '}' => self.add_token(TokenType::CloseBrace),
            '[' => self.add_token(TokenType::OpenBracket),
            ']' => self.add_token(TokenType::CloseBracket),
            '.' => self.add_token(TokenType::Dot),
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::Equal);
                } else if self.match_char('>') {
                    self.add_token(TokenType::FatArrow);
                } else {
                    self.add_token(TokenType::Assign);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessThanEqual);
                } else {
                    self.add_token(TokenType::LessThan);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterThanEqual);
                } else {
                    self.add_token(TokenType::GreaterThan);
                }
            }
            '+' => {
                if self.match_char('=') {
                    self.add_token(TokenType::PlusEqual);
                } else {
                    self.add_token(TokenType::Plus);
                }
            }
            '-' => {
                if self.match_char('=') {
                    self.add_token(TokenType::MinusEqual);
                } else {
                    self.add_token(TokenType::Minus);
                }
            }
            '*' => {
                if self.match_char('=') {
                    self.add_token(TokenType::StarEqual);
                } else {
                    self.add_token(TokenType::Star);
                }
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('=') {
                    self.add_token(TokenType::SlashEqual);
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::AmpAmp);
                } else {
                    // TODO: Report error for unexpected '&'
                    self.error(self.current - 1, "Unexpected character '&'.");
                }
            }
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::PipePipe);
                } else {
                    // TODO: This might be part of match arms. For now, report error.
                    self.error(self.current - 1, "Unexpected character '|'.");
                }
            }
            ':' => {
                if self.match_char(':') {
                    self.add_token(TokenType::DoubleColon);
                } else {
                    self.add_token(TokenType::Colon);
                }
            }
            // Whitespace
            ' ' | '\r' | '\t' => {} // Ignore whitespace
            '\n' => self.line += 1,

            // Literals
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),

            _ => self.error(self.current - 1, "Unexpected character."),
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error(self.start, "Unterminated string.");
            return;
        }

        self.advance(); // Consume the closing '"'

        let value: String = self.chars[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token(TokenType::String(value));
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let mut is_float = false;
        // Look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            self.advance(); // Consume the "."
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let value_str: String = self.chars[self.start..self.current].iter().collect();

        if is_float {
            match value_str.parse::<f64>() {
                Ok(value) => self.add_token(TokenType::Float(value)),
                Err(_) => self.error(self.start, "Invalid float literal."),
            }
        } else {
            match value_str.parse::<i64>() {
                Ok(value) => self.add_token(TokenType::Integer(value)),
                Err(_) => self.error(self.start, "Invalid integer literal."),
            }
        }
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text: String = self.chars[self.start..self.current].iter().collect();
        let token_type = match text.as_str() {
            "type" => TokenType::KeywordType,
            "let" => TokenType::KeywordLet,
            "mut" => TokenType::KeywordMut,
            "if" => TokenType::KeywordIf,
            "else" => TokenType::KeywordElse,
            "while" => TokenType::KeywordWhile,
            "match" => TokenType::KeywordMatch,
            "fn" => TokenType::KeywordFn,
            "true" => TokenType::KeywordTrue,
            "false" => TokenType::KeywordFalse,
            "None" => TokenType::KeywordNone,
            "_" => TokenType::KeywordUnderscore, // Explicit keyword for '_' pattern
            _ => TokenType::Identifier(text.clone()),
        };
        self.add_token(token_type);
    }

    fn error(&mut self, at: usize, message: &str) {
        self.reporter.add_diagnostic(
            Diagnostic::new(
                DiagnosticKind::Error,
                message.to_string(),
                Span::new(at, at + 1), // Single character error span
            )
            .with_context(format!("Error on line {}", self.line)),
        );
    }
}

