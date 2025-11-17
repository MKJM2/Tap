use std::fmt;

/// Represents the different types of tokens that the lexer can produce.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier,
    Integer(i64),
    String(String),
    Semicolon,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    XorAssign,
    OrAssign,
    AndAssign,
    NegAssign,
    Colon,
    ColonColon,
    OpPlus,
    OpMinus,
    OpMult,
    OpDivide,
    OpFloorDiv,
    OpModulo,
    OpXor,
    OpOr,
    OpAnd,
    OpNeg,
    OpLor,
    OpLand,
    OpLneg,
    OpIncrement,
    OpDecrement,
    OpExponent,
    Arrow,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Period,
    Lambda,
    KeywordInt,
    KeywordStr,
    KeywordFunc,
    KeywordReturn,
    KeywordStruct,
    KeywordMatch,
    KeywordEnum,
    KeywordFor,
    KeywordWhile,
    KeywordContinue,
    KeywordBreak,
    KeywordIf,
    KeywordElse,
    KeywordTrue,
    KeywordFalse,
    Unknown,
    EndOfFile,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// Represents a single token produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

/// The lexer, responsible for turning a source string into a vector of tokens.
pub struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`.
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    /// Tokenizes the source string and returns a vector of tokens.
    pub fn tokenize(mut self) -> Result<Vec<Token>, String> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens
            .push(Token::new(TokenType::EndOfFile, "".to_string(), self.line));
        Ok(self.tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();
        match c {
            ';' => self.add_token(TokenType::Semicolon),
            ':' => {
                if self.match_char(':') {
                    self.add_token(TokenType::ColonColon)
                } else {
                    self.add_token(TokenType::Colon)
                }
            }
            '(' => self.add_token(TokenType::OpenParen),
            ')' => self.add_token(TokenType::CloseParen),
            '{' => self.add_token(TokenType::OpenBrace),
            '}' => self.add_token(TokenType::CloseBrace),
            '[' => self.add_token(TokenType::OpenBracket),
            ']' => self.add_token(TokenType::CloseBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Period),
            '\\' => self.add_token(TokenType::Lambda),
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::Equal)
                } else {
                    self.add_token(TokenType::Assign)
                }
            }
            '+' => {
                if self.match_char('=') {
                    self.add_token(TokenType::AddAssign)
                } else if self.match_char('+') {
                    self.add_token(TokenType::OpIncrement)
                } else {
                    self.add_token(TokenType::OpPlus)
                }
            }
            '-' => {
                if self.match_char('>') {
                    self.add_token(TokenType::Arrow)
                } else if self.match_char('=') {
                    self.add_token(TokenType::SubAssign)
                } else if self.match_char('-') {
                    self.add_token(TokenType::OpDecrement)
                } else {
                    self.add_token(TokenType::OpMinus)
                }
            }
            '*' => {
                if self.match_char('=') {
                    self.add_token(TokenType::MulAssign)
                } else if self.match_char('*') {
                    self.add_token(TokenType::OpExponent)
                } else {
                    self.add_token(TokenType::OpMult)
                }
            }
            '/' => {
                if self.match_char('=') {
                    self.add_token(TokenType::DivAssign)
                } else if self.match_char('/') {
                    self.add_token(TokenType::OpFloorDiv)
                } else {
                    self.add_token(TokenType::OpDivide)
                }
            }
            '%' => {
                if self.match_char('=') {
                    self.add_token(TokenType::ModAssign)
                } else {
                    self.add_token(TokenType::OpModulo)
                }
            }
            '^' => {
                if self.match_char('=') {
                    self.add_token(TokenType::XorAssign)
                } else {
                    self.add_token(TokenType::OpXor)
                }
            }
            '|' => {
                if self.match_char('=') {
                    self.add_token(TokenType::OrAssign)
                } else if self.match_char('|') {
                    self.add_token(TokenType::OpLor)
                } else {
                    self.add_token(TokenType::OpOr)
                }
            }
            '&' => {
                if self.match_char('=') {
                    self.add_token(TokenType::AndAssign)
                } else if self.match_char('&') {
                    self.add_token(TokenType::OpAnd)
                } else {
                    self.add_token(TokenType::OpLand)
                }
            }
            '~' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NegAssign)
                } else {
                    self.add_token(TokenType::OpNeg)
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual)
                } else {
                    self.add_token(TokenType::OpLneg)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterThanEqual)
                } else {
                    self.add_token(TokenType::GreaterThan)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessThanEqual)
                } else {
                    self.add_token(TokenType::LessThan)
                }
            }
            '#' => {
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string()?,
            c if c.is_digit(10) => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),
            _ => self.add_token(TokenType::Unknown),
        }
        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text: String = self.chars[self.start..self.current].iter().collect();
        self.tokens.push(Token::new(token_type, text, self.line));
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.chars[self.current] != expected {
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

    fn string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err("Unterminated string.".to_string());
        }

        self.advance(); // The closing ".

        let value = self.chars[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token(TokenType::String(value));
        Ok(())
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }
        let value_str: String = self.chars[self.start..self.current].iter().collect();
        let value: i64 = value_str.parse().unwrap();
        self.add_token(TokenType::Integer(value));
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text: String = self.chars[self.start..self.current].iter().collect();
        let token_type = match text.as_str() {
            "int" | "całkowita" => TokenType::KeywordInt,
            "str" | "tekst" => TokenType::KeywordStr,
            "func" | "funkcja" => TokenType::KeywordFunc,
            "return" | "zwróć" => TokenType::KeywordReturn,
            "struct" | "struktura" => TokenType::KeywordStruct,
            "match" | "dopasuj" => TokenType::KeywordMatch,
            "enum" | "wyliczenie" => TokenType::KeywordEnum,
            "if" | "jeśli" => TokenType::KeywordIf,
            "else" | "inaczej" => TokenType::KeywordElse,
            "for" | "dla" => TokenType::KeywordFor,
            "while" | "dopóki" => TokenType::KeywordWhile,
            "continue" | "kontynuuj" => TokenType::KeywordContinue,
            "break" | "przerwij" => TokenType::KeywordBreak,
            "true" | "prawda" => TokenType::KeywordTrue,
            "false" | "fałsz" => TokenType::KeywordFalse,

            _ => TokenType::Identifier,
        };
        self.add_token(token_type);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let source = "=+(){},;";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let token_types: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type).collect();
        assert_eq!(
            token_types,
            vec![
                TokenType::Assign,
                TokenType::OpPlus,
                TokenType::OpenParen,
                TokenType::CloseParen,
                TokenType::OpenBrace,
                TokenType::CloseBrace,
                TokenType::Comma,
                TokenType::Semicolon,
                TokenType::EndOfFile,
            ]
        );
    }

    #[test]
    fn test_integer() {
        let source = "123;";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].token_type, TokenType::Integer(123));
        assert_eq!(tokens[0].lexeme, "123");
    }

    #[test]
    fn test_string() {
        let source = "\"hello world\";";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens[0].token_type,
            TokenType::String("hello world".to_string())
        );
        assert_eq!(tokens[0].lexeme, "\"hello world\"");
    }

    #[test]
    fn test_identifier() {
        let source = "x = 5;";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let token_types: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type).collect();
        assert_eq!(
            token_types,
            vec![
                TokenType::Identifier,
                TokenType::Assign,
                TokenType::Integer(5),
                TokenType::Semicolon,
                TokenType::EndOfFile,
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let source = "func my_func(x: int) -> int { return x; }";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let token_types: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type).collect();
        assert_eq!(
            token_types,
            vec![
                TokenType::KeywordFunc,
                TokenType::Identifier,
                TokenType::OpenParen,
                TokenType::Identifier,
                TokenType::Colon,
                TokenType::KeywordInt,
                TokenType::CloseParen,
                TokenType::Arrow,
                TokenType::KeywordInt,
                TokenType::OpenBrace,
                TokenType::KeywordReturn,
                TokenType::Identifier,
                TokenType::Semicolon,
                TokenType::CloseBrace,
                TokenType::EndOfFile,
            ]
        );
    }
}
