use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier,
    Integer(i64),
    Float(f64), // Added
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
    OpMod, // Renamed from OpModulo to match Parser
    OpXor,
    OpOr,  // Bitwise Or
    OpAnd, // Bitwise And
    OpNeg,
    OpLor,  // Logical ||
    OpLand, // Logical &&
    Bang,   // Renamed from OpLneg (!) to match Parser
    OpIncrement,
    OpDecrement,
    OpExponent,
    Arrow,    // ->
    ArrowFat, // => (Added)
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
    Lambda,     // \
    Underscore, // _ (Added)
    KeywordInt,
    KeywordStr,
    KeywordFunc,
    KeywordReturn,
    KeywordStruct,
    KeywordMatch,
    KeywordEnum,
    KeywordFor,
    KeywordIn,
    KeywordWhile,
    KeywordContinue,
    KeywordBreak,
    KeywordIf,
    KeywordElse,
    KeywordTrue,
    KeywordFalse,
    KeywordUnit, // Added
    KeywordType,
    Unknown,
    EndOfFile,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub fn len(self) -> usize {
        self.hi - self.lo
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, span: Span) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            span,
        }
    }
}

pub struct Lexer {
    chars: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            chars: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, String> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token::new(
            TokenType::EndOfFile,
            "".to_string(),
            self.line,
            Span::new(self.current, self.current),
        ));
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
            '_' => self.add_token(TokenType::Underscore),
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::Equal)
                } else if self.match_char('>') {
                    self.add_token(TokenType::ArrowFat) // Added =>
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
                    self.add_token(TokenType::OpMod)
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
                    self.add_token(TokenType::OpLand)
                } else {
                    self.add_token(TokenType::OpAnd)
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual)
                } else {
                    self.add_token(TokenType::Bang) // Changed from OpLneg
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
        let span = Span::new(self.start, self.current);
        self.tokens
            .push(Token::new(token_type, text, self.line, span));
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
        self.advance();
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

        // Look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance(); // Consume the "."
            while self.peek().is_digit(10) {
                self.advance();
            }
            let value_str: String = self.chars[self.start..self.current].iter().collect();
            let value: f64 = value_str.parse().unwrap();
            self.add_token(TokenType::Float(value));
        } else {
            let value_str: String = self.chars[self.start..self.current].iter().collect();
            let value: i64 = value_str.parse().unwrap();
            self.add_token(TokenType::Integer(value));
        }
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let text: String = self.chars[self.start..self.current].iter().collect();
        let token_type = match text.as_str() {
            "int" | "całkowita" => TokenType::KeywordInt,
            "str" | "tekst" => TokenType::KeywordStr,
            "unit" | "pusty" => TokenType::KeywordUnit, // Added
            "func" | "funkcja" => TokenType::KeywordFunc,
            "return" | "zwróć" => TokenType::KeywordReturn,
            "struct" | "struktura" => TokenType::KeywordStruct,
            "match" | "dopasuj" => TokenType::KeywordMatch,
            "enum" | "wyliczenie" => TokenType::KeywordEnum,
            "if" | "jeśli" => TokenType::KeywordIf,
            "else" | "inaczej" => TokenType::KeywordElse,
            "for" | "dla" => TokenType::KeywordFor,
            "in" | "w" => TokenType::KeywordIn,
            "while" | "dopóki" => TokenType::KeywordWhile,
            "continue" | "kontynuuj" => TokenType::KeywordContinue,
            "break" | "przerwij" => TokenType::KeywordBreak,
            "true" | "prawda" => TokenType::KeywordTrue,
            "false" | "fałsz" => TokenType::KeywordFalse,
            "type" | "typ" => TokenType::KeywordType,
            _ => TokenType::Identifier,
        };
        self.add_token(token_type);
    }
}
