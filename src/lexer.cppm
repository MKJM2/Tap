module; 

#include <iostream>
#include <vector>
#include <string>
#include <array>

export module lexer;

export enum class TokenType : std::size_t {
    IDENTIFIER,
    INTEGER,
    SEMICOLON,
    ASSIGN,
    COLON,
    ARROW,
    OPEN_PAREN,
    CLOSE_PAREN,
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    COMMA,
    LAMBDA,
    KEYWORD_FUNC,
    KEYWORD_RETURN,
    KEYWORD_STRUCT,
    KEYWORD_MATCH,
    KEYWORD_ENUM,
    KEYWORD_LOOP,
    KEYWORD_TODO,
    UNKNOWN,
    END_OF_FILE
};

export std::array<std::string,23> TokenType2String = {
    "IDENTIFIER",
    "INTEGER",
    "SEMICOLON",
    "ASSIGN",
    "COLON",
    "ARROW",
    "OPEN_PAREN",
    "CLOSE_PAREN",
    "OPEN_BRACE",
    "CLOSE_BRACE",
    "OPEN_BRACKET",
    "CLOSE_BRACKET",
    "COMMA",
    "LAMBDA",
    "KEYWORD_FUNC",
    "KEYWORD_RETURN",
    "KEYWORD_STRUCT",
    "KEYWORD_MATCH",
    "KEYWORD_ENUM",
    "KEYWORD_LOOP",
    "KEYWORD_TODO",
    "UNKNOWN",
    "END_OF_FILE"
};

export struct Token {
    TokenType type;
    std::string lexeme;
    int line;
};

export class Lexer {
public:
    Lexer(const std::string& source);
    std::vector<Token> tokenize();

private:
    void scanToken();
    char advance();
    char peek();
    char peekNext(int chars = 1);
    bool match(char expected);
    bool isDigit(char c);
    bool isAlpha(char c);
    bool isAlphaNumeric(char c);
    void addToken(TokenType type);
    void addToken(TokenType type, const std::string& literal);

private:
    std::string source;
    std::vector<Token> tokens;
    int start = 0;
    int current = 0;
    int line = 1;
};

Lexer::Lexer(const std::string& source) : source(source) {}

std::vector<Token> Lexer::tokenize() {
    while (current < source.length()) {
        start = current;
        scanToken();
    }

    tokens.push_back({TokenType::END_OF_FILE, "", line});
    return tokens;
}

void Lexer::scanToken() {
    char c = advance();
    switch (c) {
        // Handle single-character tokens
        case ';': addToken(TokenType::SEMICOLON); break;
        case '=': addToken(TokenType::ASSIGN); break;
        case ':': addToken(TokenType::COLON); break;
        case '(': addToken(TokenType::OPEN_PAREN); break;
        case ')': addToken(TokenType::CLOSE_PAREN); break;
        case '{': addToken(TokenType::OPEN_BRACE); break;
        case '}': addToken(TokenType::CLOSE_BRACE); break;
        case '[': addToken(TokenType::OPEN_BRACKET); break;
        case ']': addToken(TokenType::CLOSE_BRACKET); break;
        case ',': addToken(TokenType::COMMA); break;

        // Handle two-character tokens and keywords
        case '-':
            addToken(match('>') ? TokenType::ARROW : TokenType::UNKNOWN);
            break;
        case '\\': addToken(TokenType::LAMBDA); break;
        case 'f':
            if (peek() == 'u' && peekNext() == 'n' && !isAlphaNumeric(peekNext(2))) {
                addToken(TokenType::KEYWORD_FUNC);
            }
            break;
        case 'r':
            if (peek() == 'e' && peekNext() == 't' && peekNext(2) == 'u' && peekNext(3) == 'r' && peekNext(4) == 'n' && !isAlphaNumeric(peekNext(5))) {
                addToken(TokenType::KEYWORD_RETURN);
            }
            break;

        // Handle literals (identifiers and integers)
        default:
            if (isDigit(c)) {
                while (isDigit(peek())) advance();
                addToken(TokenType::INTEGER, source.substr(start, current - start));
            } else if (isAlpha(c)) {
                while (isAlphaNumeric(peek())) advance();
                std::string lexeme = source.substr(start, current - start);
                if (lexeme == "int" || lexeme == "str" || lexeme == "struct" || lexeme == "match" || lexeme == "enum" || lexeme == "loop" || lexeme == "todo") {
                    addToken(TokenType::KEYWORD_TODO);
                } else {
                    addToken(TokenType::IDENTIFIER, lexeme);
                }
            } else if (c == ' ' || c == '\t' || c == '\r') {
                // Ignore whitespace
            } else if (c == '\n') {
                line++;
            } else {
                // Handle unknown characters
                addToken(TokenType::UNKNOWN);
            }
            break;
    }
}

char Lexer::advance() {
    return source[current++];
}

char Lexer::peek() {
    return current < source.length() ? source[current] : '\0';
}

char Lexer::peekNext(int chars) {
    return current + chars < source.length() ? source[current + chars] : '\0';
}

bool Lexer::match(char expected) {
    if (peek() == expected) {
        advance();
        return true;
    }
    return false;
}

bool Lexer::isDigit(char c) {
    return c >= '0' && c <= '9';
}

bool Lexer::isAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

bool Lexer::isAlphaNumeric(char c) {
    return isAlpha(c) || isDigit(c);
}

void Lexer::addToken(TokenType type) {
    addToken(type, source.substr(start, current - start));
}

void Lexer::addToken(TokenType type, const std::string& literal) {
    tokens.push_back({type, literal, line});
}