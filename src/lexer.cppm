module; 

#include <iostream>
#include <vector>
#include <string>
#include <array>
#include <optional>
#include <unordered_map>

export module lexer;

export enum class TokenType : std::size_t {
    IDENTIFIER,
    INTEGER,
    STRING,
    SEMICOLON,
    ASSIGN,
    ADD_ASSIGN,
    SUB_ASSIGN,
    MUL_ASSIGN,
    DIV_ASSIGN,
    MOD_ASSIGN,
    MULT_ASSIGN,
    XOR_ASSIGN,
    OR_ASSIGN,
    AND_ASSIGN,
    NEG_ASSIGN,
    COLON,
    OP_PLUS,
    OP_MINUS,
    OP_MULT,
    OP_DIVIDE,
    OP_FLOORDIV,
    OP_MODULO,
    OP_XOR,
    OP_OR,
    OP_AND,
    OP_NEG,
    OP_LOR,
    OP_LAND,
    OP_LNEG,
    OP_INCREMENT,
    OP_DECREMENT,
    OP_EXPONENT,
    ARROW,
    EQUAL,
    NOT_EQUAL,
    OPEN_PAREN,
    CLOSE_PAREN,
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    COMMA,
    PERIOD,
    LAMBDA,
    KEYWORD_INT,   // TODO: 32bit vs 16bit vs 8 bit
    KEYWORD_STRING,
    KEYWORD_FUNC,
    KEYWORD_RETURN,
    KEYWORD_STRUCT,
    KEYWORD_MATCH,
    KEYWORD_ENUM,
    KEYWORD_FOR,
    KEYWORD_WHILE,
    KEYWORD_TODO,
    KEYWORD_TRUE,
    KEYWORD_FALSE,
    UNKNOWN,
    END_OF_FILE,
    NUM_TOKENS
};

// TODO: Use a boost macro to define an enum with corresponding strings
export std::array<std::string,static_cast<std::size_t>(TokenType::NUM_TOKENS)> 
TokenType2String = {
    "IDENTIFIER",
    "INTEGER",
    "STRING",
    "SEMICOLON",
    "ASSIGN",
    "ADD_ASSIGN",
    "SUB_ASSIGN",
    "MUL_ASSIGN",
    "DIV_ASSIGN",
    "MOD_ASSIGN",
    "MULT_ASSIGN",
    "XOR_ASSIGN",
    "OR_ASSIGN",
    "AND_ASSIGN",
    "NEG_ASSIGN",
    "COLON",
    "OP_PLUS",
    "OP_MINUS",
    "OP_MULT",
    "OP_DIVIDE",
    "OP_FLOORDIV",
    "OP_MODULO",
    "OP_XOR",
    "OP_OR",
    "OP_AND",
    "OP_NEG",
    "OP_LOR",
    "OP_LAND",
    "OP_LNEG",
    "OP_INCREMENT",
    "OP_DECREMENT",
    "OP_EXPONENT",
    "ARROW",
    "EQUAL",
    "NOT_EQUAL",
    "OPEN_PAREN",
    "CLOSE_PAREN",
    "OPEN_BRACE",
    "CLOSE_BRACE",
    "OPEN_BRACKET",
    "CLOSE_BRACKET",
    "COMMA",
    "PERIOD",
    "LAMBDA",
    "KEYWORD_INT",
    "KEYWORD_STRING",
    "KEYWORD_FUNC",
    "KEYWORD_RETURN",
    "KEYWORD_STRUCT",
    "KEYWORD_MATCH",
    "KEYWORD_ENUM",
    "KEYWORD_FOR",
    "KEYWORD_WHILE",
    "KEYWORD_TODO",
    "KEYWORD_TRUE",
    "KEYWORD_FALSE",
    "UNKNOWN",
    "END_OF_FILE",
};

std::unordered_map<std::string, TokenType> keyword2Token = {
    {"int",    TokenType::KEYWORD_INT},
    {"str",    TokenType::KEYWORD_STRING},
    {"func",   TokenType::KEYWORD_FUNC},
    {"return", TokenType::KEYWORD_RETURN},
    {"struct", TokenType::KEYWORD_STRUCT},
    {"match",  TokenType::KEYWORD_MATCH},
    {"enum",   TokenType::KEYWORD_ENUM},
    {"for",    TokenType::KEYWORD_FOR},
    {"while",  TokenType::KEYWORD_WHILE},
    {"true",   TokenType::KEYWORD_TRUE},
    {"false",  TokenType::KEYWORD_FALSE},
};

export struct Token {
    TokenType type;
    std::string lexeme;
    int line;
};

export class Lexer {
public:
    Lexer() {};
    Lexer(const std::string& source);
    void setSource(const std::string& in);
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

void Lexer::setSource(const std::string& in) {
    source = in;
    start = 0;
    current = 0;
    line = 1;
    tokens.clear();
}

void Lexer::scanToken() {
    char c = advance();
    switch (c) {
        // Handle single-character tokens
        case ';': addToken(TokenType::SEMICOLON); break;
        case ':': addToken(TokenType::COLON); break;
        case '(': addToken(TokenType::OPEN_PAREN); break;
        case ')': addToken(TokenType::CLOSE_PAREN); break;
        case '{': addToken(TokenType::OPEN_BRACE); break;
        case '}': addToken(TokenType::CLOSE_BRACE); break;
        case '[': addToken(TokenType::OPEN_BRACKET); break;
        case ']': addToken(TokenType::CLOSE_BRACKET); break;
        case ',': addToken(TokenType::COMMA); break;
        case '.': addToken(TokenType::PERIOD); break;

        // Handle two-character tokens and keywords
        case '=': {
            if (match('=')) {
                addToken(TokenType::EQUAL); break;
            }
            addToken(TokenType::ASSIGN); 
            break;
        }
        case '+': {
            if (match('=')) {
                addToken(TokenType::ADD_ASSIGN); break;
            } else if (match('-')) {
                addToken(TokenType::OP_INCREMENT); break;
            }
            addToken(TokenType::OP_PLUS); 
            break;
        }
        case '-': {
            if (match('>')) {
                addToken(TokenType::ARROW); break;
            } else if (match('=')) {
                addToken(TokenType::SUB_ASSIGN); break;
            } else if (match('-')) {
                addToken(TokenType::OP_DECREMENT); break;
            }
            addToken(TokenType::OP_MINUS);
            break;
        }
        case '*': {
            if (match('=')) {
                addToken(TokenType::MUL_ASSIGN); break;
            } else if (match('*')) {
                addToken(TokenType::OP_EXPONENT); break;
            } 
            addToken(TokenType::OP_MULT);
            break;
        }
        case '/': {
            if (match('=')) {
                addToken(TokenType::DIV_ASSIGN); break;
            } else if (match('/')) {
                addToken(TokenType::OP_FLOORDIV); break;
            }
            addToken(TokenType::OP_DIVIDE); 
            break;
        } 
        case '%': {
            if (match('=')) {
                addToken(TokenType::MOD_ASSIGN); break;
            }
            addToken(TokenType::OP_MODULO);
            break;
        }
        case '^': {
            if (match('=')) {
                addToken(TokenType::XOR_ASSIGN); break;
            } 
            addToken(TokenType::OP_XOR); 
            break;
        }
        case '|': {
            if (match('=')) {
                addToken(TokenType::OR_ASSIGN); break;
            } else if (match('|')) {
                addToken(TokenType::OP_LOR); break;
            }
            addToken(TokenType::OP_OR); 
            break;
        }
        case '&': {
            if (match('=')) {
                addToken(TokenType::AND_ASSIGN); break;
            } else if (match('&')) {
                addToken(TokenType::OP_AND); break;
            }
            addToken(TokenType::OP_LAND); 
            break;
        }
        case '~': {
            if (match('=')) {
                addToken(TokenType::NEG_ASSIGN); break;
            }
            addToken(TokenType::OP_NEG); 
            break;
        }
        case '!': {
            if (match('=')) {
                addToken(TokenType::NOT_EQUAL); break;
            }
            addToken(TokenType::OP_LNEG); 
            break;
        }
        case '\\': addToken(TokenType::LAMBDA); break;
        default:
            if (isDigit(c)) {
                while (isDigit(peek())) advance();
                addToken(TokenType::INTEGER, source.substr(start, current - start));
            } else if (isAlpha(c)) {
                while (isAlphaNumeric(peek())) advance();
                std::string lexeme = source.substr(start, current - start);

                auto it = keyword2Token.find(lexeme);
                if (it != keyword2Token.end()) {
                    addToken(it->second, lexeme);
                } else {
                    addToken(TokenType::IDENTIFIER, lexeme);
                }
            } else if (c == '"') {
                // TODO: Parse string literal
                while (peek() != '"') {
                    if (peek() == '\n') {
                        line++;
                    }
                    advance();
                }

                if (peek() == '\0') {
                    // Handle unterminated string literal error
                    // You might want to add an error handling mechanism here
                    std::cerr << "Error: Unterminated string literal before EOF\n";
                    std::exit(1);
                } else {
                    // Consume the closing double quote
                    advance();
                    std::string value = source.substr(start + 1, current - start - 2);
                    addToken(TokenType::STRING, value);
                }
            } else if (c == ' ' || c == '\t' || c == '\r') {
                // We ignore whitespace
            } else if (c == '\n') {
                line++;
            } else {
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