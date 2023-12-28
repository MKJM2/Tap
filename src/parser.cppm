module;

import lexer;

#include <iostream>
#include <vector>
#include <string>
#include <assert.h>

export module parser;

export class ASTNode {
public:
    // Enum to represent different types of AST nodes
    enum NodeType {
        INT,
        FLOAT,
        STRING,
        BOOL,
        VARIABLE,
        UNOP,
        BINOP,
        CONDITIONAL,
    };

    NodeType type() const {
        return static_cast<NodeType>(type_);
    }

    unsigned int arity() const {
        return arity_;
    }

    virtual ~ASTNode() {};
protected: 
    ASTNode() = delete;
    ASTNode(char8_t op) : type_(op), arity_(0) {}

private:
    const char8_t arity_;
    const char8_t type_;
};

class ASTVariable : public ASTNode {
public:
  static bool typeof(const ASTNode *e) {
    return e->type() == VARIABLE;
  }

  ASTVariable() = delete;
  ASTVariable(const char* s) : ASTNode(VARIABLE), name_(s) {}
  ASTVariable(std::string s) : ASTNode(VARIABLE), name_(std::move(s)) {}

  // Name of the variable
  const std::string& name() const { return name_; }

private:
  std::string name_;
};


export class Parser {
public:
    Parser() {}
    Parser(std::vector<Token> &t) : tokens_(t) {
        type = t.size() > 0 ? t[0].type : TokenType::END_OF_FILE;
    }
    void setTokens(std::vector<Token> &tokens);
    void parse_program();
protected:
    TokenType next(int tokens = 1);
    TokenType peek();
    TokenType peekNext();
    void expect(TokenType type);
    /* One function per each EBNF rule */
    void statement();
    void assignment();
    void type_annotation();
    void expression();
    void term();
    void factor();
    void list();
    void lambda();
    void function_call();
    void function_def();
    void arglist();
    void typed_arglist();
    void parse_error(std::string err);
private:
    std::vector<Token> tokens_;
    TokenType type = TokenType::NUM_TOKENS;
    int current = 0;
};

TokenType Parser::peek() {
    assert(current < tokens_.size());
    return type = tokens_[current].type;
}

TokenType Parser::next(int tokens) {
    current += tokens;
    assert(current < tokens_.size());
    return type = tokens_[current].type;
}

TokenType Parser::peekNext() {
    assert(current < tokens_.size());
    return tokens_[current + 1].type;
}

void Parser::setTokens(std::vector<Token>& tokens) {
    tokens_ = tokens;
    type = tokens.size() > 0 ? tokens[0].type : TokenType::END_OF_FILE;
    current = 0;
}

static 
std::string error_expected(TokenType exp, TokenType got) {
    return "Expected token " \
        + TokenType2String[static_cast<std::size_t>(exp)] \
        + " but got " 
        + TokenType2String[static_cast<std::size_t>(got)];
}

void Parser::expect(TokenType t) {
    assert(TokenType::IDENTIFIER <= t && t < TokenType::NUM_TOKENS);
    if (peek() != t) {
        parse_error(error_expected(t, type)); // throws
    }
    next();
}

void Parser::parse_error(std::string error) {
    throw std::runtime_error(error + " <- at token '" \
              + tokens_[current].lexeme + "' in line " \
              + std::to_string(tokens_[current].line) + '\n');
}

/**
 * @brief Parsers a TAP program given as a list of Tokens
 * 
 *  program         = {statement} .
 */
void Parser::parse_program() {
    while (type != TokenType::END_OF_FILE) {
        statement();
    }

}

/**
 * @brief Parse a statement
 *  statement     = [assignment ";"]
 *                  [ident ":" type-annotation ";"]
 *                  [function_call ";" ]
 *                  ["return" expression ";" ] .
 */
void Parser::statement() {
    if (type == TokenType::IDENTIFIER) {
        switch (peekNext()) {
            case TokenType::COLON: { /* Type annotation */
                next(2); 
                type_annotation();
                break;
            }
            case TokenType::ASSIGN: { /* Variable assignment */
                next();
                assignment();
                break;
            }
            case TokenType::OPEN_PAREN: { /* Function call */
                next();
                function_call();
                break;
            }
            case TokenType::SEMICOLON: {
                next();
                break;
            }
            case TokenType::END_OF_FILE: {
                parse_error("Early EOF reached. Missing semicolon?");
            }
            default: { /* Try parsing an expression */
                expression();
                break;
            }
        }
    } else if (type == TokenType::KEYWORD_FUNC) {
        function_def();
        // No semicolon required after function definition 
        return;
    } else if (type == TokenType::KEYWORD_RETURN) {
        next();
        expression();
    } else { /* Try parsing an expression statement (evaluates to a value) */
        expression();
    }

    // Each valid statement ends with a semicolon
    expect(TokenType::SEMICOLON);
}

// type-annotation = type-ident { "->" type-annotation } | "[" type-annotation "]" .
void Parser::type_annotation() {
    if (type == TokenType::OPEN_BRACE) { /* Parse list type */
        next();
        type_annotation();
        expect(TokenType::CLOSE_BRACE);
    } else if (type == TokenType::KEYWORD_INT || type == TokenType::KEYWORD_STRING) {
        next();
        if (type == TokenType::ARROW)
            type_annotation();

    } else 
        parse_error("Error while parsing type annotation: invalid type");
}

// assignment = ident [ ":" type-annotation ] "=" expression { "," ident "=" expression } .
void Parser::assignment() {
    expect(TokenType::ASSIGN);
    expression();
    while (type == TokenType::COMMA) {
        next();
        expect(TokenType::IDENTIFIER);
        expect(TokenType::ASSIGN);
        expression();
    }
}

// function_call   = ident "(" arglist ")" .
void Parser::function_call() {
    expect(TokenType::OPEN_PAREN);
    arglist();
    expect(TokenType::CLOSE_PAREN);
}


// function_def    = "func" ident "(" typed-arglist ")" "{" statement "}" .
void Parser::function_def() {
    expect(TokenType::KEYWORD_FUNC);
    expect(TokenType::IDENTIFIER);
    expect(TokenType::OPEN_PAREN);
    typed_arglist();
    expect(TokenType::CLOSE_PAREN);
    expect(TokenType::OPEN_BRACE);
    statement();
    expect(TokenType::CLOSE_BRACE);
}

void Parser::arglist() {
    expression();
    while (type == TokenType::COMMA) {
        next();
        expression();
    }
}

// typed_arglist         = [ expression { "," expression } ] .
void Parser::typed_arglist() {
    expression();
    if (type == TokenType::COLON) {
        next();
        type_annotation();
    }
    while (type == TokenType::COMMA) {
        next();
        expression();
        if (type == TokenType::COLON) {
            next();
            type_annotation();
        }
    }
}

void Parser::expression() {
    term();
    while (type == TokenType::OP_PLUS || type == TokenType::OP_MINUS) {
        next();
        term();
    }
}

void Parser::term() {
    factor();
    while (type == TokenType::OP_MULT || type == TokenType::OP_DIVIDE) {
        next();
        factor();
    }
}

// factor = ident | integer | string | list | lambda | function_call | "(" expression ")" .
void Parser::factor() {
    switch (type) {
        case TokenType::IDENTIFIER: /* Identifier or function call */
            next();
            if (type == TokenType::OPEN_PAREN) {/* Function call */
                next();
                expression();
                expect(TokenType::CLOSE_PAREN);
            }
            break;
        case TokenType::INTEGER:
            next(); break;
        case TokenType::STRING:
            next(); break;
        case TokenType::OPEN_BRACE:
            // REVIEW: Empty list support
            list(); break;
        case TokenType::LAMBDA:
            lambda(); break;
        case TokenType::OPEN_PAREN:
            next();
            expression();
            expect(TokenType::CLOSE_PAREN);
            break;
        default:
            parse_error("Bad factor");
    }
}

// list = "[" [ expression { "," expression } ] "]" .
void Parser::list() {
    expect(TokenType::OPEN_BRACKET);
    if (type != TokenType::CLOSE_BRACKET) { /* Empty lists */
        expression();
        while (type == TokenType::COMMA) {
            next();
            expression();
        }
    }
    expect(TokenType::CLOSE_BRACKET);
}

// lambda = "\" ident "." expression .
void Parser::lambda() {
    expect(TokenType::LAMBDA);
    expect(TokenType::IDENTIFIER);
    expect(TokenType::PERIOD);
    expression();
}