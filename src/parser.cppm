module;

import lexer;
import ast;

#include <iostream>
#include <vector>
#include <string>
#include <memory>
#include <assert.h>

export module parser;

export class Parser {
public:
    Parser() {}
    Parser(std::vector<Token> &t) : tokens_(t) {
        type = t.size() > 0 ? t[0].type : TokenType::END_OF_FILE;
    }
    void setTokens(std::vector<Token> &tokens);
    std::unique_ptr<ASTNode> parse_program();
protected:
    Token curr();
    Token expect(TokenType type);
    TokenType next(int tokens = 1);
    TokenType peek();
    TokenType peekNext();
    /* One function per each EBNF rule */
    std::unique_ptr<ASTNode> statement();
    std::unique_ptr<ASTNode> assignment();
    std::unique_ptr<ASTNode> type_annotation();
    std::unique_ptr<ASTNode> expression();
    std::unique_ptr<ASTNode> term();
    std::unique_ptr<ASTNode> factor();
    std::unique_ptr<ASTNode> list();
    std::unique_ptr<ASTNode> lambda();
    std::unique_ptr<ASTNode> function_call();
    std::unique_ptr<ASTNode> function_def();
    std::vector<std::unique_ptr<ASTNode>> arglist();
    std::vector<std::unique_ptr<ASTNode>> typed_arglist();
    void parse_error(std::string err);
private:
    std::vector<Token> tokens_;
    TokenType type = TokenType::NUM_TOKENS;
    int current = 0;
};

Token Parser::curr() {
    return tokens_[current];
}

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

Token Parser::expect(TokenType t) {
    assert(TokenType::IDENTIFIER <= t && t < TokenType::NUM_TOKENS);
    if (peek() != t) {
        parse_error(error_expected(t, type)); // throws
    }
    Token tok = curr();
    next();
    return tok;
}

void Parser::parse_error(std::string error) {
    throw std::runtime_error(error + " at token '" \
              + tokens_[current].lexeme + "' in line " \
              + std::to_string(tokens_[current].line));
}

/**
 * @brief Parsers a TAP program given as a list of Tokens
 * 
 *  program         = {statement} .
 */
std::unique_ptr<ASTNode> Parser::parse_program() {
    std::vector<std::unique_ptr<ASTNode>> statements;
    while (type != TokenType::END_OF_FILE) {
        statements.push_back(statement());
    }
    return std::make_unique<Program>(statements);
}

/**
 * @brief Parse a statement
 *  statement     = [assignment ";"]
 *                  [ident ":" type-annotation ";"]
 *                  [function_call ";" ]
 *                  ["return" expression ";" ] .
 */
std::unique_ptr<ASTNode> Parser::statement() {

    std::unique_ptr<ASTNode> node;
    if (type == TokenType::IDENTIFIER) {
        Token t = curr();
        switch (peekNext()) {
            case TokenType::COLON: { /* Type annotation */
                next(2); 
                type_annotation();
                if (type != TokenType::SEMICOLON) { /* Inline type annotation */
                    // Assignment
                    expect(TokenType::ASSIGN);
                    node = std::make_unique<Assignment>(t.lexeme, expression());
                }
                break;
            }
            case TokenType::ASSIGN: { /* Variable assignment */
                node = assignment();
                break;
            }
            case TokenType::OPEN_PAREN: { /* Function call */
                // node = function_call();
                node = expression();
                break;
            }
            case TokenType::SEMICOLON: {
                next();
                node = std::make_unique<Identifier>(t.lexeme);
                break;
            }
            case TokenType::END_OF_FILE: {
                parse_error("Early EOF reached. Missing semicolon?");
            }
            default: { /* Try parsing an expression */
                node = expression();
                break;
            }
        }
    } else if (type == TokenType::KEYWORD_FUNC) {
        // No semicolon required after function definition 
        return function_def();
    } else if (type == TokenType::KEYWORD_RETURN) {
        // TODO: Return statement
        next();
        node = expression();
    } else { /* Try parsing an expression statement (evaluates to a value) */
        node = expression();
    }

    // Each valid statement ends with a semicolon
    expect(TokenType::SEMICOLON);

    return node;
}

// type-annotation = type-ident { "->" type-annotation } | "[" type-annotation "]" .
std::unique_ptr<ASTNode> Parser::type_annotation() {
    if (type == TokenType::OPEN_BRACKET) { /* Parse list type */
        next();
        type_annotation();
        expect(TokenType::CLOSE_BRACKET);
    } else if (type == TokenType::KEYWORD_INT || type == TokenType::KEYWORD_STRING) {
        next();
    } else {
        parse_error("Error while parsing type annotation: invalid type");
    }

    if (type == TokenType::ARROW) {
        next();
        type_annotation();
    }

    // TODO:
    return std::unique_ptr<ASTNode>(nullptr);
}

// assignment = ident [ ":" type-annotation ] "=" expression { "," ident "=" expression } .
std::unique_ptr<ASTNode> Parser::assignment() {
    Token tok = expect(TokenType::IDENTIFIER);
    if (type == TokenType::COLON) { /* Optional type annotation */
        next();
        type_annotation();
    }
    expect(TokenType::ASSIGN);
    std::unique_ptr<ASTNode> exp = expression();

    // TODO: Handle multiple assignments on a line
    while (type == TokenType::COMMA) {
        next();
        expect(TokenType::IDENTIFIER);
        expect(TokenType::ASSIGN);
        expression();
    }

    return std::make_unique<Assignment>(tok.lexeme, std::move(exp));
}

// function_call   = ident "(" arglist ")" .
std::unique_ptr<ASTNode> Parser::function_call() {
    Token tok_name = expect(TokenType::IDENTIFIER);
    expect(TokenType::OPEN_PAREN);
    std::vector<std::unique_ptr<ASTNode>> args = {};
    if (peek() != TokenType::CLOSE_PAREN) { /* Non-empty arglist */
        args = arglist();
    }
    expect(TokenType::CLOSE_PAREN);
    return std::make_unique<FunctionCall>(tok_name.lexeme, std::move(args));
}


// function_def    = "func" ident "(" typed-arglist ")" "{" {statement} "}" .
std::unique_ptr<ASTNode> Parser::function_def() {
    expect(TokenType::KEYWORD_FUNC);
    Token tok_name = expect(TokenType::IDENTIFIER);
    expect(TokenType::OPEN_PAREN);
    typed_arglist();
    expect(TokenType::CLOSE_PAREN);
    if (type == TokenType::COLON) { /* Optional type annotation */
        next();
        type_annotation();
    }
    expect(TokenType::OPEN_BRACE);
    std::unique_ptr<FunctionDef> func = std::make_unique<FunctionDef>(tok_name.lexeme);
    while (type != TokenType::CLOSE_BRACE) {
        func->addStatement(statement());
    }
    expect(TokenType::CLOSE_BRACE);

    return func;
}

std::vector<std::unique_ptr<ASTNode>> Parser::arglist() {
    expression();
    while (type == TokenType::COMMA) {
        next();
        expression();
    }

    return {};
}

// typed_arglist         = [ expression { "," expression } ] .
std::vector<std::unique_ptr<ASTNode>> Parser::typed_arglist() {
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

    return {};
}

std::unique_ptr<ASTNode> Parser::expression() {
    std::unique_ptr<Expression> e = std::make_unique<Expression>(term());
    while (type == TokenType::OP_PLUS || type == TokenType::OP_MINUS) {
        char op = curr().lexeme[0]; 
        next();
        e->addTerm(term(), op);
    }
    return e;
}

std::unique_ptr<ASTNode> Parser::term() {
    std::unique_ptr<Term> t = std::make_unique<Term>(factor());
    while (type == TokenType::OP_MULT || type == TokenType::OP_DIVIDE) {
        char op = curr().lexeme[0];
        next();
        t->addFactor(factor(), op);
    }
    return t;
}

// factor = ident | integer | string | list | lambda | function_call | "(" expression ")" .
std::unique_ptr<ASTNode> Parser::factor() {
    Token t = curr();
    switch (type) {
        case TokenType::IDENTIFIER: /* Identifier or function call */
            if (peekNext() == TokenType::OPEN_PAREN) { /* Function call */
                return function_call();
            }
            /* Identifier */
            next();
            return std::make_unique<Identifier>(t.lexeme);
        case TokenType::INTEGER:
            next();
            return std::make_unique<Integer>(std::stoi(t.lexeme));
        case TokenType::STRING:
            next();
            return std::make_unique<String>(t.lexeme);
        case TokenType::OPEN_BRACKET:
            return list(); 
        case TokenType::LAMBDA:
            return lambda(); 
        case TokenType::OPEN_PAREN: {
            next();
            std::unique_ptr<ASTNode> t = expression();
            expect(TokenType::CLOSE_PAREN);
            return t;
        }
        default:
            parse_error("Bad factor");
    }
    std::cerr << "Returning nullptr\n";
    return std::unique_ptr<ASTNode>(nullptr);
}

// list = "[" [ expression { "," expression } ] "]" .
std::unique_ptr<ASTNode> Parser::list() {
    expect(TokenType::OPEN_BRACKET);
    std::vector<std::unique_ptr<ASTNode>> elements;

    if (type != TokenType::CLOSE_BRACKET) { /* Empty lists */
        elements.push_back(expression());
        while (type == TokenType::COMMA) {
            next();
            elements.push_back(expression());
        }
    }
    expect(TokenType::CLOSE_BRACKET);
    return std::make_unique<List>(elements);
}

// lambda = "\" ident "." expression .
std::unique_ptr<ASTNode> Parser::lambda() {
    expect(TokenType::LAMBDA);
    expect(TokenType::IDENTIFIER);
    expect(TokenType::PERIOD);
    expression();

    // TODO:
    return std::unique_ptr<ASTNode>(nullptr);
}
