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
    Program* parse_program();
protected:
    Token curr();
    Token expect(TokenType type);
    TokenType next(int tokens = 1);
    TokenType peek();
    TokenType peekNext();
    /* One function per each EBNF rule */
    ASTNode* statement();
    Assignment* assignment();
    ASTNode* type_annotation();
    Expression* expression();
    Term* term();
    ASTNode* factor();
    List* list();
    ASTNode* lambda();
    FunctionCall* function_call();
    FunctionDef* function_def();
    std::vector<ASTNode*> arglist();
    std::vector<ASTNode*> typed_arglist();
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
Program* Parser::parse_program() {
    std::vector<ASTNode*> statements;
    while (type != TokenType::END_OF_FILE) {
        statements.push_back(statement());
    }
    return new Program(statements);
}

/**
 * @brief Parse a statement
 *  statement     = [assignment ";"]
 *                  [ident ":" type-annotation ";"]
 *                  [function_call ";" ]
 *                  ["return" expression ";" ] .
 */
ASTNode* Parser::statement() {

    ASTNode* node;
    if (type == TokenType::IDENTIFIER) {
        Token t = curr();
        switch (peekNext()) {
            case TokenType::COLON: { /* Type annotation */
                next(2); 
                type_annotation();
                if (type != TokenType::SEMICOLON) { /* Inline type annotation */
                    // Assignment
                    expect(TokenType::ASSIGN);
                    node = new Assignment(t.lexeme, expression());
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
                node = new Identifier(t.lexeme);
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
ASTNode* Parser::type_annotation() {
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
    return nullptr;
}

// assignment = ident [ ":" type-annotation ] "=" expression { "," ident "=" expression } .
Assignment* Parser::assignment() {
    Token tok = expect(TokenType::IDENTIFIER);
    if (type == TokenType::COLON) { /* Optional type annotation */
        next();
        type_annotation();
    }
    expect(TokenType::ASSIGN);
    ASTNode* exp = expression();

    // TODO: Handle multiple assignments on a line
    while (type == TokenType::COMMA) {
        next();
        expect(TokenType::IDENTIFIER);
        expect(TokenType::ASSIGN);
        expression();
    }

    return new Assignment(tok.lexeme, std::move(exp));
}

// function_call   = ident "(" arglist ")" .
FunctionCall* Parser::function_call() {
    Token tok_name = expect(TokenType::IDENTIFIER);
    expect(TokenType::OPEN_PAREN);
    std::vector<ASTNode*> args = {};
    if (peek() != TokenType::CLOSE_PAREN) { /* Non-empty arglist */
        args = arglist();
    }
    expect(TokenType::CLOSE_PAREN);
    return new FunctionCall(tok_name.lexeme, args);
}


// function_def    = "func" ident "(" typed-arglist ")" "{" {statement} "}" .
FunctionDef* Parser::function_def() {
    expect(TokenType::KEYWORD_FUNC);
    Token tok_name = expect(TokenType::IDENTIFIER);
    expect(TokenType::OPEN_PAREN);
    FunctionDef *func = new FunctionDef(tok_name.lexeme);
    std::vector<ASTNode*> args = {};
    if (peek() != TokenType::CLOSE_PAREN) {
        args = typed_arglist();
    }
    for (auto& arg : args) {
        func->addArg(static_cast<Identifier&>(*arg).name());
    }
    expect(TokenType::CLOSE_PAREN);
    if (type == TokenType::COLON) { /* Optional type annotation */
        next();
        type_annotation();
    }
    expect(TokenType::OPEN_BRACE);
    while (type != TokenType::CLOSE_BRACE) {
        func->addStatement(statement());
    }
    expect(TokenType::CLOSE_BRACE);

    return func;
}

std::vector<ASTNode*> Parser::arglist() {
    std::vector<ASTNode*> args; /* A list of expressions */
    args.push_back(expression());
    while (type == TokenType::COMMA) {
        next();
        args.push_back(expression());
    }

    return args;
}

// typed_arglist         = [ expression { "," expression } ] .
std::vector<ASTNode*> Parser::typed_arglist() {
    std::vector<ASTNode*> args; /* A list of string identifiers */
    args.push_back(factor());
    if (type == TokenType::COLON) {
        next();
        type_annotation();
    }
    while (type == TokenType::COMMA) {
        next();
        args.push_back(factor());
        if (type == TokenType::COLON) {
            next();
            type_annotation();
        }
    }

    // Ensure all args are identifiers
    for (auto& arg : args) {
        if (arg->type() != ASTNode::IDENTIFIER)
            parse_error("Invalid argument type: expected string identifier");
    }

    return args;
}

Expression* Parser::expression() {
    Expression *e = new Expression(term());
    while (type == TokenType::OP_PLUS || type == TokenType::OP_MINUS) {
        char op = curr().lexeme[0]; 
        next();
        e->addTerm(term(), op);
    }
    return e;
}

Term* Parser::term() {
    Term *t = new Term(factor());
    while (type == TokenType::OP_MULT || type == TokenType::OP_DIVIDE) {
        char op = curr().lexeme[0];
        next();
        t->addFactor(factor(), op);
    }

    // TODO: Hacky, if the term has only one factor, return that factor
    // if (t->size() == 1) {
    //     return std::move(t->factors().front());
    // }

    return t;
}

// factor = ident | integer | string | list | lambda | function_call | "(" expression ")" .
ASTNode* Parser::factor() {
    Token t = curr();
    switch (type) {
        case TokenType::IDENTIFIER: /* Identifier or function call */
            if (peekNext() == TokenType::OPEN_PAREN) { /* Function call */
                return function_call();
            }
            /* Identifier */
            next();
            return new Identifier(t.lexeme);
        case TokenType::INTEGER:
            next();
            return new Integer(std::stoi(t.lexeme));
        case TokenType::STRING:
            next();
            return new String(t.lexeme);
        case TokenType::OPEN_BRACKET:
            return list(); 
        case TokenType::LAMBDA:
            return lambda(); 
        case TokenType::OPEN_PAREN: {
            next();
            ASTNode* t = expression();
            expect(TokenType::CLOSE_PAREN);
            return t;
        }
        default:
            parse_error("Bad factor");
    }
    std::cerr << "Returning nullptr\n";
    return nullptr;
}

// list = "[" [ expression { "," expression } ] "]" .
List* Parser::list() {
    expect(TokenType::OPEN_BRACKET);
    std::vector<ASTNode*> elements;

    if (type != TokenType::CLOSE_BRACKET) { /* Empty lists */
        elements.push_back(expression());
        while (type == TokenType::COMMA) {
            next();
            elements.push_back(expression());
        }
    }
    expect(TokenType::CLOSE_BRACKET);
    return new List(elements);
}

// lambda = "\" ident "." expression .
ASTNode* Parser::lambda() {
    expect(TokenType::LAMBDA);
    expect(TokenType::IDENTIFIER);
    expect(TokenType::PERIOD);
    expression();

    // TODO:
    return nullptr;
}
