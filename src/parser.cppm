module;

import lexer;

#include <iostream>
#include <vector>
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
    Parser(std::vector<Token> &t) : tokens_(t) {}

protected:
    TokenType next();
    TokenType peek();
    void expect(TokenType type);
    /* One function per each EBNF rule */
    void parse_program();
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

private:
    std::vector<Token> tokens_;
    int current = 0;
};

TokenType Parser::peek() {
    assert(current < tokens_.size());
    return tokens_[current].type;
}

TokenType Parser::next() {
    assert(current < tokens_.size());
    return tokens_[current++].type;
}


void Parser::expect(TokenType type) {
    assert(TokenType::IDENTIFIER <= type && type < TokenType::NUM_TOKENS);
    if (peek() != type) {
        std::cerr << "Syntax error: Expected token " 
                  << TokenType2String[static_cast<std::size_t>(type)] \
                  << " but got " 
                  << TokenType2String[static_cast<std::size_t>(peek())] \
                  << std::endl;
        std::exit(1);
    }
    next();
}

void Parser::parse_program() {
    while (peek() != TokenType::END_OF_FILE) {
        this->statement();
    }

}

void Parser::statement() {
    (void) 0;
}