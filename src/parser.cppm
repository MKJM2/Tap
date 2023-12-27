module;

import lexer;

#include <iostream>
#include <vector>

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
    Parser(std::vector<Token> &t) : tokens(t) {}

private:
    std::vector<Token> tokens;
};