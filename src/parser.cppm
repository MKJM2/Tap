module;

import lexer;

#include <iostream>
#include <vector>
#include <string>
#include <memory>
#include <assert.h>

export module parser;

export class ASTNode {
public:
    // Enum to represent different types of AST nodes
    enum NodeType {
        INT,
        STRING,
        IDENTIFIER,
        FUNC_DEF,
        FUNC_CALL,
        LAMBDA,
        LIST,
        FACTOR,
        TERM,
        ASSIGNMENT,
        EXPRESSION,
        STATEMENT,
        PROGRAM
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
    ASTNode(char8_t op, char8_t ar) : type_(op), arity_(ar) {}
    char8_t arity_;
private:
    const char8_t type_;
};

class Identifier : public ASTNode {
public:
  static bool typeof(const ASTNode *e) {
    return e->type() == IDENTIFIER;
  }

  Identifier() = delete;
  Identifier(const char* s) : ASTNode(IDENTIFIER), name_(s) {}
  Identifier(std::string s) : ASTNode(IDENTIFIER), name_(std::move(s)) {}

  // Name of the variable
  const std::string& name() const { return name_; }

private:
  std::string name_;
};


class Integer : public ASTNode {
public:
  static bool typeof(const ASTNode *e) {
    return e->type() == INT;
  }

  Integer() = delete;
  Integer(int literal) : ASTNode(INT), value_(literal) {}

  // Name of the variable
  const int value() const { return value_ ; }

private:
  int value_;
};


class String : public ASTNode {
public:
  static bool typeof(const ASTNode *e) {
    return e->type() == STRING;
  }

  String() = delete;
  String(const char* s) : ASTNode(STRING), value_(s) {}
  String(std::string s) : ASTNode(STRING), value_(std::move(s)) {}

  // Name of the variable
  const std::string value() const { return value_ ; }

private:
    std::string value_;
};


class FunctionDef : public ASTNode {
public:
  static bool typeof(const ASTNode *e) {
    return e->type() == FUNC_DEF;
  }

  FunctionDef() = delete;
  FunctionDef(std::string& s,
              std::vector<std::string>& args,
              std::vector<std::unique_ptr<ASTNode>>& statements)
      : ASTNode(FUNC_DEF)
      , name_(std::move(s))
      , args_(args) 
      , statements_(std::move(statements)) {}

  // Getters
  const std::string name() const { return name_ ; }
  const std::vector<std::string> args() const { return args_ ; }

  // Setters
  void add_arg(const std::string arg) { args_.push_back(arg); }

private:
    std::string name_;
    std::vector<std::string> args_;
    std::vector<std::unique_ptr<ASTNode>> statements_;
};

class FunctionCall : public ASTNode {
public:
  FunctionCall() = delete;
  FunctionCall(std::string& s, std::vector<std::unique_ptr<ASTNode>>& args)
    : ASTNode(FUNC_CALL)
    , name_(std::move(s))
    , args_(std::move(args)) {}

  // Getters
  const std::string name() const { return name_ ; }

  // Setters
  void add_arg(std::unique_ptr<ASTNode> arg) { args_.push_back(std::move(arg)); }

private:
    std::string name_;
    std::vector<std::unique_ptr<ASTNode>> args_;
};


class Factor : public ASTNode {
public:
  Factor() = delete;
  Factor(ASTNode *node) : ASTNode(FACTOR), child_(node) {
     assert(node->type() == IDENTIFIER ||
            node->type() == INT ||
            node->type() == STRING ||
            node->type() == LIST ||
            node->type() == LAMBDA ||
            node->type() == FUNC_CALL ||
            node->type() == EXPRESSION);
  }

  // Getters
  const ASTNode *child() const { return child_ ; }

private:
    ASTNode *child_;
};


class List : public ASTNode {
public:
  List();
  List(std::vector<std::unique_ptr<ASTNode>>& elements) 
  : ASTNode(LIST)
  , elements_(std::move(elements)) {}

  // Getters
  ASTNode* child(int idx) const {
    assert(idx < elements_.size());
    return elements_[idx].get();
 }

private:
    std::vector<std::unique_ptr<ASTNode>> elements_;
};


/*
template <unsigned arr>
class Term : public ASTNode {
public:
    Term() : ASTNode(TERM), op_('\0') {} // No factors initially, zero arity
    Term(const ASTNode* f) : ASTNode(TERM, 1), op_('\0') { factors_[0] = f; }
    Term(const Factor& f1, const char op, const Factor& f2) : ASTNode(TERM, 2) {
        op_ = op;
        factors_[0] = f1;
        factors_[1] = f2;
    }

private:
    char8_t op_;
    ASTNode* factors_[ar];
};
*/


class Term : public ASTNode {
public:
    Term() : ASTNode(TERM) {}

    Term (const ASTNode& factor) : ASTNode(TERM) {
        arity_ = 1;
        factors_.push_back(std::move(factor));
    }

    void addFactor(const ASTNode& factor, const char op = '\0') {
        assert(factor.type() == FACTOR);
        factors_.push_back(std::move(factor));
        operators_.push_back(op); 
        arity_++;  // we increment the arity for each added factor
    }

    const std::vector<ASTNode>& getFactors() const {
        return factors_;
    }

    const std::vector<char>& getOperators() const {
        return operators_;
    }

private:
    std::vector<ASTNode> factors_;
    std::vector<char> operators_;  // Operators between factors
};

class Expression : public ASTNode {
public:
    Expression() : ASTNode(EXPRESSION) {}

    Expression (const ASTNode& term) : ASTNode(EXPRESSION) {
        arity_ = 1;
        terms_.push_back(std::move(term));
    }

    void addFactor(const ASTNode& term, const char op = '\0') {
        assert(term.type() == TERM);
        terms_.push_back(std::move(term));
        operators_.push_back(op);
        arity_++;  // we increment the arity for each added factor
    }

    const std::vector<ASTNode>& getTerms() const {
        return terms_;
    }

    const std::vector<char>& getOperators() const {
        return operators_;
    }

private:
    std::vector<ASTNode> terms_;
    std::vector<char> operators_;  // Operators between factors
};


class Assignment : public ASTNode {
public:
  Assignment() = delete;
  Assignment(std::string s, std::unique_ptr<ASTNode> rhs) 
  : ASTNode(ASSIGNMENT)
  , name_(std::move(s))
  , child_(std::move(rhs)) {
     assert(rhs->type() == IDENTIFIER ||
            rhs->type() == INT ||
            rhs->type() == STRING ||
            rhs->type() == LIST ||
            rhs->type() == LAMBDA ||
            rhs->type() == FUNC_CALL ||
            rhs->type() == EXPRESSION);
  };

  // Getters
  const std::string& name() const { return name_ ; }
  const ASTNode *child() const { return child_.get() ; }

private:
    std::string name_;
    std::unique_ptr<ASTNode> child_;
};

class Statement : public ASTNode {
public:
  Statement() = delete;
  Statement(ASTNode* node) : ASTNode(STATEMENT), child_(node) {
     assert(node->type() == ASSIGNMENT ||
            node->type() == FUNC_CALL ||
            node->type() == EXPRESSION);
  }

  // Getters
  const ASTNode *child() const { return child_ ; }
private:
    ASTNode *child_;
};

class Program : public ASTNode {
public:
  Program();
  Program(std::vector<std::unique_ptr<ASTNode>>& statements) 
  : ASTNode(PROGRAM)
  , statements_(std::move(statements)) {}

  // Getters
  ASTNode* child(int idx) const {
    assert(idx < statements_.size());
    return statements_[idx].get();
 }

private:
    std::vector<std::unique_ptr<ASTNode>> statements_;
};


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
    TokenType next(int tokens = 1);
    TokenType peek();
    TokenType peekNext();
    void expect(TokenType type);
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
    std::unique_ptr<ASTNode> arglist();
    std::unique_ptr<ASTNode> typed_arglist();
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
    if (type == TokenType::IDENTIFIER) {
        Token t = curr();
        switch (peekNext()) {
            case TokenType::COLON: { /* Type annotation */
                next(2); 
                type_annotation();
                if (type != TokenType::SEMICOLON) { /* Inline type annotation */
                    // Assignment
                    expect(TokenType::ASSIGN);
                    return std::make_unique<Assignment>(t.lexeme, expression());
                }
                break;
            }
            case TokenType::ASSIGN: { /* Variable assignment */
                return assignment();
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
                return expression();
            }
        }
    } else if (type == TokenType::KEYWORD_FUNC) {
        // No semicolon required after function definition 
        return function_def();
    } else if (type == TokenType::KEYWORD_RETURN) {
        // TODO: Return statement
        next();
        return expression();
    } else { /* Try parsing an expression statement (evaluates to a value) */
        return expression();
    }

    // Each valid statement ends with a semicolon
    expect(TokenType::SEMICOLON);
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
}

// assignment = ident [ ":" type-annotation ] "=" expression { "," ident "=" expression } .
std::unique_ptr<ASTNode> Parser::assignment() {
    expect(TokenType::IDENTIFIER);
    if (type == TokenType::COLON) { /* Optional type annotation */
        next();
        type_annotation();
    }
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
std::unique_ptr<ASTNode> Parser::function_call() {
    expect(TokenType::OPEN_PAREN);
    arglist();
    expect(TokenType::CLOSE_PAREN);
}


// function_def    = "func" ident "(" typed-arglist ")" "{" {statement} "}" .
std::unique_ptr<ASTNode> Parser::function_def() {
    expect(TokenType::KEYWORD_FUNC);
    expect(TokenType::IDENTIFIER);
    expect(TokenType::OPEN_PAREN);
    typed_arglist();
    expect(TokenType::CLOSE_PAREN);
    if (type == TokenType::COLON) { /* Optional type annotation */
        next();
        type_annotation();
    }
    expect(TokenType::OPEN_BRACE);
    while (type != TokenType::CLOSE_BRACE) {
        statement();
    }
    expect(TokenType::CLOSE_BRACE);
}

std::unique_ptr<ASTNode> Parser::arglist() {
    expression();
    while (type == TokenType::COMMA) {
        next();
        expression();
    }
}

// typed_arglist         = [ expression { "," expression } ] .
std::unique_ptr<ASTNode> Parser::typed_arglist() {
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

std::unique_ptr<ASTNode> Parser::expression() {
    term();
    while (type == TokenType::OP_PLUS || type == TokenType::OP_MINUS) {
        next();
        term();
    }
}

std::unique_ptr<ASTNode> Parser::term() {
    factor();
    while (type == TokenType::OP_MULT || type == TokenType::OP_DIVIDE) {
        next();
        factor();
    }
}

// factor = ident | integer | string | list | lambda | function_call | "(" expression ")" .
std::unique_ptr<ASTNode> Parser::factor() {
    Token t = curr();
    switch (type) {
        case TokenType::IDENTIFIER: /* Identifier or function call */
            next();
            if (type == TokenType::OPEN_PAREN) { /* Function call */
                function_call();
            }
            break;
        case TokenType::INTEGER:
            next();
            return std::make_unique<Integer>(std::stoi(t.lexeme));
        case TokenType::STRING:
            next();
            return std::make_unique<String>(t.lexeme);
        case TokenType::OPEN_BRACKET:
            list(); break;
        case TokenType::LAMBDA:
            lambda(); break;
        case TokenType::OPEN_PAREN: {
            next();
            std::unique_ptr<ASTNode> t = expression();
            expect(TokenType::CLOSE_PAREN);
            return t;
        }
        default:
            parse_error("Bad factor");
    }
}

// list = "[" [ expression { "," expression } ] "]" .
std::unique_ptr<ASTNode> Parser::list() {
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
std::unique_ptr<ASTNode> Parser::lambda() {
    expect(TokenType::LAMBDA);
    expect(TokenType::IDENTIFIER);
    expect(TokenType::PERIOD);
    expression();
}
