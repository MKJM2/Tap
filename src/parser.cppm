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
  FunctionDef(std::string& name) : ASTNode(FUNC_DEF), name_(std::move(name)) {}
  FunctionDef(std::string& name,
              std::vector<std::string>& args,
              std::vector<std::unique_ptr<ASTNode>>& statements)
      : ASTNode(FUNC_DEF)
      , name_(std::move(name))
      , args_(args) 
      , statements_(std::move(statements)) {}

  // Getters
  const std::string name() const { return name_ ; }
  const std::vector<std::string> args() const { return args_ ; }
  const std::vector<std::unique_ptr<ASTNode>>& statements() const { return statements_; }

  // Setters
  void addArg(const std::string arg) { args_.push_back(arg); }
  void addStatement(std::unique_ptr<ASTNode> statement) {
      statements_.push_back(std::move(statement));
  }

private:
    std::string name_;
    std::vector<std::string> args_;
    std::vector<std::unique_ptr<ASTNode>> statements_;
};

class FunctionCall : public ASTNode {
public:
  FunctionCall() = delete;
  FunctionCall(std::string s, std::unique_ptr<ASTNode> args)
    : ASTNode(FUNC_CALL)
    , name_(std::move(s))
    , args_(std::move(args)) {}

  // Getters
  const std::string name() const { return name_ ; }
  const ASTNode *args() const { return args_.get() ; }

private:
    std::string name_;
    std::unique_ptr<ASTNode> args_;
};


class Factor : public ASTNode {
public:
  Factor() = delete;
  Factor(std::unique_ptr<ASTNode> node) : ASTNode(FACTOR), child_(std::move(node)) {
     assert(node->type() == IDENTIFIER ||
            node->type() == INT ||
            node->type() == STRING ||
            node->type() == LIST ||
            node->type() == LAMBDA ||
            node->type() == FUNC_CALL ||
            node->type() == EXPRESSION);
  }

  // Getters
  const ASTNode *child() const { return child_.get() ; }

private:
    std::unique_ptr<ASTNode> child_;
};


class List : public ASTNode {
public:
  List();
  List(std::vector<std::unique_ptr<ASTNode>>& elements) 
  : ASTNode(LIST)
  , elements_(std::move(elements)) {}

  // Getters
  const ASTNode* child(int idx) const {
    assert(idx < elements_.size());
    return elements_[idx].get();
  }
  const std::vector<std::unique_ptr<ASTNode>>& elements() const { return elements_; }

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

    Term (std::unique_ptr<ASTNode> factor) : ASTNode(TERM) {
        arity_ = 1;
        factors_.push_back(std::move(factor));
    }

    void addFactor(std::unique_ptr<ASTNode> factor, const char op = '\0') {
        factors_.push_back(std::move(factor));
        operators_.push_back(op); 
        arity_++;  // we increment the arity for each added factor
    }

    const std::vector<char>& operators() const {
        return operators_;
    }

    const std::vector<std::unique_ptr<ASTNode>>& factors() const {
        return factors_;
    }

private:
    std::vector<std::unique_ptr<ASTNode>> factors_;
    std::vector<char> operators_;  // Operators between factors
};

class Expression : public ASTNode {
public:
    Expression() : ASTNode(EXPRESSION) {}

    Expression (std::unique_ptr<ASTNode> term) : ASTNode(EXPRESSION) {
        arity_ = 1;
        terms_.push_back(std::move(term));
    }

    void addTerm(std::unique_ptr<ASTNode> term, const char op = '\0') {
        assert(term->type() == TERM);
        terms_.push_back(std::move(term));
        operators_.push_back(op);
        arity_++;  // we increment the arity for each added factor
    }

    const std::vector<std::unique_ptr<ASTNode>>& terms() const {
        return terms_;
    }

    const std::vector<char>& operators() const {
        return operators_;
    }

private:
    std::vector<std::unique_ptr<ASTNode>> terms_;
    std::vector<char> operators_;  // Operators between factors
};


class Assignment : public ASTNode {
public:
  Assignment() = delete;
  Assignment(std::string s, std::unique_ptr<ASTNode> rhs) 
  : ASTNode(ASSIGNMENT)
  , name_(std::move(s))
  , child_(std::move(rhs)) {
     assert(child_);
     assert(child_->type() == IDENTIFIER ||
            child_->type() == INT ||
            child_->type() == STRING ||
            child_->type() == LIST ||
            child_->type() == LAMBDA ||
            child_->type() == FUNC_CALL ||
            child_->type() == EXPRESSION);
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
    Statement(std::unique_ptr<ASTNode> node) : ASTNode(STATEMENT), child_(std::move(node)) {
        assert(node);
        assert(node->type() == ASSIGNMENT ||
            node->type() == FUNC_CALL ||
            node->type() == EXPRESSION);
  }

  // Getters
  const ASTNode *child() const { return child_.get() ; }
private:
    std::unique_ptr<ASTNode> child_;
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

    const std::vector<std::unique_ptr<ASTNode>>& statements() const { return statements_; }

private:
    std::vector<std::unique_ptr<ASTNode>> statements_;
};

export 
void printTree(const ASTNode* node) {
    if (!node) return;
    switch (node->type()) {
        case ASTNode::NodeType::INT:
            std::cout << "INT: " << static_cast<const Integer*>(node)->value();
            break;
        case ASTNode::NodeType::STRING:
            std::cout << "STRING: " << static_cast<const String*>(node)->value();
            break;
        case ASTNode::NodeType::IDENTIFIER:
            std::cout << "VARIABLE: " << static_cast<const Identifier*>(node)->name();
            break;
        case ASTNode::NodeType::FUNC_DEF:
            std::cout << "FUNC_DEF(" << static_cast<const FunctionDef*>(node)->name() << ", ";
            for (auto& arg : static_cast<const FunctionDef*>(node)->args()) {
                std::cout << "ARG: \n" << arg << "\n";
            }
            for (auto& statement : static_cast<const FunctionDef*>(node)->statements()) {
                std::cout << "STATEMENT: \n";
                printTree(statement.get());
                std::cout << "\n";
            }
            break;
        case ASTNode::NodeType::FUNC_CALL:
            std::cout << "FUNC_CALL: " << static_cast<const FunctionCall*>(node)->name() << "\n";
            printTree(static_cast<const FunctionCall*>(node)->args());
            break;
        case ASTNode::NodeType::LAMBDA:
            std::cout << "LAMBDA\n";
            break;
        case ASTNode::NodeType::LIST:
            std::cout << "LIST [\n";
            for (auto& element : static_cast<const List*>(node)->elements()) {
                std::cout << "ELEMENT: "; 
                printTree(element.get());
                std::cout << "\n";
            }
            std::cout << "]\n";
            break;
        case ASTNode::NodeType::FACTOR:
            std::cout << "FACTOR\n";
            printTree(static_cast<const Factor*>(node)->child());
            break;
        case ASTNode::NodeType::TERM: {
            std::cout << "TERM(";
            const Term* term = static_cast<const Term*>(node);
            int n = term->factors().size();
            for (int i = 0; i < n; ++i) {
                printTree(term->factors()[i].get());
                if (i < n - 1) {
                    std::cout << term->operators()[i];
                }
            }
            std::cout << ")";
            break;
        }
        case ASTNode::NodeType::ASSIGNMENT:
            std::cout << "ASSIGNMENT(";
            std::cout << static_cast<const Assignment*>(node)->name() << ", ";
            printTree(static_cast<const Assignment*>(node)->child());
            std::cout << ")";
            break;
        case ASTNode::NodeType::EXPRESSION: {
            std::cout << "EXPRESSION(";
            const Expression* exp = static_cast<const Expression*>(node);
            int n = exp->terms().size();
            for (int i = 0; i < n; ++i) {
                printTree(exp->terms()[i].get());
                if (i < n - 1) {
                    std::cout << exp->operators()[i];
                }
            }
            std::cout << ')';
            break;
        }
        case ASTNode::NodeType::STATEMENT:
            std::cout << "STATEMENT";
            printTree(static_cast<const Statement*>(node)->child());
            break;
        case ASTNode::NodeType::PROGRAM:
            std::cout << "PROGRAM\n";
            for (auto& statement : static_cast<const Program*>(node)->statements()) {
                printTree(statement.get());
                std::cout << "\n";
            }
            std::cout << "END OF PROGRAM\n";
            break;
    }
}


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
    std::unique_ptr<ASTNode> args = nullptr;
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

std::unique_ptr<ASTNode> Parser::arglist() {
    expression();
    while (type == TokenType::COMMA) {
        next();
        expression();
    }

    return std::unique_ptr<ASTNode>(nullptr);
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

    return std::unique_ptr<ASTNode>(nullptr);
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
