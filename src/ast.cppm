module;

#include <iostream>
#include <vector>
#include <memory>
#include <assert.h>
#include <string>

export module ast;

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

export
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


export
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


export
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


export
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

export
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

export
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

export
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

export
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


export
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


export
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


export
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
