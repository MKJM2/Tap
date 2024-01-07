module;

import ast;

#include <memory>
#include <string>
#include <iomanip>
#include <variant>
#include <iostream>
#include <functional>
#include <unordered_map>

export module interpreter;

typedef std::monostate Nothing;

/* Silence compiler warning using type-dependent expression */
template <class... T>
constexpr bool always_false = false;

// (Non-expression) statements evaluate to nothing
export
using Value = std::variant<Nothing, Integer, String, List, FunctionDef>;

Value toValue(const ASTNode* node) {
    switch (node->type()) {
        case ASTNode::INT:
            return Integer(static_cast<const Integer&>(*node));
        case ASTNode::STRING:
            return String(static_cast<const String&>(*node));
        case ASTNode::LIST:
            return List(static_cast<const List&>(*node));
        case ASTNode::FUNC_DEF:
            return FunctionDef(static_cast<const FunctionDef&>(*node));
        default:
            return Nothing();
    }
    return Nothing();
}


class Environment {
public:
    void set(const std::string& name, Value value) {
        table_[name] = value;
    }

    Value get(const std::string& name) const {
        auto it = table_.find(name);
        if (it != table_.end()) {
            return it->second;
        }
        // Handle undefined variable error
        throw std::runtime_error("Undefined variable: " + name);
    }

private:
    std::unordered_map<std::string, Value> table_;
};

export
class Interpreter {
public:
    Interpreter() {};
    Interpreter(std::unique_ptr<ASTNode> root) : root_(std::move(root)) {}

    void setRoot(std::unique_ptr<ASTNode> root) {
        root_ = std::move(root);
    }

    Value interpret() {
        return evaluate(root_.get());
    }

    Value evaluate(const ASTNode* node);
private:
    std::unique_ptr<ASTNode> root_;
    Environment env_;

    Value evaluateTerm(const Term* node);
    Value evaluateExpression(const Expression* node);
    Value evaluateFunctionCall(const FunctionCall* node);
    Value applyBinop(const Value& lhs, const Value& rhs, char op);
    Value applyUnop(const Value& val, char op);
};

Value Interpreter::evaluate(const ASTNode* node) {
    if (!node) {
        std::cerr << "Empty node encountered\n";
        return Nothing();
    }

    switch (node->type()) {
        case ASTNode::INT:
        case ASTNode::STRING:
            return toValue(node);
        case ASTNode::IDENTIFIER:
            return env_.get(static_cast<const Identifier*>(node)->name());
        case ASTNode::FUNC_DEF: {
            auto func = static_cast<const FunctionDef*>(node);
            env_.set(func->name(), toValue(node));
            return Nothing();
        }
        case ASTNode::FUNC_CALL:
            return evaluateFunctionCall(static_cast<const FunctionCall*>(node));
        case ASTNode::LAMBDA:
        case ASTNode::LIST:
            return Nothing(); // TODO: Implement
        case ASTNode::FACTOR:
            return evaluate(static_cast<const Factor*>(node)->child());
        case ASTNode::TERM:
            return evaluateTerm(static_cast<const Term*>(node));
        case ASTNode::ASSIGNMENT: {
            auto a = static_cast<const Assignment*>(node);
            Value rhs = evaluate(a->child());
            env_.set(a->name(), rhs);
            return rhs; /* Assignment evaluates to value assigned */
        }
        case ASTNode::EXPRESSION:
            return evaluateExpression(static_cast<const Expression*>(node));
        case ASTNode::PROGRAM: {
            auto prog = static_cast<const Program*>(node);
            size_t nstatements = prog->statements().size();
            Value res = Nothing();
            for (size_t i = 0; i < nstatements; ++i) {
                res = evaluate(prog->statements()[i].get());
                if (i == nstatements - 1) { 
                    /* Return value of a program is the return value of its last statement */
                    return res;
                }
            }
            return res;
        }
        default:
            std::cerr << "Unsupported node type encountered:" \
                      << node->type() << std::endl;
            return Nothing();
    }

    return Nothing();
}

Value Interpreter::evaluateTerm(const Term* term) {
    if (!term || term->factors().empty()) {
        std::cerr << "Empty or invalid term encountered\n";
        return Nothing();
    }

    // First factor is the starting value
    Value result = evaluate(term->factors().front().get());

    // Apply remaining factors and operators
    const auto& factors = term->factors();
    const auto& operators = term->operators();
    for (size_t i = 1; i < factors.size(); ++i) {
        Value nextFactorVal = evaluate(factors[i].get());
        char op = operators[i - 1];

        result = applyBinop(result, nextFactorVal, op);
    }

    return result;
}

Value Interpreter::applyBinop(const Value& lhs, const Value& rhs, char op) {
    if (std::holds_alternative<Integer>(lhs) && std::holds_alternative<Integer>(rhs)) {
        int leftVal = std::get<Integer>(lhs).value();
        int rightVal = std::get<Integer>(rhs).value();
        switch (op) {
            case '*':
                return Integer(leftVal * rightVal);
            case '/':
                // TODO: div-by-zero
                return Integer(leftVal / rightVal);
            case '+':
                return Integer(leftVal + rightVal);
            case '-':
                return Integer(leftVal - rightVal);
            default:
                std::cerr << "Unsupported operator encountered\n";
                return Nothing();
        }
    }
    else if (std::holds_alternative<String>(lhs) && std::holds_alternative<String>(rhs)) {
        std::string lStr = std::get<String>(lhs).value();
        std::string rStr = std::get<String>(rhs).value();
        switch (op) {
            case '+':
                return String(lStr + rStr);
            default:
                std::cerr << "Unsupported operator for type str\n";
                return Nothing();
        }
    }

    // Handle other types or error
    std::cerr << "Invalid types for operation\n";
    return Nothing();
}

// TODO: Currently unary operators aren't supported in the language
Value Interpreter::applyUnop(const Value& val, char op) {
    return Nothing();
}


Value Interpreter::evaluateExpression(const Expression* expr) {

    if (!expr || expr->terms().empty()) {
        std::cerr << "Empty or invalid expression encountered\n";
        return Nothing();
    }

    // Start with the first term
    Value result = evaluate(expr->terms().front().get());

    // Apply remaining terms and operators
    const auto& terms = expr->terms();
    const auto& operators = expr->operators();
    for (size_t i = 1; i < terms.size(); ++i) {
        Value nextTermVal = evaluate(terms[i].get());
        char op = operators[i - 1];

        result = applyBinop(result, nextTermVal, op);
    }

    return result;
}


Value Interpreter::evaluateFunctionCall(const FunctionCall* node) {
    return Nothing();
}


export
void printValue(const Value& value) {
    std::visit([](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Integer>) {
            std::cout << "int = " << arg.value() << "\n";
        } else if constexpr (std::is_same_v<T, String>) {
            std::cout << "str = " << std::quoted(arg.value()) << "\n";
        } else if constexpr (std::is_same_v<T, List>) {
            std::cout << "[";
            for (auto& item : arg.elements()) {
                std::cout << "Element , "; // TODO: Fix
            }
            std::cout << "]\n";
        } else if constexpr (std::is_same_v<T, FunctionDef>) {
            std::cout << "func\n";
        } else if constexpr (std::is_same_v<T, Nothing>) {
            std::cout << "Nothing\n";
        } else {
            static_assert(always_false<T>, "non-exhaustive visitor!");
        }
    }, value);
}
