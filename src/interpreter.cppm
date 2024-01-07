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
    Interpreter(std::unique_ptr<ASTNode> root) : root_(std::move(root)) {}

    Value interpret() {
        return evaluate(root_.get());
    }

    Value evaluate(const ASTNode* node);
private:
    std::unique_ptr<ASTNode> root_;
    Environment env_;

    Value evaluateTerm(const Term* node);
    Value evaluateExpression(const Expression* node);
    Value applyBinop(const Value& lhs, const Value& rhs, char op);
};

Value Interpreter::evaluate(const ASTNode* node) {
    if (!node) {
        std::cerr << "Empty node encountered\n";
        return Nothing();
    }

    switch (node->type()) {
        case ASTNode::INT:
        case ASTNode::STRING:
        case ASTNode::FUNC_DEF:
        case ASTNode::LIST:
            return toValue(node);
        case ASTNode::FACTOR:
            return evaluate(static_cast<const Factor*>(node)->child());
        case ASTNode::TERM:
            return evaluateTerm(static_cast<const Term*>(node));
        default:
            std::cerr << "Unsupported node type encountered\n";
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
                // Add division by zero check
                return Integer(leftVal / rightVal);
            // Add more operators as needed
            default:
                std::cerr << "Unsupported operator encountered\n";
                return Nothing();
        }
    }

    // Handle other types or error
    std::cerr << "Invalid types for operation\n";
    return Nothing();
}


Value Interpreter::evaluateExpression(const Expression* node) {
    return Nothing();
}


export
void printValue(const Value& value) {
    std::visit([](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Integer>) {
            std::cout << "Integer " << arg.value() << "\n";
        } else if constexpr (std::is_same_v<T, String>) {
            std::cout << "String " << std::quoted(arg.value()) << "\n";
        } else if constexpr (std::is_same_v<T, List>) {
            std::cout << "[";
            for (auto& item : arg.elements()) {
                std::cout << "Element , "; // TODO: Fix
            }
            std::cout << "]\n";
        } else if constexpr (std::is_same_v<T, FunctionDef>) {
            std::cout << "FunctionDef\n";
        } else if constexpr (std::is_same_v<T, Nothing>) {
            std::cout << "Nothing\n";
        } else {
            static_assert(always_false<T>, "non-exhaustive visitor!");
        }
    }, value);
}
