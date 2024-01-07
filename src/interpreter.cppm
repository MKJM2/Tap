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

    Value evaluate(ASTNode* node);
private:
    std::unique_ptr<ASTNode> root_;
    Environment env_;

    Value evaluateExpression(const Expression* node) {
        // Implement expression evaluation logic...
        return Nothing();
    }

    Value evaluateTerm(const Term* node) {
        // Implement term evaluation logic...
        return Nothing();
    }
};

Value Interpreter::evaluate(ASTNode* node) {
    if (!node) {
        std::cerr << "Empty node encountered\n";
        return Nothing();
    }

    switch (node->type()) {
        default:
            std::cerr << "Unsupported node type encountered\n";
            return Nothing();
    }

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
