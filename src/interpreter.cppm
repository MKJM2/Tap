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

export
void printValue(const Value& value) {
    std::visit([](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Integer>) {
            std::cout << arg.value() << " : int\n";
        } else if constexpr (std::is_same_v<T, String>) {
            std::cout << std::quoted(arg.value()) << " : str\n";
        } else if constexpr (std::is_same_v<T, List>) {
            std::cout << "[";
            for (auto& item : arg.elements()) {
                std::cout << "Element , "; // TODO: Fix
            }
            std::cout << "]\n";
        } else if constexpr (std::is_same_v<T, FunctionDef>) {
            std::cout << arg.name() << " : func\n";
        } else if constexpr (std::is_same_v<T, Nothing>) {
            std::cout << "null\n";
        } else {
            static_assert(always_false<T>, "non-exhaustive visitor!");
        }
    }, value);
}

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
friend class Interpreter;
public:
    void set(const std::string& name, Value value) {
        table_[name] = value;
    }

    Value get(const std::string& name) const {
        auto it = table_.find(name);
        if (it != table_.end()) {
            return it->second;
        }
        throw std::runtime_error("Undefined name: " + name);
    }

private:
    std::unordered_map<std::string, Value> table_;

    void print() const {
        std::cout << "Environment:\n";
        for (auto& [name, value] : table_) {
            std::cout << name << " :: ";
            printValue(value);
        }
    }
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

    // Default to global environment
    Value evaluate(const ASTNode* node) {
        return evaluate(node, env_);
    }

    // Evaluate in a given environment
    Value evaluate(const ASTNode* node, Environment& env);
private:
    std::unique_ptr<ASTNode> root_;
    Environment env_;

    Value evaluateTerm(const Term* node, Environment& env);
    Value evaluateExpression(const Expression* node, Environment& env);
    Value evaluateFunctionCall(const FunctionCall* node, Environment& env);
    Value applyBinop(const Value& lhs, const Value& rhs, char op);
    Value applyUnop(const Value& val, char op);
};

Value Interpreter::evaluate(const ASTNode* node, Environment& env) {
    if (!node) {
        // std::cerr << "Empty node encountered\n";
        return Nothing();
    }

    switch (node->type()) {
        case ASTNode::INT:
        case ASTNode::STRING:
            return toValue(node);
        case ASTNode::IDENTIFIER:
            return env.get(static_cast<const Identifier*>(node)->name());
        case ASTNode::FUNC_DEF: {
            auto func = static_cast<const FunctionDef*>(node);
            env.set(func->name(), toValue(node));
            return toValue(func);
        }
        case ASTNode::FUNC_CALL:
            return evaluateFunctionCall(static_cast<const FunctionCall*>(node), env);
        case ASTNode::LAMBDA:
        case ASTNode::LIST:
            return Nothing(); // TODO: Implement
        case ASTNode::FACTOR:
            return evaluate(static_cast<const Factor*>(node)->child(), env);
        case ASTNode::TERM:
            return evaluateTerm(static_cast<const Term*>(node), env);
        case ASTNode::ASSIGNMENT: {
            auto a = static_cast<const Assignment*>(node);
            Value rhs = evaluate(a->child(), env);
            env.set(a->name(), rhs);
            return rhs; /* Assignment evaluates to value assigned */
        }
        case ASTNode::EXPRESSION:
            return evaluateExpression(static_cast<const Expression*>(node), env);
        case ASTNode::PROGRAM: {
            auto prog = static_cast<const Program*>(node);
            size_t nstatements = prog->statements().size();
            Value res = Nothing();
            for (size_t i = 0; i < nstatements; ++i) {
                res = evaluate(prog->statements()[i].get(), env);
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

Value Interpreter::evaluateTerm(const Term* term, Environment &env) {
    if (!term || term->factors().empty()) {
        std::cerr << "Empty or invalid term encountered\n";
        return Nothing();
    }

    // First factor is the starting value
    Value result = evaluate(term->factors().front().get(), env);

    // Apply remaining factors and operators
    const auto& factors = term->factors();
    const auto& operators = term->operators();
    for (size_t i = 1; i < factors.size(); ++i) {
        Value nextFactorVal = evaluate(factors[i].get(), env);
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
    std::cerr << "Unary operators are not currently supported\n";
    return Nothing();
}


Value Interpreter::evaluateExpression(const Expression* expr, Environment& env) {

    if (!expr || expr->terms().empty()) {
        std::cerr << "Empty or invalid expression encountered\n";
        return Nothing();
    }

    // Start with the first term
    Value result = evaluate(expr->terms().front().get(), env);

    // Apply remaining terms and operators
    const auto& terms = expr->terms();
    const auto& operators = expr->operators();
    for (size_t i = 1; i < terms.size(); ++i) {
        Value nextTermVal = evaluate(terms[i].get(), env);
        char op = operators[i - 1];

        result = applyBinop(result, nextTermVal, op);
    }

    return result;
}

/**
 * @brief Evaluate a function call
 * @param node Function call AST node
 * @return Value returned by the function
 * @todo Implement type checking
 */
Value Interpreter::evaluateFunctionCall(const FunctionCall* node, Environment& env) {
    // Check if function defined in current environment
    Value func = env.get(node->name());
    if (!std::holds_alternative<FunctionDef>(func)) {
        std::cerr << "Undefined function: " << node->name() << std::endl;
        return Nothing();
    }
    const FunctionDef& funcDef = std::get<FunctionDef>(func);

    // Each function call has its own extended environment (copy of global env)
    Environment funcEnv(env);

    // Evaluate arguments and bind them to parameter names
    const auto& passed_args = node->args();
    const auto& args = funcDef.args();

    if (passed_args.size() != args.size()) {
        std::cerr << "Invalid number of arguments passed to function " << node->name() << "\n";
        std::cerr << "Currying is currently not implemented\n";
        return Nothing();
    }

    for (size_t i = 0; i < passed_args.size(); ++i) {
        Value argVal = evaluate(passed_args[i].get(), env);
        funcEnv.set(args[i], argVal);
    }

    // Evaluate function body in the extended environment (Dynamic scoping)
    // Note: The first encountered value-returning statement in the function body is returned
    for (auto& statement : funcDef.statements()) {
        Value res = evaluate(statement.get(), funcEnv);
        if (!std::holds_alternative<Nothing>(res)) {
            return res;
        }
    }
    std::cerr << "Unexpected end of function " << node->name() << ". Is void?\n";
    return Nothing();
}

