import lexer;
import parser;

#include <vector>
#include <string>
#include <gtest/gtest.h>

// TODO: lexer_tester

void parser_tester(std::string input) {
    static Lexer lexer;
    static Parser parser;

    // No try/catch
    lexer.setSource(input);
    std::vector<Token> tokens = lexer.tokenize();
    parser.setTokens(tokens);
    parser.parse_program();
}

TEST(ParserTest, Tautology) {
    ASSERT_TRUE(true);
}

TEST(ParserTest, Empty) {
    ASSERT_NO_THROW(parser_tester(""));
}

TEST(ParserTest, ExpressionStatement) {
    ASSERT_NO_THROW(parser_tester("5   ;"));
}

TEST(ParserTest, Addition) {
    ASSERT_NO_THROW(parser_tester("1 + 5;"));
}

TEST(ParserTest, Subtraction) {
    ASSERT_NO_THROW(parser_tester("1 - 5;"));
}

TEST(ParserTest, Multiplication) {
    ASSERT_NO_THROW(parser_tester("1 * 5;"));
}

TEST(ParserTest, FloatDivision) {
    ASSERT_NO_THROW(parser_tester("9 / 2;"));
}

TEST(ParserTest, NestedArithmetic) {
    ASSERT_NO_THROW(parser_tester("(1 + 5) * 2 + 2 / 2 - 2 * (5 / (1 * 2)) ;"));
}

TEST(ParserTest, EmptyList) {
    ASSERT_NO_THROW(parser_tester("[] ;"));
}

TEST(ParserTest, IntegerList) {
    ASSERT_NO_THROW(parser_tester("[1, 2, 3, 4, 5];"));
}

TEST(ParserTest, StringList) {
    ASSERT_NO_THROW(parser_tester("[\"John\", \"Smith\"];"));
}

TEST(ParserTest, MixedList) {
    ASSERT_NO_THROW(parser_tester("[\"John\", 123];"));
}

TEST(ParserTest, ExpressionList) {
    ASSERT_NO_THROW(parser_tester("[\"John\", 123 + (2 / 3) * 2];"));
}

TEST(ParserTest, Assignment) {
    ASSERT_NO_THROW(parser_tester("x = 5;"));
}

TEST(ParserTest, TypeAnnotation) {
    ASSERT_NO_THROW(parser_tester("x : int;"));
}

TEST(ParserTest, ListTypeAnnotation) {
    ASSERT_NO_THROW(parser_tester("lst : [int] ;"));
}

TEST(ParserTest, NestedTypeAnnotation) {
    ASSERT_NO_THROW(parser_tester("lst2 : [int -> int -> [int -> int]] ;"));
}

TEST(ParserTest, ListInlineTypeAnnotation) {
    ASSERT_NO_THROW(parser_tester("z : [str] = [\"John\", \"Smith\"];"));
}

TEST(ParserTest, VariableExpression) {
    ASSERT_NO_THROW(parser_tester("x;"));
}

TEST(ParserTest, FunctionCallNoArgs) {
    ASSERT_NO_THROW(parser_tester("print();"));
}

TEST(ParserTest, FunctionCallSingleArg) {
    ASSERT_NO_THROW(parser_tester("print(\"Hello, World!\");"));
}

TEST(ParserTest, FunctionCallMultipleArgs) {
    ASSERT_NO_THROW(parser_tester("print(\"Hello, World!\", 5, 2 + 3);"));
}

TEST(ParserTest, Function) {
    ASSERT_NO_THROW(parser_tester("func fib(n) {\n \
    return fib(n - 1) + fib(n - 2); \n}"));
}

TEST(ParserTest, FunctionWithArgTypes) {
    ASSERT_NO_THROW(parser_tester("func fib(n: int) {\n \
    return fib(n - 1) + fib(n - 2); \n}"));
}

TEST(ParserTest, FunctionWithRetTypes) {
    ASSERT_NO_THROW(parser_tester("func fib(n) : int -> int {\n \
    return fib(n - 1) + fib(n - 2); \n}"));
}

TEST(ParserTest, FunctionFullyTyped) {
    ASSERT_NO_THROW(parser_tester("func fib(n: int) : int -> int {\n \
    return fib(n - 1) + fib(n - 2); \n}"));
}
