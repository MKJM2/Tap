import lexer;
import parser;

#include <vector>
#include <string>
#include <gtest/gtest.h>

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

TEST(ParserTest, Assignment) {
    ASSERT_NO_THROW(parser_tester("x = 5;"));
}

TEST(ParserTest, TypeAnnotation) {
    ASSERT_NO_THROW(parser_tester("x : int;"));
}

TEST(ParserTest, Lists) {
    ASSERT_NO_THROW(parser_tester("z : [str] = [\"John\", \"Smith\"];"));
}

TEST(ParserTest, Function) {
    ASSERT_NO_THROW(parser_tester("func fib(n: int) : int -> int {\n \
    return fib(n - 1) + fib(n - 2); \n}"));
}
