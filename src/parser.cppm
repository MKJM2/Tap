module;

import lexer;

#include <iostream>
#include <vector>

export module parser;

export class Parser {
public:
    Parser() {};
    Parser(std::vector<Token> &t) : tokens(t) {};

private:
    std::vector<Token> tokens;
};