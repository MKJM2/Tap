import lexer;

#include <iostream>
#include <string>
#include <vector>

void printTokens(std::string& in) {
	Lexer lexer(in);

	std::vector<Token> tokens = lexer.tokenize();

	for (auto& token : tokens) {
		std::cout << "Token: '"
		          << token.lexeme
				  << "' of type "
				  << TokenType2String[static_cast<std::size_t>(token.type)]
				  << "\n";
	}
}

int main() {

	std::string input;
	do {
		std::cout << "> ";
		std::getline(std::cin, input);
		printTokens(input);
	} while (input != "quit");

	return 0;
}
