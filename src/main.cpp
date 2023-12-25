import lexer;

#include <iostream>
#include <string>
#include <vector>

int main() {

	std::string test = "y = 20;";
	Lexer lexer(test);

	std::vector<Token> tokens = lexer.tokenize();

	for (auto& token : tokens) {
		std::cout << "Token: '"
		          << token.lexeme
				  << "' of type "
				  << TokenType2String[static_cast<std::size_t>(token.type)]
				  << "\n";
	}





	std::cout << "Hello world!\n";
	return 0;
}
