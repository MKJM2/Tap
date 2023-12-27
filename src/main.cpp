import lexer;
import parser;

#include <iostream>
#include <string>
#include <vector>
#include <cstring>
#include <readline/readline.h>
#include <readline/history.h>

void printTokens(std::vector<Token>& tokens) {

	for (auto& token : tokens) {
		std::cout << "Token: '"
		          << token.lexeme
				  << "' of type "
				  << TokenType2String[static_cast<std::size_t>(token.type)]
				  << "\n";
	}
}

int main() {

	char *cstr{};
	std::string input{};
	Lexer lexer;
	Parser parser;

	while (1) {

        if ((cstr = readline(">> ")) == nullptr)
			break;

        // Exit the REPL if the user enters "exit"
		if ((input = cstr) == "exit") {
            free(cstr);
            break;
        }

		add_history(cstr);

		lexer.setSource(input);
		try {
			std::vector<Token> tokens = lexer.tokenize();
			printTokens(tokens);
		} catch (const std::exception& e) {
			std::cerr << "Error: " << e.what() << std::endl;
		}

		/*
        try {
            ASTNode* ast = parser.parse(tokens);
            // TODO: semantic analysis, interpretation (tree-walking)
            if (ast) {
                std::cout << "Parsed AST Node Type: "  \
						  << static_cast<int>(ast->getType())  \
						  << std::endl;
            }
        } catch (const std::exception& e) {
            std::cerr << "Error: " << e.what() << std::endl;
        }
		*/

        free(cstr);
	}

	return 0;
}
