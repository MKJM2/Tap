import lexer;
import ast;
import parser;
import interpreter;

#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <cstring>
#include <fstream>
#include <readline/readline.h>
#include <readline/history.h>

#define NAME ("Tap")
#define VERSION ("0.0.1")
#define AUTHOR ("Michal Kurek")

static void printTokens(std::vector<Token>& tokens) {
	for (auto& token : tokens) {
		std::cout << "Token: '"
		          << token.lexeme
				  << "' of type "
				  << TokenType2String[static_cast<std::size_t>(token.type)]
				  << "\n";
	}
}

static void processInput(const std::string& input) {
	static Interpreter interpreter;
    try {
		std::vector<Token> tokens = Lexer(input).tokenize();
		// std::cout << "Lexer output:\n";
		// printTokens(tokens);
		std::unique_ptr<ASTNode> ast = Parser(tokens).parse_program();
		// std::cout << "Parser output:\n";
		// printTree(ast.get());
		// Value result = Interpreter(std::move(ast)).interpret();
		interpreter.setRoot(std::move(ast));
		Value result = interpreter.interpret();
		// std::cout << "Interpreter output:\n";
		printValue(result);

    } catch (const std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
}

int main(int argc, char *argv[]) {
	if (argc > 1) {
	    // Treat command-line argument as input file
	    const char* filename = argv[1];
	    std::ifstream inputFile(filename);

	    if (!inputFile) {
		    std::cerr << "Error: Unable to open file '" << filename << "'\n";
		    return 1;
	    }

	    // Read the content of the file into a string
	    std::string fileContent((std::istreambuf_iterator<char>(inputFile)),
							    std::istreambuf_iterator<char>());

	    processInput(fileContent);
	    return 0;
    }

	// If no input file specified, we default to a REPL
	char *cstr{};
	std::string input{};

    // Print basic info
    std::cout << NAME << " " << VERSION << " (c) " << AUTHOR << " 2024" << std::endl;
    std::cout << "Built on " << __DATE__ << " " << __TIME__ << std::endl;

	while (1) {

        if ((cstr = readline(">> ")) == nullptr)
			break;

        // Exit the REPL if the user enters "exit"
		if ((input = cstr) == "exit") {
            free(cstr);
            break;
        }

        add_history(cstr);
        processInput(input);
        free(cstr);
	}

	return 0;
}
