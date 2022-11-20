#include "make_comp.hpp"

// Global variables to be turned on and off at the markers will
bool PRINT_AST = false;
bool TERMINAL_IR = true;
bool WARNINGS = true; 


/**
 * @brief Attempts to create a Root Program AST Node
 * Any syntax errors are thrown and caught and reported,
 *
 * If no syntax errors exist, the AST Printer is ran and prints the AST for the given program
 */

std::unique_ptr<ProgramAST> root;

static void parser()
{
	getNextToken();
	try
	{
		root = Program();

		if (CurTok.type != EOF_TOK)
		{
			throw ParseException("Expected: end_of_file.", CurTok.lineNo, CurTok.columnNo);
		}

		if (PRINT_AST)
		{
			// AST Printer
			root->to_string("", "Program", false);
		}
	}
	catch (const std::exception &e)
	{
		std::cout << e.what() << std::endl
				  << "Got: " << CurTok.lexeme << " line: " << CurTok.lineNo << " col: " << CurTok.columnNo << std::endl;
		exit(1);
	}
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv)
{
	if (argc == 2)
	{
		filename = argv[1];
		pFile = fopen(argv[1], "r");
		if (pFile == NULL)
			perror("Error opening file");
	}
	else
	{
		std::cout << "Usage: ./code InputFile\n";
		return 1;
	}

	// initialize line number and column numbers to zero
	lineNo = 1;
	columnNo = 1;

	/**
	* Lexter that doesn't need to be ran  
	* 
	* get the first token
	* getNextToken();
	* while (CurTok.type != EOF_TOK) {
	* 	fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
	* 					CurTok.type);
	* 	getNextToken();
	* }
	* fprintf(stderr, "Lexer Finished\n");
	*/

	// Make the module, which holds all the code.
	TheModule = std::make_unique<llvm::Module>("mini-c", TheContext);

	// Run the parser now.
	parser();
	fprintf(stderr, "Parsing Finished\n");

	try
	{
		root->codegen();
	}
	catch (const std::exception &e)
	{
		std::cout << e.what() << std::endl;
		exit(1);
	}

	//********************* Start printing final IR **************************
	// Print out all of the generated code into a file called output.ll
	auto Filename = "output.ll";
	std::error_code EC;
	llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

	if (EC)
	{
		llvm::errs() << "Could not open file: " << EC.message();
		return 1;
	}


	if (TERMINAL_IR)
	{
		TheModule->print(llvm::errs(), nullptr); // print IR to terminal#
	}

	if (WARNINGS)
	{
		/**
		 * Output any compiler warnings that didn't result in a crash, but may result in undesierable behaivour.
		 */
		if (Warnings.size() != 0)
		{
			std::cout << "Warnings: " << std::endl;
			int warningCnt = 0;
			for (auto &Warn : Warnings)
			{
				std::cout << std::to_string(++warningCnt) << ". " << Warn.to_string() << std::endl;
			}
		}
	}

	TheModule->print(dest, nullptr);
	//********************* End printing final IR ****************************

	fclose(pFile); // close the file that contains the code that was parsed
	return 0;
}
