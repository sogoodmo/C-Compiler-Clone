#include "lexer.hpp"

TOKEN returnTok(std::string lexVal, int tok_type)
{
	TOKEN return_tok;
	return_tok.lexeme = lexVal;
	return_tok.type = tok_type;
	return_tok.lineNo = lineNo;
	return_tok.columnNo = columnNo - lexVal.length() - 1;
	return return_tok;
}


// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
TOKEN gettok()
{
	static int LastChar = ' ';
	static int NextChar = ' ';

	// Skip any whitespace.
	while (isspace(LastChar))
	{
		if (LastChar == '\n' || LastChar == '\r')
		{
			lineNo++;
			columnNo = 1;
		}
		LastChar = getc(pFile);
		columnNo++;
	}

	if (isalpha(LastChar) ||
		(LastChar == '_'))
	{ // identifier: [a-zA-Z_][a-zA-Z_0-9]*
		IdentifierStr = LastChar;
		columnNo++;

		while (isalnum((LastChar = getc(pFile))) || (LastChar == '_'))
		{
			IdentifierStr += LastChar;
			columnNo++;
		}

		if (IdentifierStr == "int")
			return returnTok("int", INT_TOK);
		if (IdentifierStr == "bool")
			return returnTok("bool", BOOL_TOK);
		if (IdentifierStr == "float")
			return returnTok("float", FLOAT_TOK);
		if (IdentifierStr == "void")
			return returnTok("void", VOID_TOK);
		if (IdentifierStr == "bool")
			return returnTok("bool", BOOL_TOK);
		if (IdentifierStr == "extern")
			return returnTok("extern", EXTERN);
		if (IdentifierStr == "if")
			return returnTok("if", IF);
		if (IdentifierStr == "else")
			return returnTok("else", ELSE);
		if (IdentifierStr == "while")
			return returnTok("while", WHILE);
		if (IdentifierStr == "return")
			return returnTok("return", RETURN);
		if (IdentifierStr == "true")
		{
			BoolVal = true;
			return returnTok("true", BOOL_LIT);
		}
		if (IdentifierStr == "false")
		{
			BoolVal = false;
			return returnTok("false", BOOL_LIT);
		}

		return returnTok(IdentifierStr.c_str(), IDENT);
	}

	if (LastChar == '=')
	{
		NextChar = getc(pFile);
		if (NextChar == '=')
		{ // EQ: ==
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("==", EQ);
		}
		else
		{
			LastChar = NextChar;
			columnNo++;
			return returnTok("=", ASSIGN);
		}
	}

	if (LastChar == '{')
	{
		LastChar = getc(pFile);
		columnNo++;
		return returnTok("{", LBRA);
	}
	if (LastChar == '}')
	{
		LastChar = getc(pFile);
		columnNo++;
		return returnTok("}", RBRA);
	}
	if (LastChar == '(')
	{
		LastChar = getc(pFile);
		columnNo++;
		return returnTok("(", LPAR);
	}
	if (LastChar == ')')
	{
		LastChar = getc(pFile);
		columnNo++;
		return returnTok(")", RPAR);
	}
	if (LastChar == ';')
	{
		LastChar = getc(pFile);
		columnNo++;
		return returnTok(";", SC);
	}
	if (LastChar == ',')
	{
		LastChar = getc(pFile);
		columnNo++;
		return returnTok(",", COMMA);
	}

	if (isdigit(LastChar) || LastChar == '.')
	{ // Number: [0-9]+.
		std::string NumStr;

		if (LastChar == '.')
		{ // Floatingpoint Number: .[0-9]+
			do
			{
				NumStr += LastChar;
				LastChar = getc(pFile);
				columnNo++;
			} while (isdigit(LastChar));

			FloatVal = strtof(NumStr.c_str(), nullptr);
			return returnTok(NumStr, FLOAT_LIT);
		}
		else
		{
			do
			{ // Start of Number: [0-9]+
				NumStr += LastChar;
				LastChar = getc(pFile);
				columnNo++;
			} while (isdigit(LastChar));

			if (LastChar == '.')
			{ // Floatingpoint Number: [0-9]+.[0-9]+)
				do
				{
					NumStr += LastChar;
					LastChar = getc(pFile);
					columnNo++;
				} while (isdigit(LastChar));

				FloatVal = strtof(NumStr.c_str(), nullptr);
				return returnTok(NumStr, FLOAT_LIT);
			}
			else
			{ // Integer : [0-9]+
				IntVal = strtod(NumStr.c_str(), nullptr);
				return returnTok(NumStr, INT_LIT);
			}
		}
	}

	if (LastChar == '&')
	{
		NextChar = getc(pFile);
		if (NextChar == '&')
		{ // AND: &&
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("&&", AND);
		}
		else
		{
			LastChar = NextChar;
			columnNo++;
			return returnTok("&", int('&'));
		}
	}

	if (LastChar == '|')
	{
		NextChar = getc(pFile);
		if (NextChar == '|')
		{ // OR: ||
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("||", OR);
		}
		else
		{
			LastChar = NextChar;
			columnNo++;
			return returnTok("|", int('|'));
		}
	}

	if (LastChar == '!')
	{
		NextChar = getc(pFile);
		if (NextChar == '=')
		{ // NE: !=
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("!=", NE);
		}
		else
		{
			LastChar = NextChar;
			columnNo++;
			return returnTok("!", NOT);
			;
		}
	}

	if (LastChar == '<')
	{
		NextChar = getc(pFile);
		if (NextChar == '=')
		{ // LE: <=
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("<=", LE);
		}
		else
		{
			LastChar = NextChar;
			columnNo++;
			return returnTok("<", LT);
		}
	}

	if (LastChar == '>')
	{
		NextChar = getc(pFile);
		if (NextChar == '=')
		{ // GE: >=
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok(">=", GE);
		}
		else
		{
			LastChar = NextChar;
			columnNo++;
			return returnTok(">", GT);
		}
	}

	if (LastChar == '/')
	{ // could be division or could be the start of a comment
		LastChar = getc(pFile);
		columnNo++;
		if (LastChar == '/')
		{ // definitely a comment
			do
			{
				LastChar = getc(pFile);
				columnNo++;
			} while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

			if (LastChar != EOF)
				return gettok();
		}
		else
			return returnTok("/", DIV);
	}

	// Check for end of file.  Don't eat the EOF.
	if (LastChar == EOF)
	{
		columnNo++;
		return returnTok("0", EOF_TOK);
	}

	// Otherwise, just return the character as its ascii value.
	int ThisChar = LastChar;
	std::string s(1, ThisChar);
	LastChar = getc(pFile);
	columnNo++;
	return returnTok(s, int(ThisChar));
}

