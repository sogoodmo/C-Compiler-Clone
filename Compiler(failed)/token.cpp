#include "token.hpp"

// TOKEN struct is used to keep track of information about a token
struct TOKEN
{
	int type = -100;
	std::string lexeme;
	int lineNo;
	int columnNo;
};