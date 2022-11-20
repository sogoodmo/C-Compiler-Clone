#include "parser_helper.hpp"

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
TOKEN getNextToken()
{

	if (tok_buffer.size() == 0)
		tok_buffer.push_back(gettok());

	TOKEN temp = tok_buffer.front();
	tok_buffer.pop_front();

	return CurTok = temp;
}

void putBackToken(TOKEN tok) { 
    tok_buffer.push_front(tok); 
}

/**
 * @brief Checks if the current token is the same as the expected token. If not an error is thrown
 *
 * @param expectedTokenType
 * @param errMessage
 * @param prodRule
 */
void Match(TOKEN_TYPE expectedTokenType, std::string errMessage)
{
	if (CurTok.type != expectedTokenType)
	{
		throw ParseException(errMessage, CurTok.lineNo, CurTok.columnNo);
	}
	getNextToken();
}

/**
 * @brief Get the Ident And Match object
 *
 * @param CurTok
 * @return TOKEN Token of identifer if no error, otherwise an error is a thrown
 */
TOKEN GetIdentAndMatch()
{
	TOKEN prev_token = CurTok;
	Match(TOKEN_TYPE::IDENT, "Expected identifer token. ");
	return prev_token;
}

/**
 * @brief Looks at the next token in the queue without removing it
 *
 * @return TOKEN Next token in the queue
 */
TOKEN PeekToken()
{
	TOKEN tmpToken = CurTok;

	TOKEN nextToken = getNextToken();
	putBackToken(nextToken);

	CurTok = tmpToken;

	return nextToken;
}

/**
 * @brief Checks if the current token's type is one of the valid types
 *
 * @return true
 * @return false
 */
bool ValidType()
{
	switch (CurTok.type)
	{
	case BOOL_TOK:
	case FLOAT_TOK:
	case INT_TOK:
		return true;
	default:
		return false;
	}
}

/**
 * @brief Checks if the current token is a valid token for starting an expression
 *
 * @return true
 * @return false
 */
bool ValidExprStart()
{
	switch (CurTok.type)
	{
	case BOOL_LIT:
	case FLOAT_LIT:
	case INT_LIT:
	case LPAR:
	case IDENT:
	case NOT:
	case MINUS:
		return true;
	default:
		return false;
	}
}