#ifndef PARSER_HELPER_H
#define PARSER_HELPER_H

#include "common.hpp"
#include "lexer.hpp"

TOKEN getNextToken();
void putBackToken(TOKEN tok);

void Match(TOKEN_TYPE expectedTokenType, std::string errMessage);
TOKEN GetIdentAndMatch();
TOKEN PeekToken();

bool ValidType();
bool ValidExprStart();


#endif