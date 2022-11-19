#include "common.hpp"

#ifndef PARSER_HELPER_H
#define PARSER_HELPER_H

TOKEN getNextToken();
void putBackToken(TOKEN tok);
void Match(TOKEN_TYPE expectedTokenType, std::string errMessage);
TOKEN GetIdentAndMatch();
TOKEN PeekToken();
bool ValidType();
bool ValidExprStart();


#endif