#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;
using namespace std;

FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE {

	IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
	ASSIGN = int('='), // '='

	// delimiters
	LBRA = int('{'),  // left brace
	RBRA = int('}'),  // right brace
	LPAR = int('('),  // left parenthesis
	RPAR = int(')'),  // right parenthesis
	SC = int(';'),    // semicolon
	COMMA = int(','), // comma

	// types
	INT_TOK = -2,   // "int"
	VOID_TOK = -3,  // "void"
	FLOAT_TOK = -4, // "float"
	BOOL_TOK = -5,  // "bool"

	// keywords
	EXTERN = -6,  // "extern"
	IF = -7,      // "if"
	ELSE = -8,    // "else"
	WHILE = -9,   // "while"
	RETURN = -10, // "return"
	// TRUE   = -12,     // "true"
	// FALSE   = -13,     // "false"

	// literals
	INT_LIT = -14,   // [0-9]+
	FLOAT_LIT = -15, // [0-9]+.[0-9]+
	BOOL_LIT = -16,  // "true" or "false" key words

	// logical operators
	AND = -17, // "&&"
	OR = -18,  // "||"

	// operators
	PLUS = int('+'),    // addition or unary plus
	MINUS = int('-'),   // substraction or unary negative
	ASTERIX = int('*'), // multiplication
	DIV = int('/'),     // division
	MOD = int('%'),     // modular
	NOT = int('!'),     // unary negation

	// comparison operators
	EQ = -19,      // equal
	NE = -20,      // not equal
	LE = -21,      // less than or equal to
	LT = int('<'), // less than
	GE = -23,      // greater than or equal to
	GT = int('>'), // greater than

	// special tokens
	EOF_TOK = 0, // signal end of file

	// invalid
	INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
	int type = -100;
	std::string lexeme;
	int lineNo;
	int columnNo;
};

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type) {
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
static TOKEN gettok() {
	static int LastChar = ' ';
	static int NextChar = ' ';

	// Skip any whitespace.
	while (isspace(LastChar)) {
		if (LastChar == '\n' || LastChar == '\r') {
			lineNo++;
			columnNo = 1;
		}
		LastChar = getc(pFile);
		columnNo++;
	}

	if (isalpha(LastChar) ||
			(LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
		IdentifierStr = LastChar;
		columnNo++;

		while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
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
		if (IdentifierStr == "true") {
			BoolVal = true;
			return returnTok("true", BOOL_LIT);
		}
		if (IdentifierStr == "false") {
			BoolVal = false;
			return returnTok("false", BOOL_LIT);
		}

		return returnTok(IdentifierStr.c_str(), IDENT);
	}

	if (LastChar == '=') {
		NextChar = getc(pFile);
		if (NextChar == '=') { // EQ: ==
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("==", EQ);
		} else {
			LastChar = NextChar;
			columnNo++;
			return returnTok("=", ASSIGN);
		}
	}

	if (LastChar == '{') {
		LastChar = getc(pFile);
		columnNo++;
		return returnTok("{", LBRA);
	}
	if (LastChar == '}') {
		LastChar = getc(pFile);
		columnNo++;
		return returnTok("}", RBRA);
	}
	if (LastChar == '(') {
		LastChar = getc(pFile);
		columnNo++;
		return returnTok("(", LPAR);
	}
	if (LastChar == ')') {
		LastChar = getc(pFile);
		columnNo++;
		return returnTok(")", RPAR);
	}
	if (LastChar == ';') {
		LastChar = getc(pFile);
		columnNo++;
		return returnTok(";", SC);
	}
	if (LastChar == ',') {
		LastChar = getc(pFile);
		columnNo++;
		return returnTok(",", COMMA);
	}

	if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
		std::string NumStr;

		if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
			do {
				NumStr += LastChar;
				LastChar = getc(pFile);
				columnNo++;
			} while (isdigit(LastChar));

			FloatVal = strtof(NumStr.c_str(), nullptr);
			return returnTok(NumStr, FLOAT_LIT);
		} else {
			do { // Start of Number: [0-9]+
				NumStr += LastChar;
				LastChar = getc(pFile);
				columnNo++;
			} while (isdigit(LastChar));

			if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
				do {
					NumStr += LastChar;
					LastChar = getc(pFile);
					columnNo++;
				} while (isdigit(LastChar));

				FloatVal = strtof(NumStr.c_str(), nullptr);
				return returnTok(NumStr, FLOAT_LIT);
			} else { // Integer : [0-9]+
				IntVal = strtod(NumStr.c_str(), nullptr);
				return returnTok(NumStr, INT_LIT);
			}
		}
	}

	if (LastChar == '&') {
		NextChar = getc(pFile);
		if (NextChar == '&') { // AND: &&
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("&&", AND);
		} else {
			LastChar = NextChar;
			columnNo++;
			return returnTok("&", int('&'));
		}
	}

	if (LastChar == '|') {
		NextChar = getc(pFile);
		if (NextChar == '|') { // OR: ||
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("||", OR);
		} else {
			LastChar = NextChar;
			columnNo++;
			return returnTok("|", int('|'));
		}
	}

	if (LastChar == '!') {
		NextChar = getc(pFile);
		if (NextChar == '=') { // NE: !=
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("!=", NE);
		} else {
			LastChar = NextChar;
			columnNo++;
			return returnTok("!", NOT);
			;
		}
	}

	if (LastChar == '<') {
		NextChar = getc(pFile);
		if (NextChar == '=') { // LE: <=
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok("<=", LE);
		} else {
			LastChar = NextChar;
			columnNo++;
			return returnTok("<", LT);
		}
	}

	if (LastChar == '>') {
		NextChar = getc(pFile);
		if (NextChar == '=') { // GE: >=
			LastChar = getc(pFile);
			columnNo += 2;
			return returnTok(">=", GE);
		} else {
			LastChar = NextChar;
			columnNo++;
			return returnTok(">", GT);
		}
	}

	if (LastChar == '/') { // could be division or could be the start of a comment
		LastChar = getc(pFile);
		columnNo++;
		if (LastChar == '/') { // definitely a comment
			do {
				LastChar = getc(pFile);
				columnNo++;
			} while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

			if (LastChar != EOF)
				return gettok();
		} else
			return returnTok("/", DIV);
	}

	// Check for end of file.  Don't eat the EOF.
	if (LastChar == EOF) {
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

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

	if (tok_buffer.size() == 0)
		tok_buffer.push_back(gettok());

	TOKEN temp = tok_buffer.front();
	tok_buffer.pop_front();

	return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
	virtual ~ASTnode() {}
	virtual Value *codegen() = 0;
	virtual std::string to_string() const {};
};

/// IntASTnode - Class for integer literals like 1, 2, 10,
class IntASTnode : public ASTnode {
	int Val;
	TOKEN Tok;
	std::string Name;

public:
	IntASTnode(TOKEN tok, int val) : Val(val), Tok(tok) {}
	virtual Value *codegen() override;


	virtual std::string to_string() const override {
		return Name;
	};
};



/* add other AST nodes as nessasary */

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//


// ----- Helper Functions ------ // 
/* Add function calls for each production */
class ParseException : public exception{
    string Err;
public:
    ParseException(string err) : Err(err) {}

    virtual const char *what() const throw(){
        return Err.c_str();
    }
};
static void Match(TOKEN_TYPE expectedTokenType, string expectedTokenLexeme){
	if (CurTok.type != expectedTokenType){
		throw ParseException("Invalid Token Error: Expected: " + expectedTokenLexeme + "\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}

	getNextToken();
}
// ----- Helper Functions End ------ // 

static void Expr();
static void Block();
static void Stmt();
static void Var_Type();
static void Type_Spec();

static void Arg_List_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
			Match(COMMA, "COMMA");
			Expr();
			Arg_List_Prime();
			break;
		case RPAR:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Arg_List(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
			Expr();
			Arg_List_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Args(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
			Arg_List();
		case RPAR:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, RPAR}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}


static void Rval_Term(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
			Match(BOOL_LIT, "BOOL_LIT");
			break;
		case FLOAT_LIT:
			Match(FLOAT_LIT, "FLOAT_LIT");
			break;
		case INT_LIT:
			Match(INT_LIT, "INT_LIT");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Ident_Prime(){
	switch (CurTok.type)
	{	
		case COMMA:
		case RPAR:
		case SC:
		case OR:
		case AND:
		case EQ:
		case NE:
		case LT:
		case GT:
		case LE:
		case GE:
		case PLUS:
		case MOD:
		case DIV:
		case ASTERIX:
		case MINUS:
			break;
		case LPAR:
			Match(LPAR, "LPAR");
			Args();
			Match(RPAR, "RPAR");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, LPAR, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MOD, DIV, ASTERIX}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Ident(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
			Rval_Term();
			break;
		case IDENT:
			Match(IDENT, "IDENT");
			Rval_Ident_Prime();
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, IDENT}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Rval_Par(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case IDENT:
			Rval_Ident();
			break;
		case LPAR:
			Match(LPAR, "LPAR");
			Expr();
			Match(RPAR, "RPAR");
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Rval_Neg(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
			Rval_Par();
			break;
		case NOT:
			Match(NOT, "NOT");
			Rval_Par();
			break;
		case MINUS:
			Match(MINUS, "MINUS");
			Rval_Par();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Mul_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
		case AND:
		case EQ:
		case NE:
		case LT:
		case GT:
		case LE:
		case GE:
		case PLUS:
			break;
		case MOD:
			Match(MOD, "MOD");
			Rval_Neg();
			Rval_Mul_Prime();
			break;
		case DIV:
			Match(DIV, "DIV");
			Rval_Neg();
			Rval_Mul_Prime();
			break;
		case ASTERIX:
			Match(ASTERIX, "ASTERIX");
			Rval_Neg();
			Rval_Mul_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MOD, DIV, ASTERIX}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Mul(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			Rval_Neg();
			Rval_Mul_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Add_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
		case AND:
		case EQ:
		case NE:
		case LT:
		case GT:
		case LE:
		case GE:
			break;
		case PLUS:
			Match(PLUS, "PLUS");
			Rval_Mul();
			Rval_Add_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Add(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			Rval_Mul();
			Rval_Add_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Cmp_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
		case AND:
		case EQ:
		case NE:
			break;
		case LT:
			Match(LT, "LT");
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		case GT:
			Match(GT, "GT");
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		case LE:
			Match(GE, "GE");
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		case GE:
			Match(GE, "GE");
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Cmp(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Eq_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
		case AND:
			break;
		case EQ:
			Match(EQ, "EQ");
			Rval_Cmp();
			Rval_Cmp_Prime();
			break;
		case NE:
			Match(NE, "NE");
			Rval_Cmp();
			Rval_Eq_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_Eq(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			Rval_Cmp();
			Rval_Eq_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_And_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
			break;
		case AND:
			Match(AND, "AND");
			Rval_Eq();
			Rval_And_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}
static void Rval_And(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			Rval_Eq();
			Rval_And_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}


static void Rval_Or_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
			break;
		case OR:
			Match(OR, "OR");
			Rval_And();
			Rval_Or_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Rval_Or(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			Rval_And();
			Rval_Or_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Expr(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
			Rval_Or();
			break;
		// Non LL(1) production 
		// Must use extra look-ahead 
		case IDENT: {
			TOKEN tmpToken = CurTok;

			TOKEN nextToken = getNextToken();
			putBackToken(nextToken); 

			CurTok = tmpToken; 

			if (nextToken.type == IDENT){
				Match(IDENT, "IDENT");
				Match(EQ, "EQ");
				Expr();
			} else{
				Rval_Or();
			}

			break;
		}
		case SC:
			Match(SC, "SC");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Return_Stmt_Prime(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
			Expr();
			Match(SC, "SC");
			break;
		case SC:
			Match(SC, "SC");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Return_Stmt(){
	switch (CurTok.type)
	{
		case RETURN:
			Match(RETURN, "RETURN");
			Return_Stmt_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {RETURN}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Else_Stmt(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		case SC:
		case RETURN:
		case IF:
		case WHILE:
		case LBRA:
		case RBRA:
			break;
		case ELSE:
			Match(ELSE, "ELSE");
			Block();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, ELSE}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void If_Stmt(){
	switch (CurTok.type)
	{
		case IF:
			Match(IF, "IF");
			Match(LPAR, "LPAR");
			Expr();
			Match(RPAR, "RPAR");
			Block();
			Else_Stmt();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {IF}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void While_Stmt(){
	switch (CurTok.type)
	{
		case WHILE:
			Match(WHILE, "WHILE");
			Match(LPAR, "LPAR");
			Expr();
			Match(RPAR, "RPAR");
			Stmt();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {WHILE}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Expr_Stmt(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
			Expr();
			Match(SC, "SC");
		case SC:
			Match(SC, "SC");
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Stmt(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		case SC:
			Expr_Stmt();
		case RETURN:	
			Return_Stmt();
		case IF:
			If_Stmt();
		case WHILE:
			While_Stmt();
		case LBRA:
			Block();
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, LBRA}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Stmt_List(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		case SC:
		case RETURN:
		case IF:
		case WHILE:
		case LBRA:
			Stmt();
			Stmt_List();
		case RBRA:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Local_Decl(){
	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Var_Type();
			Match(IDENT, "IDENT");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Local_Decls(){
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		case SC:
		case RETURN:
		case IF:
		case WHILE:
		case LBRA:
		case RBRA:
			break;
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Local_Decl();
			Local_Decls();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Block(){
	switch (CurTok.type)
	{
		case LBRA:
			Match(LBRA, "LBRA");
			Local_Decls();
			Stmt_List();
			Match(RBRA, "RBRA");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {LBRA}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Param(){
	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Var_Type();
			Match(IDENT, "IDENT");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Param_List_Prime(){
	switch (CurTok.type)
	{
		case COMMA:
			Match(COMMA, "COMMA");
			Param();
			Param_List_Prime();
			break;
		case RPAR:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Param_List(){
	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Param();
			Param_List_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Params(){
	switch (CurTok.type)
	{
		case RPAR:
			break;
		case VOID_TOK:
			Match(VOID_TOK, "VOID_TOK");
			break;
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Param_List();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {RPAR, VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}


static void Func_Decl(){
	switch (CurTok.type)
	{
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Type_Spec();
			Match(IDENT, "IDENT");
			Match(LPAR, "LPAR");
			Params();
			Match(RPAR, "RPAR");
			Block();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Var_Type(){
	switch (CurTok.type)
	{
		case BOOL_TOK:
			Match(BOOL_TOK, "BOOL_TOK");
			break;
		case FLOAT_TOK:
			Match(FLOAT_TOK, "FLOAT_TOK");
			break;
		case INT_TOK:
			Match(INT_TOK, "INT_TOK");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Type_Spec(){
	switch (CurTok.type)
	{
		case VOID_TOK:
			Match(VOID_TOK, "VOID_TOK");
			break;
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Var_Type();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	} 
}

static void Var_Decl(){
	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			Var_Type();
			Match(IDENT, "IDENT");
			Match(SC, "SC");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	} 
}

static void Decl_Prime(){
	switch (CurTok.type)
	{
		case LPAR:
			Match(LPAR, "LPAR");
			Params();
			Match(RPAR, "RPAR");
			Block();
			break;
		case SC:
			Match(SC, "SC");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {LPAR, SC}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	} 
}

static void Decl() {
	switch (CurTok.type)
	{
	case VOID_TOK:
		Match(VOID_TOK, "VOID_TOK");
		Match(IDENT, "IDENT");
		Match(LPAR, "LPAR");
		Params();
		Match(RPAR, "RPAR");
		Block();
		break;
	case BOOL_TOK:
	case FLOAT_TOK:
	case INT_TOK:
		Var_Type();
		Match(IDENT, "IDENT");
		Decl_Prime();
		break;
	default:
		throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
};

static void Decl_List_Prime(){
	switch (CurTok.type)
	{
	case VOID_TOK:
	case BOOL_TOK:
	case FLOAT_TOK:
	case INT_TOK:
		Decl();
		Decl_List_Prime();
		break;
	}
}

static void Decl_List() {
	switch (CurTok.type)
	{
	case VOID_TOK:
	case BOOL_TOK:
	case FLOAT_TOK:
	case INT_TOK:
		Decl_List_Prime();
		break;
	default:
		throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
};


static void Extern(){
	switch (CurTok.type)
	{
		case EXTERN:
			Match(EXTERN, "EXTERN");
			Type_Spec();
			Match(IDENT, "IDENT");
			Match(LPAR, "LPAR");
			Params();
			Match(RPAR, "RPAR");
			Match(SC, "SC");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {EXTERN}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Extern_List_Prime(){
	switch (CurTok.type)
	{
	case EXTERN:
		Extern();
		Extern_List_Prime();
		break;
	case VOID_TOK:
	case BOOL_TOK:
	case FLOAT_TOK:
	case INT_TOK:
		return; 
	default:
		throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
	}
}

static void Extern_List() {
	switch (CurTok.type)
	{
		case EXTERN:
			Extern();
			Extern_List_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected {EXTERN}\nGot: "+CurTok.lexeme);
	}

};

// program ::= extern_list decl_list | decl_list
static void Program(){
    switch (CurTok.type)
    {
    case VOID_TOK:
    case BOOL_TOK:
    case FLOAT_TOK:
    case INT_TOK:
        Decl_List();
        break;
    case EXTERN:
        Extern_List();
        Decl_List();
        break;
    default:
        throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}\nGot: " + CurTok.lexeme + " on line " + to_string(CurTok.lineNo));
    }
} 	

static void parser(){
	getNextToken();

	try {
		Program();
	} catch(const exception& e){
		cout << CurTok.type << endl;

		cout << e.what() << endl;
	}
}



//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
																		 const ASTnode &ast) {
	os << ast.to_string();
	return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
	if (argc == 2) {
		pFile = fopen(argv[1], "r");
		if (pFile == NULL)
			perror("Error opening file");
	} else {
		std::cout << "Usage: ./code InputFile\n";
		return 1;
	}

	// initialize line number and column numbers to zero
	lineNo = 1;
	columnNo = 1;

	// get the first token
	// getNextToken();
	// while (CurTok.type != EOF_TOK) {
	// 	fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
	// 					CurTok.type);
	// 	getNextToken();
	// }
	// fprintf(stderr, "Lexer Finished\n");

	// Make the module, which holds all the code.
	TheModule = std::make_unique<Module>("mini-c", TheContext);

	// Run the parser now.
	parser();
	fprintf(stderr, "Parsing Finished\n");

	//********************* Start printing final IR **************************
	// Print out all of the generated code into a file called output.ll
	auto Filename = "output.ll";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

	if (EC) {
		errs() << "Could not open file: " << EC.message();
		return 1;
	}
	// TheModule->print(errs(), nullptr); // print IR to terminal
	TheModule->print(dest, nullptr);
	//********************* End printing final IR ****************************

	fclose(pFile); // close the file that contains the code that was parsed
	return 0;
}
