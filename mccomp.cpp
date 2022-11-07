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



// Turning a vector of arguments into a nice string 
// string format_args(vector<unique_ptr<ASTnode>> args){
// 	string arg_str="";

// 	for (int i = 0; i < args.size() - 1; i++){
// 		arg_str.append(args[i]->to_string());
// 		arg_str.append(", ");
// 	}
// 	return "(" + arg_str + args[args.size() - 1]->to_string() + ")";
// }

/// IntegerAST - Class for numeric literals like 1, 2, 10, 10
// class IntegerAST : public ASTnode {
// 	double Val;

// public:
// 	IntegerAST(int val) 
// 		: Val(val){}

// 	virtual string to_string() const override {
// 		return std::to_string(Val);
// 	};
// };
// /// FloatAST - Class for float literals like 1.3 2.1
// class FloatAST : public ASTnode {
// 	float Val;

// public:
// 	NumberAST(float val) 
// 		: Val(val){}

// 	virtual string to_string() const override {
// 		return std::to_string(Val);
// 	};
// };
// /// Bool - Class for boo literals like True, False
// class BoolAST : public ASTnode {
// 	bool Val;

// public:
// 	NumberAST(double val) 
// 		: Val(val){}

// 	virtual string to_string() const override {
// 		return std::to_string(Val);
// 	};
// };
// /// VariableAST - Class for variable names 
// class VariableAST : public ASTnode {
// 	string Name;
// 	string Type; 

// public:
// 	VariableAST(string name, string type) 
// 		: Name(name), Type(type) {}

// 	virtual string to_string() const override {
// 		return Type + " " + Name;
// 	};
// };
// /// BinaryExpAST - Class for variable names 
// class BinaryExprAST : public ASTnode {
// 	char Op;
// 	unique_ptr<ASTnode> LHS, RHS; 

// public:
// 	BinaryExprAST(char op, unique_ptr<ASTnode> LHS, unique_ptr<ASTnode> RHS) 
// 		: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

// 	virtual string to_string() const override {
// 		return LHS->to_string() + Op + RHS->to_string();
// 	};
// };
// /// UnaryExpAST - Class for variable names 
// class UnaryExprAST : public ASTnode {
// 	char Op;
// 	unique_ptr<ASTnode> Expr; 


// public:
// 	UnaryExprAST(char op, unique_ptr<ASTnode> Expr) 
// 		: Op(op), Expr(std::move(Expr)) {}

// 	virtual string to_string() const override {
// 		return Op + Expr->to_string();
// 	};
// };


// /// FuncCallAST - Class for function calls 
// class FuncCallAST : public ASTnode { 
// 	string FuncName;
// 	vector<unique_ptr<ASTnode>> FuncArgs; 

// public:
// 	FuncCallAST(string FuncName, vector<unique_ptr<ASTnode>> FuncArgs)
// 		: FuncName(FuncName), FuncArgs(std::move(FuncArgs)) {}
	
// 	virtual string to_string() const override {
// 		return FuncName + format_args(FuncArgs);
// 	};
// };


// class Param : public ASTnode{
// 	string Name;
// 	string Type;
	
// public:
// 	Param(string Name, string Type)
// 		: Name(Name), Type(Type) {}
	
// 	virtual string to_string() const override{
// 		return Type + " " + Name;
// 	};
// };

// // FuncSignatureAST - Class for the a function signature
// class FuncSignatureAST : public ASTnode {
// 	string FuncName;
// 	string FuncType; 
// 	vector<FuncParamAST> FuncArgs; 
	
// public:
// 	FuncSignatureAST(string FuncName, string FuncType, vector<FuncParamAST> FuncArgs) 
// 		: FuncName(FuncName), FuncType(FuncType), FuncArgs(std::move(FuncArgs)) {}

// 	virtual string to_string() const override{
// 		return FuncType + " " + FuncName + format_args(FuncArgs);
// 	};
// };


// class DeclAST; 



// class UnaryExprAST : public ASTnode {
// 	char Op;
// 	unique_ptr<ExprAST> Expr; 

// public:
// 	UnaryExprAST(char op, unique_ptr<ASTnode> Expr) 
// 		: Op(op), Expr(std::move(Expr)) {}

// 	virtual string to_string() const override {
// 		return Op + Expr->to_string();
// 	};
// };


// class ExprAST : public ASTnode{
// 	TOKEN Tok;
// 	std::string Ident;
// 	std::unique_ptr<ExprAST> Expr; 

// public:
// 	ExprAST(Token Tok, const std::string &Ident, std::unqiue_ptr<ExprAST> Expr)
// 		: Tok(std::move(Tok)), Ident(Ident), Expr(std::move(Expr)) {}
	
// 	virtual std::string to_string() const override{
		
// 		// Basically just check if Ident is empty -> then the thing else -> ident=123
// 		return Ident + " = " + Expr->to_string();
// 	}
// };

// class IfAST : public ASTnode{
// 	TOKEN Tok;
// 	std::unique_ptr<ExprAST> ConditionExpr;
// 	std::unique_ptr<BlockAST> TrueBlock; 
// 	std::unique_ptr<BlockAST> ElseBlock; 

// public:
// 	IfAST(TOKEN Tok, std::unqiue_ptr<ExprAST> ConditionExpr, std::unique_ptr<BlockAST> TrueBlock, std::unqiue_ptr<BlockAST> ElseBlock)
// 		: Tok(std::move(Tok)), ConditionExpr(std::move(ConditionExpr)), TrueBlock(std::move(TrueBlock)), ElseBlock(std::move(ElseBlock)) {}
	
// 	virtual std::string to_string() const override{
// 		return "IF ( " + ConditionExpr->to_string() + " )" + "-finish this later";
//  	};
// };
// class WhileAST : public ASTnode{
// 	TOKEN Tok; 
// 	std::unique_ptr<ExprAST> ConditionExpr;
// 	std::unique_ptr<BlockAST> LoopBlock; 

// public: 
// 	WhileAST(TOKEN Tok, std::unqiue_ptr<ExprAST> ConditionExpr, std::unique_ptr<BlockAST> LoopBlock)
// 		: Tok(std::move(Tok)), ConditionExpr(std::move(ConditionExpr)), LoopBlock(std::move(LoopBlock)) {}
	
// 	virtual std::string to_string() const override{
// 		return "WHILE ( " + ConditionExpr->to_string() + " )" + "-finish this later";
// 	};
// };

// class StmtAST : public ASTnode{
// 	TOKEN Tok; 
// 	std::unique_ptr<ExprAST> Expr;
// 	std::unique_ptr<IfAST> If;
// 	std::unique_ptr<WhileAST> While;
// 	std::unique_ptr<ExprAST> Return;

// public:
// 	StmtAST(TOKEN Tok, std::unique_ptr<ExprAST> Expr, std::unique_ptr<IfAST> If, std::unique_ptr<WhileAST> While, std::unique_ptr<ReturnAST> Return)
// 		: Tok(std::move(Tok)), Expr(std::move(Expr)), If(std::move(If)), While(std::move(While)), Return(std::move(Return)) {}
	
// 	virtual std::string to_string() const override{
// 		return "fk";
// 	};
// };

// class BlockAST : public ASTnode{
// 	TOKEN Tok;
// 	std::vector<std::unique_ptr<DeclAST>> Block_Decl_List; 
// 	std::vector<std::unique_ptr<StmtAST>> Block_Stmt_List;

// public:
// 	BlockAST(std::vector<std::unique_ptr<DeclAST>> Block_Decl_List, std::vector<std::unique_ptr<StmtAST>> Block_Stmt_List, TOKEN Tok)
// 		: Block_Decl_List(std::move(Block_Decl_List)), Block_Stmt_List(std::move(Block_Stmt_List)), Tok(std::move(Tok)) {}
	
// 	virtual std::string to_string() const override{
// 		return "fl";
// 	};
// };

// class ParamAST : public ASTnode {
// 	TOKEN Tok;
// 	VAR_TYPE Type;
// 	std::string Ident;

// public:
// 	ParamAST(VAR_TYPE Type, const std::string &Ident, TOKEN Tok)
// 		: Type(Type), Ident(Ident), Tok(std::move(Tok)) {}
	
// 	virtual std::string to_string() const override{
// 		return TypeToStr(Type) + " " + Ident;
// 	};
// };

// class ExternAST : public ASTnode {
// 	TOKEN Tok; 
// 	VAR_TYPE Type;
// 	std::string Ident; 
// 	std::vector<std::unique_ptr<ParamAST>> Params_List;

// public:
// 	ExternAST(VAR_TYPE Type, const std::string &Ident, std::vector<std::unique_ptr<ParamAST>> Params_List, TOKEN Tok)
// 		: Type(Type), Ident(Ident), Params_List(std::move(Params_List)), Tok(std::move(Tok)) {}
	
// 	virtual std::string to_string() const override{
// 		return "shit";
// 	};
// };

// class DeclAST : public ASTnode{
// 	TOKEN Tok;
// 	VAR_TYPE Type;
// 	std::vector<std::unique_ptr<ParamAST>> Params_List;
// 	std::unique_ptr<BlockAST> Block; 

// public:
// 	DeclAST(std::vector<std::unique_ptr<ParamAST>> Params_List, std::unique_ptr<BlockAST> Block, VAR_TYPE Type, TOKEN Tok)
// 		: Params_List(std::move(Params_List)), Block(std::move(Block)), Tok(std::move(Tok)), Type(Type) {}

// 	virtual std::string to_string() const override{
// 		return "";
// 	};
// };

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
	virtual ~ASTnode() {}
	// virtual Value *codegen() = 0;
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const { };
};

enum VAR_TYPE {
	VOID_TYPE = 0,
	INT_TYPE,
	FLOAT_TYPE,
	BOOL_TYPE
};

const std::string TypeToStr(VAR_TYPE type)
{
    switch (type)
    {
        case VOID_TYPE: return "void";
        case INT_TYPE: return "int";
        case FLOAT_TYPE: return "float";
        case BOOL_TYPE: return "bool";
		default: return "";
    }
}

// ---- AST Declerations Start ---- // 
#pragma region 
class ProgramAST; 
class DeclAST;

class ParamAST;
class FuncDeclAST;
class FuncCallAST;

class VariableAST;
class VariableDeclAST;
class VariableAssignmentAST;

class StmtAST;
class BlockAST; 
class IfAST;
class WhileAST;
class ReturnAST; 

class ExprAST; 
class BinaryExprAST;
class UnaryExprAST; 

class IntegerAST;
class FloatAST; 
class BoolAST;
class VoidAST; 	

class StmtAST : public ASTnode {};
class ExprAST : public StmtAST {}; 
#pragma endregion
// ---- AST Declerations End ---- // 

/// ==================================== Program & Decls START !! =============================================== ///
#pragma region
// class ProgramAST : public ASTnode {
// 	std::vector<std::unique_ptr<FuncDeclAST>> Extern_List; 
// 	std::vector<std::unique_ptr<DeclAST>> Decl_List;
	

// public:
// 	ProgramAST(std::vector<std::unique_ptr<FuncDeclAST>> Extern_List, std::vector<std::unique_ptr<DeclAST>> Decl_List)
// 		: Extern_List(std::move(Extern_List)), Decl_List(std::move(Decl_List)) {}

// 	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{

// 		std::cout << nodeStr << std::endl;

// 		for (const auto &extern_decl : Extern_List){
// 			// extern_decl->to_string(prefix + (isLeft ? "│   " : "    "), "Extern", true);
// 		}
// 		for (const auto &decl : Decl_List){
// 			decl->to_string(prefix + (isLeft ? "│   " : "    "), "Global Decl", false);
// 		}
// 	};
// };

// class DeclAST : public ASTnode { 
// 	std::unique_ptr<FuncDeclAST> FuncDecl; 
// 	std::unique_ptr<VariableDeclAST> VarDecl;

// public:
// 	DeclAST(std::unique_ptr<FuncDeclAST> FuncDecl, std::unique_ptr<VariableDeclAST> VarDecl)
// 		: FuncDecl(std::move(FuncDecl)), VarDecl(std::move(VarDecl)) {}

// 	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{
		
// 		std::cout << prefix; 

// 		std::cout << (isLeft ?  "├──" : "└──");

// 		std::cout << nodeStr << std::endl;

// 		if (FuncDecl != nullptr){
// 			// FuncDecl->to_string(prefix + (isLeft ? "│   " : "    "), "Function Decl", false);
// 		}
// 		if (VarDecl != nullptr){
// 			// VarDecl->to_string(prefix + (isLeft ? "│   " : "    "), "Variable Decl", false);
// 		}
// 	};
// };

#pragma endregion
/// =================================== !! Program & Decls END !! ================================================ ///

// /// =================================== !! Functions Start !! ================================================ ///
// #pragma region
// class FuncCallAST : public ExprAST {
// 	TOKEN FuncName; 
// 	std::vector<std::unique_ptr<ExprAST>> Args; 

// public:
// 	FuncCallAST(TOKEN FuncName, std::vector<std::unique_ptr<ExprAST>> Args)
// 		: FuncName(std::move(FuncName)), Args(std::move(Args)) {}

// 	// virtual std::string to_string() const override{
// 	// 	return FuncName + " Add call";
// 	// }
// };


// class FuncDeclAST : public ASTnode{
// 	VAR_TYPE Type;
// 	TOKEN Ident; 
// 	std::vector<std::unique_ptr<ParamAST>> Params;
	
// 	//May be null - in the case of extern 
// 	std::unique_ptr<BlockAST> FuncBlock; 

// public:
// 	FuncDeclAST(TOKEN Ident, VAR_TYPE Type, std::vector<std::unique_ptr<ParamAST>> Params, std::unique_ptr<BlockAST> FuncBlock)
// 		: Ident(std::move(Ident)), Type(std::move(Type)), Params(std::move(Params)), FuncBlock(std::move(FuncBlock)) {}
	
// 	// virtual std::string to_string() const override{
// 	// 	return TypeToStr(Type) + " " + Name + Params->to_string() + FuncBlock->to_string() ;
// 	// }
// };

// class ParamAST : public ASTnode {
// 	VAR_TYPE Type;
// 	TOKEN Ident;

// public:
// 	ParamAST(TOKEN Ident, VAR_TYPE Type)
// 		: Type(std::move(Type)), Ident(std::move(Ident)) {}
	
// 	// virtual std::string to_string() const override{
// 	// 	return TypeToStr(Type) + " " + Ident;
// 	// };
// };

// #pragma endregion
// /// =================================== !! Functions End !! ================================================ ///

/// =================================== !! Variable's START !! ================================================ ///
#pragma region
class Variable : public ExprAST {
	TOKEN Ident; 

public: 
	Variable(TOKEN Ident)
		: Ident(std::move(Ident)){}
	
	// virtual std::string to_string() const override{
	// 	return Ident;
	// };
};
class VariableDeclAST : public ASTnode {
	VAR_TYPE Type; 
	TOKEN Ident; 

public: 
	VariableDeclAST(TOKEN Ident, VAR_TYPE Type)
		: Ident(std::move(Ident)), Type(std::move(Type)) {}

	// virtual std::string to_string() const override{
	// 	return TypeToStr(Type) + " " + Name;
	// };
};
class VariableAssignmentAST : public ExprAST {
	TOKEN Ident; 
	std::unique_ptr<ExprAST> Expr; 

public:
	VariableAssignmentAST(TOKEN Ident, std::unique_ptr<ExprAST> Expr)
		: Ident(std::move(Ident)), Expr(std::move(Expr)) {} 	

	// virtual std::string to_string() const override{
	// 	return Ident + " = " + Expr; 
	// };
};
#pragma endregion
/// =================================== !! Variable's END !! ================================================ ///

/// =================================== !! Block & Stmts Start !! ================================================ ///
#pragma region
// class StmtAST : public ASTnode{
// 	std::unique_ptr<ExprAST> Expr;
// 	std::unique_ptr<IfAST> If;
// 	std::unique_ptr<WhileAST> While;
// 	std::unique_ptr<ReturnAST> Return;
// 	std::unique_ptr<BlockAST> Block; 

// public:
// 	StmtAST(std::unique_ptr<ExprAST> Expr, std::unique_ptr<IfAST> If, std::unique_ptr<WhileAST> While, std::unique_ptr<ExprAST> Return, std::unique_ptr<BlockAST> Block)
// 		: Expr(std::move(Expr)), If(std::move(If)), While(std::move(While)), Return(std::move(Return)), Block(std::move(Block)) {}
	
// 	// virtual std::string to_string() const override{
// 	// 	return "fk";
// 	// };
// };

class BlockAST : public StmtAST{
	std::vector<std::unique_ptr<VariableDeclAST>> VarDecls; 
	std::vector<std::unique_ptr<StmtAST>> StmtList;

public:
	BlockAST(std::vector<std::unique_ptr<VariableDeclAST>> VarDecls, std::vector<std::unique_ptr<StmtAST>> StmtList)
		: VarDecls(std::move(VarDecls)), StmtList(std::move(StmtList)) {}
	
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{
		std::cout << prefix; 

		std::cout << (isLeft ?  "├──" : "└──");

		std::cout << "Block" << std::endl;

		for (int i=0; i<VarDecls.size()-1; i++){
			// VarDecls[i]->to_string(prefix + (isLeft ? "│   " : "    "), "LocalDecl", true);
		}
		// VarDecls[VarDecls.size()-1]->to_string(prefix + (isLeft ? "│   " : "    "), "LocalDecl", (StmtList.size() != 0));

		for (int i=0; i<StmtList.size()-1; i++){
			// StmtList[i]->to_string(prefix + (isLeft ? "│   " : "    "), "Statement", true);
		}
		// StmtList[StmtList.size()-1]->to_string(prefix + (isLeft ? "│   " : "    "), "Statement", false);
	};
};
class IfAST : public StmtAST{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<BlockAST> TrueBlock; 
	std::unique_ptr<BlockAST> ElseBlock; 
public:
	IfAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<BlockAST> TrueBlock, std::unique_ptr<BlockAST> ElseBlock)
		: ConditionExpr(std::move(ConditionExpr)), TrueBlock(std::move(TrueBlock)), ElseBlock(std::move(ElseBlock)) {}
	
	// virtual std::string to_string() const override{
	// 	return "IF ( )" + "-finish this later";
 	// };
};
class WhileAST : public StmtAST{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<StmtAST> LoopBlock; 

public: 
	WhileAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<StmtAST> LoopBlock)
		: ConditionExpr(std::move(ConditionExpr)), LoopBlock(std::move(LoopBlock)) {}
	
	// virtual std::string to_string() const override{
	// 	return "WHILE ( )" + "-finish this later";
	// };
};
class ReturnAST : public StmtAST {
	std::unique_ptr<ExprAST> ReturnExpr;

public:
	ReturnAST(std::unique_ptr<ExprAST> ReturnExpr)
		: ReturnExpr(std::move(ReturnExpr)) {}

	// virtual std::string to_string() const override{
	// 	return "RETURN ( )" + "-finish this later";
	// };
};
// 	std::unique_ptr<VariableAssignmentAST> VarAss; 
// 	std::unique_ptr<BinaryExprAST> BinaryExpr; 
// 	std::unique_ptr<UnaryExprAST> UnaryExpr;

// public:
// 	ExprAST(std::unique_ptr<VariableAssignmentAST> VarAss, std::unique_ptr<BinaryExprAST> BinaryExpr, std::unique_ptr<UnaryExprAST> UnaryExpr)
// 		: VarAss(std::move(VarAss)), BinaryExpr(std::move(BinaryExpr)), UnaryExpr(std::move(UnaryExpr)) {}
	
// 	// virtual std::string to_string() const override{
// 	// 	return "Fix this";
// 	// };
// };

#pragma endregion
/// =================================== !! Block & Stmts End !! ================================================ ///

/// =================================== !! Binary / Unary AST Start !! ================================================ ///
#pragma region
class BinaryExprAST : public ExprAST {
	TOKEN Op;
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> RHS; 

public:
	BinaryExprAST(TOKEN Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) 
		: Op(std::move(Op)), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

	// virtual std::string to_string() const override {
	// 	return "need to implement";
	// };
};
class UnaryExprAST : public ExprAST {
	TOKEN Op;
	std::unique_ptr<ExprAST> Expr; 

public:
	UnaryExprAST(TOKEN Op, std::unique_ptr<ExprAST> Expr) 
		: Op(std::move(Op)), Expr(std::move(Expr)) {}

	// virtual std::string to_string() const override {
	// 	return "need to fix";
	// };
};
#pragma endregion
/// =================================== !! Binary / Unary AST End !! ================================================ ///

/// =================================== !! Literal AST Start !! ================================================ ///
#pragma region
class IntegerAST : public ExprAST {
	TOKEN Val;

public:
	IntegerAST(TOKEN Val) 
		: Val(std::move(Val)){}

	// virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override {
	// 	return ;
	// };
};
class FloatAST : public ExprAST {
	TOKEN Val;

public:
	FloatAST(TOKEN Val) 
		: Val(std::move(Val)){}

	// virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override {
	// 	return ;
	// };
};
class BoolAST : public ExprAST {
	TOKEN Val;

public:
	BoolAST(TOKEN Val) 
		: Val(std::move(Val)){}

	// virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override {
	// 	return;
	// };
};
class VoidAST : public ExprAST {};
#pragma endregion
// =================================== !! Literal AST End !! ================================================ ///
/// =================================== !! Functions Start !! ================================================ ///
#pragma region
class ParamAST : public ASTnode {
	VAR_TYPE Type;
	TOKEN Ident;

public:
	ParamAST(TOKEN Ident, VAR_TYPE Type)
		: Type(std::move(Type)), Ident(std::move(Ident)) {}
	
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{
		std::cout << prefix; 

		std::cout << (isLeft ?  "├──" : "└──");

		std::cout << nodeStr << " " << TypeToStr(Type) << " " << Ident.lexeme << std::endl;
	};
};

class FuncCallAST : public ExprAST {
	TOKEN FuncName; 
	std::vector<std::unique_ptr<ExprAST>> Args; 

public:
	FuncCallAST(TOKEN FuncName, std::vector<std::unique_ptr<ExprAST>> Args)
		: FuncName(std::move(FuncName)), Args(std::move(Args)) {}

	// virtual std::string to_string() const override{
	// 	return FuncName + " Add call";
	// }
};


class FuncDeclAST : public ASTnode{
	VAR_TYPE Type;
	TOKEN Ident; 
	std::vector<std::unique_ptr<ParamAST>> Params;
	
	//May be null - in the case of extern 
	std::unique_ptr<BlockAST> FuncBlock; 

public:
	FuncDeclAST(TOKEN Ident, VAR_TYPE Type, std::vector<std::unique_ptr<ParamAST>> Params, std::unique_ptr<BlockAST> FuncBlock)
		: Ident(std::move(Ident)), Type(std::move(Type)), Params(std::move(Params)), FuncBlock(std::move(FuncBlock)) {}
	
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{
		std::cout << prefix; 

		std::cout << (isLeft ?  "├──" : "└──");

		std::cout << nodeStr << " " << TypeToStr(Type) << " " << Ident.lexeme << std::endl;

		for (int i=0; i<Params.size()-1; i++){
			Params[i]->to_string(prefix + (isLeft ? "│   " : "    "), "Param", true);
		}
		Params[Params.size()-1]->to_string(prefix + (isLeft ? "│   " : "    "), "Param", (FuncBlock != nullptr));
		
		if (FuncBlock != nullptr){
			FuncBlock->to_string(prefix + (isLeft ? "│  " : "    "), "Block", false);
		}

	};
};


#pragma endregion
/// =================================== !! Functions End !! ================================================ ///
/// ==================================== Program & Decls START !! =============================================== ///
#pragma region
class DeclAST : public ASTnode { 
	std::unique_ptr<FuncDeclAST> FuncDecl; 
	std::unique_ptr<VariableDeclAST> VarDecl;

public:
	DeclAST(std::unique_ptr<FuncDeclAST> FuncDecl, std::unique_ptr<VariableDeclAST> VarDecl)
		: FuncDecl(std::move(FuncDecl)), VarDecl(std::move(VarDecl)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{
		if (FuncDecl != nullptr){
			FuncDecl->to_string(prefix, "FuncDef", false);
		}
		if (VarDecl != nullptr){
			VarDecl->to_string(prefix, "VarDecl", false);
		}
	};
};

class ProgramAST : public ASTnode {
	std::vector<std::unique_ptr<FuncDeclAST>> Extern_List; 
	std::vector<std::unique_ptr<DeclAST>> Decl_List;
	

public:
	ProgramAST(std::vector<std::unique_ptr<FuncDeclAST>> Extern_List, std::vector<std::unique_ptr<DeclAST>> Decl_List)
		: Extern_List(std::move(Extern_List)), Decl_List(std::move(Decl_List)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override{

		std::cout << nodeStr << std::endl;

		for (int i=0; i<Extern_List.size()-1; i++){
			Extern_List[i]->to_string(prefix + (isLeft ? "│   " : "    "), "Extern", true);
		}
		Extern_List[Extern_List.size()-1]->to_string(prefix + (isLeft ? "│   " : "    "), "Extern", (Decl_List.size() != 0));

		for (int i=0; i<Decl_List.size()-1; i++){
			Decl_List[i]->to_string(prefix + (isLeft ? "│   " : "    "), "GlobalDecl", true);
		}
		Decl_List[Decl_List.size()-1]->to_string(prefix + (isLeft ? "│   " : "    "), "GlobalDecl", false);
	};
};
#pragma endregion
/// =================================== !! Program & Decls END !! ================================================ ///


//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

// ----- Helper Functions ------ // 
#pragma region

class ParseException : public exception{
    string Err;
public:
    ParseException(string err) : Err(err) {}

    virtual const char *what() const throw(){
        return Err.c_str();
    }
};

/**
 * @brief Checks if the current token is the same as the expected token. If not an error is thrown 
 * 
 * @param expectedTokenType 
 * @param errMessage 
 * @param prodRule 
 */
static void Match(TOKEN_TYPE expectedTokenType, string errMessage, const char * prodRule = __builtin_FUNCTION()){
	if (CurTok.type != expectedTokenType){
		throw ParseException("Invalid Token Error: " + errMessage);
	}
	// cout << "[FOR TESTING] : Production Rule: " << prodRule << endl << "Matched: " << CurTok.lexeme << endl << endl; 
	getNextToken();
}

/**
 * @brief Checks if the current token is a valid token for adding a layer of presedence 
 * 
 * @param type 
 * @return true If the next token is valid token for adding a layer of presedence
 * @return false Otherwise 
 */
static bool ValidPresedenceLayer(int type){
	bool addlayer; 
	switch(type){
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
			addlayer = true;
			break;
		default:
			addlayer = false;
			break;
	}
	return addlayer;
}

/**
 * @brief Get the Ident And Match object
 * 
 * @param CurTok 
 * @return TOKEN Token of identifer if no error, otherwise an error is a thrown 
 */
static TOKEN GetIdentAndMatch(TOKEN CurTok){
	TOKEN prev_token = CurTok; 
	Match(IDENT, "Expected identifer token. ");
	return prev_token; 
}

#pragma endregion
// ----- Helper Functions End ------ // 

// ----- Function Declerations Start ----- //
#pragma region
static void Arg_List_Prime();
static std::vector<std::unique_ptr<ExprAST>> Arg_List();
static std::vector<std::unique_ptr<ExprAST>> Args();
static void Arg_List_Prime(std::vector<std::unique_ptr<ExprAST>> &args);
static std::vector<std::unique_ptr<ExprAST>> Arg_List();
static std::vector<std::unique_ptr<ExprAST>> Args();
static std::unique_ptr<ExprAST> Rval_Term();
static std::vector<std::unique_ptr<ExprAST>> Rval_Ident_Prime();
static std::unique_ptr<ExprAST> Rval_Ident();
static std::unique_ptr<ExprAST> Rval_Par();
static std::unique_ptr<ExprAST> Rval_Neg();
static std::unique_ptr<ExprAST> Rval_Mul_Prime(std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> Rval_Mul();
static std::unique_ptr<ExprAST> Rval_Add_Prime(std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> Rval_Add();
static std::unique_ptr<ExprAST> Rval_Cmp_Prime(std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> Rval_Cmp();
static std::unique_ptr<ExprAST> Rval_Eq_Prime(std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> Rval_Eq();
static std::unique_ptr<ExprAST> Rval_And_Prime(std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> Rval_And();
static std::unique_ptr<ExprAST> Rval_Or_Prime(std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> Rval_Or();
static std::unique_ptr<ExprAST> Expr();
static std::unique_ptr<ReturnAST> Return_Stmt_Prime();
static std::unique_ptr<ReturnAST> Return_Stmt();
static std::unique_ptr<BlockAST> Else_Stmt();
static std::unique_ptr<IfAST> If_Stmt();
static std::unique_ptr<WhileAST> While_Stmt();
static std::unique_ptr<ExprAST> Expr_Stmt();
static std::unique_ptr<StmtAST> Stmt();
static void Stmt_List(std::vector<std::unique_ptr<StmtAST>> &stmt_list);
static std::unique_ptr<VariableDeclAST> Local_Decl();
static void Local_Decls(std::vector<std::unique_ptr<VariableDeclAST>> &variable_decls);
static std::unique_ptr<BlockAST> Block();
static std::unique_ptr<ParamAST> Param();
static void Param_List_Prime(std::vector<std::unique_ptr<ParamAST>> &param_list);
static std::vector<std::unique_ptr<ParamAST>> Param_List();
static std::vector<std::unique_ptr<ParamAST>> Params();
static VAR_TYPE Var_Type();
static VAR_TYPE Type_Spec();
static void Decl_Prime(std::unique_ptr<FuncDeclAST> &func_decl, std::unique_ptr<VariableDeclAST> &var_decl, VAR_TYPE type, const std::string &ident);
static std::unique_ptr<DeclAST> Decl() ;
static void Decl_List_Prime(std::unique_ptr<DeclAST> &decl_list);
static std::vector<std::unique_ptr<DeclAST>> Decl_List() ;
static std::unique_ptr<FuncDeclAST> Extern();
static void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list);
static std::vector<std::unique_ptr<FuncDeclAST>> Extern_List() ;
static std::unique_ptr<ProgramAST> Program();
#pragma endregion
// ----- Function Declerations End ----- //


// arg_list_prime ::= "," expr arg_list_prime | epsilon 
static void Arg_List_Prime(std::vector<std::unique_ptr<ExprAST>> &args){
	switch (CurTok.type)
	{
		case COMMA: 
		{
			Match(COMMA, "Expected ',' after argument");
			auto arg = Expr();
			args.push_back(std::move(arg));

			Arg_List_Prime(args);
			break;
		}
		case RPAR:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR}");
	}
}

// arg_list ::= expr arg_list_prime 
static std::vector<std::unique_ptr<ExprAST>> Arg_List(){
	std::vector<std::unique_ptr<ExprAST>> args;
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS: 
		{
			auto expr = Expr();
			args.push_back(std::move(expr));

			Arg_List_Prime(args);
			break; 	
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
	return args; 
}

// args ::= arg_list |  epsilon
static std::vector<std::unique_ptr<ExprAST>> Args(){
	std::vector<std::unique_ptr<ExprAST>> args;

	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		{
			args = Arg_List();
			break;
		}
		case RPAR: 
		{
			auto voidArg = std::make_unique<VoidAST>();

			args.push_back(std::move(voidArg));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, RPAR}");
	}
	return args;
}


// rval_term ::= INT_LIT | FLOAT_LIT | BOOL_LIT
static std::unique_ptr<ExprAST> Rval_Term(){
	std::unique_ptr<ExprAST> expr; 
	TOKEN lit_tok; 

	switch (CurTok.type)
	{
		case BOOL_LIT:
		{
			lit_tok = CurTok;
			Match(BOOL_LIT, "Expected bool literal. ");
			
			expr = std::make_unique<BoolAST>(std::move(lit_tok));
			break;
		}
		case FLOAT_LIT:
		{
			lit_tok = CurTok; 
			Match(FLOAT_LIT, "Expected float literal. ");

			expr = std::make_unique<FloatAST>(std::move(lit_tok));
			break;
		}
		case INT_LIT:
		{
			lit_tok = CurTok; 
			Match(INT_LIT, "Expected int literal. ");

			expr = std::make_unique<IntegerAST>(std::move(lit_tok));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT}");
	}
	return expr; 
}

// rval_ident_prime ::= epsilon | "(" args ")" 
static std::vector<std::unique_ptr<ExprAST>> Rval_Ident_Prime(){
	std::vector<std::unique_ptr<ExprAST>> args; 

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
		{
			Match(LPAR, "Expected '(' before function call. ");
			args = Args();
			Match(RPAR, "Expected ')' after function call. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, LPAR, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MOD, DIV, ASTERIX}");
	}
	return args;
}

// rval_ident ::= IDENT rval_ident_prime | rval_term 
static std::unique_ptr<ExprAST> Rval_Ident(){
	std::unique_ptr<ExprAST> expr; 
	// cout << "Rval_Ident" << endl;
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		{
			expr = Rval_Term();
			break;
		}
		case IDENT:
		{
			TOKEN ident = GetIdentAndMatch(CurTok);
			
			auto args = Rval_Ident_Prime();
			
			// Since identifer could be function call or variable 
			// We must identify first if it is a function call or variable 
			// Then create the correct node depending on which 
			if (args.size() == 0){
				expr = std::make_unique<Variable>(std::move(ident));
			} else{
				expr = std::make_unique<FuncCallAST>(std::move(ident), std::move(args));
			}
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, IDENT}");
	}
	return expr; 
}

// rval_par ::= "(" expr ")" | rval_ident
static std::unique_ptr<ExprAST> Rval_Par(){
	// cout << "Rval_Par" << endl;
	std::unique_ptr<ExprAST> expr; 
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case IDENT:
		{
			expr = Rval_Ident();
			break;
		}
		case LPAR:
		{
			Match(LPAR, "Expected '(' before expression. ");
			expr = Expr();
			Match(RPAR, "Expected ')' after expression. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT}");
	}
	return expr; 
}

// rval_neg ::= "-" rval_neg | "!" rval_neg | rval_par 
static std::unique_ptr<ExprAST> Rval_Neg(){
	std::unique_ptr<ExprAST> unary_expr;
	TOKEN Op_Token; 

	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		{
			unary_expr = Rval_Par();
			break;
		}
		case NOT:
		{
			Op_Token = CurTok;
			Match(NOT, "Expected '!' operator. ");

			auto expr = Rval_Neg();

			unary_expr = std::make_unique<UnaryExprAST>(std::move(Op_Token), std::move(expr));
			break;
		}
		case MINUS:
		{
			Op_Token = CurTok; 
			Match(MINUS, "Expected '-' operator. ");

			auto expr = Rval_Neg();

			unary_expr = std::make_unique<UnaryExprAST>(std::move(Op_Token), std::move(expr));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
	return unary_expr; 
}

// rval_mul_prime ::= "*" rval_neg  | "/" rval_neg  | "%" rval_neg | epsilon
static std::unique_ptr<ExprAST> Rval_Mul_Prime(std::unique_ptr<ExprAST> LHS){
	// cout << "Rval_Mul_Prime" << endl;
	std::unique_ptr<ExprAST> LHS_Prime; 
	std::unique_ptr<ExprAST> RHS; 
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token;

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
		case MINUS:
			break;
		case MOD:
		{
			Op_Token = CurTok;
			Match(MOD, "Expected '%' operator. ");

			LHS_Prime = Rval_Neg();
			RHS = Rval_Mul_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case DIV:
		{
			Op_Token = CurTok;
			Match(DIV, "Expected '/' operator. ");

			LHS_Prime = Rval_Neg();
			RHS = Rval_Mul_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case ASTERIX:
		{
			Op_Token = CurTok;
			Match(ASTERIX, "Expected '*' operator. ");

			LHS_Prime = Rval_Neg();
			RHS = Rval_Mul_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MINUS, MOD, DIV, ASTERIX}");
	}
	return expr;
}

// rval_mul ::= rval_neg rval_mul_prime 
static std::unique_ptr<ExprAST> Rval_Mul(){
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type)){
		LHS = Rval_Neg();
		expr = Rval_Mul_Prime(std::move(LHS));
	} else{
		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}

	return expr; 
}

// rval_add_prime ::= "+" rval_mul  | "-" rval_mul | epsilon
static std::unique_ptr<ExprAST> Rval_Add_Prime(std::unique_ptr<ExprAST> LHS){
	// cout << "Rval_Add_Prime" << endl;
	std::unique_ptr<ExprAST> LHS_Prime; 
	std::unique_ptr<ExprAST> RHS; 
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token;

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
		{
			Op_Token = CurTok;
			Match(PLUS, "Expected '+' operator. ");

			LHS_Prime = Rval_Mul();
			RHS = Rval_Add_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case MINUS:
		{
			Op_Token = CurTok;
			Match(MINUS, "Expected '-' operator. ");

			LHS_Prime = Rval_Mul();
			RHS = Rval_Add_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MINUS}");
	}
	return expr; 
}

// rval_add ::= rval_mul rval_add_prime 
static std::unique_ptr<ExprAST> Rval_Add(){
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type)){
		LHS = Rval_Mul();
		expr = Rval_Add_Prime(std::move(LHS));
	} else{
		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
	return expr; 
}

// rval_cmp_prime ::= "<=" rval_add | "<" rval_add | ">=" rval_add | ">" rval_add | epsilon
static std::unique_ptr<ExprAST> Rval_Cmp_Prime(std::unique_ptr<ExprAST> LHS){
	std::unique_ptr<ExprAST> LHS_Prime; 
	std::unique_ptr<ExprAST> RHS; 
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token; 

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
		{
			Op_Token = CurTok;
			Match(LT, "Expected '<' operator. ");

			LHS_Prime = Rval_Add();
			RHS = Rval_Cmp_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case GT:
		{
			Op_Token = CurTok;
			Match(GT, "Expected '>' operator. ");

			LHS_Prime = Rval_Add();
			RHS = Rval_Cmp_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case LE:
		{
			Op_Token = CurTok;
			Match(LE, "Expected '<=' operator. ");

			LHS_Prime = Rval_Add();
			RHS = Rval_Cmp_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case GE:
		{
			Op_Token = CurTok;
			Match(GE, "Exepcted '>=' operator. ");

			LHS_Prime = Rval_Add();
			RHS = Rval_Cmp_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE}");
	}
	return expr; 
}

// rval_cmp ::= rval_add rval_cmp_prime 
static std::unique_ptr<ExprAST> Rval_Cmp(){
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type)){
		LHS = Rval_Add();
		expr = Rval_Cmp_Prime(std::move(LHS));
	} else{
		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
	return expr; 
}

// rval_eq_prime ::= "==" rval_cmp | "!=" rval_cmp | epsilon
static std::unique_ptr<ExprAST> Rval_Eq_Prime(std::unique_ptr<ExprAST> LHS){
	std::unique_ptr<ExprAST> LHS_Prime; 
	std::unique_ptr<ExprAST> RHS; 
	std::unique_ptr<ExprAST> expr; 
	TOKEN Op_Token; 

	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
		case AND:
			break;
		case EQ:
		{
			Op_Token = CurTok; 
			Match(EQ, "Expected '==' operator. ");

			LHS_Prime = Rval_Cmp();
			RHS = Rval_Eq_Prime(std::move(LHS));
		
			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		case NE:
		{
			Op_Token = CurTok;
			Match(NE, "Expected '!=' operator. ");

			LHS_Prime = Rval_Cmp();
			RHS = Rval_Eq_Prime(std::move(LHS));
			
			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE}");
	}

	return expr; 
}

// rval_eq ::= rval_cmp rval_eq_prime 
static std::unique_ptr<ExprAST> Rval_Eq(){
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> expr; 

	if (ValidPresedenceLayer(CurTok.type)){
		LHS = Rval_Cmp();
		expr = Rval_Eq_Prime(std::move(LHS));
	} else{
		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}

	return expr; 
}

// rval_and_prime ::= "&&" rval_eq rval_and_prime | epsilon
static std::unique_ptr<ExprAST> Rval_And_Prime(std::unique_ptr<ExprAST> LHS){
	std::unique_ptr<ExprAST> LHS_Prime; 
	std::unique_ptr<ExprAST> RHS; 
	std::unique_ptr<ExprAST> expr; 

	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
		case OR:
			break;
		case AND:
		{
			TOKEN Op_Token = CurTok;
			Match(AND, "Expected '&&' operator. ");

			LHS_Prime = Rval_Eq();
			RHS = Rval_And_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND}");
	}
	return expr;
}

// rval_and ::= rval_eq rval_and_prime 
static std::unique_ptr<ExprAST> Rval_And(){
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> expr; 

	if (ValidPresedenceLayer(CurTok.type)){
		LHS = Rval_Eq();
		expr = Rval_And_Prime(std::move(LHS));
	} else{
		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}

	return expr; 
}


// rval_or_prime ::= "||" rval_and rval_or_prime | epsilon
static std::unique_ptr<ExprAST> Rval_Or_Prime(std::unique_ptr<ExprAST> LHS){
	std::unique_ptr<ExprAST> LHS_Prime; 
	std::unique_ptr<ExprAST> RHS; 
	std::unique_ptr<ExprAST> expr; 

	switch (CurTok.type)
	{
		case COMMA:
		case RPAR:
		case SC:
			break;
		case OR:
		{
			TOKEN Op_Token = CurTok; 
			Match(OR, "Expected '||' operator. ");

			LHS_Prime = Rval_And();
			RHS = Rval_Or_Prime(std::move(LHS));

			expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR}");
	}

	return expr; 
}


// rval_or ::= rval_and rval_or_prime 
static std::unique_ptr<ExprAST> Rval_Or(){
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> expr; 

	if (ValidPresedenceLayer(CurTok.type)){
		LHS = Rval_And();
		expr = Rval_Or_Prime(std::move(LHS));
	} else{
		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}

	return expr;
}

// expr ::= IDENT "=" expr | rval_or
static std::unique_ptr<ExprAST> Expr(){
	// cout << "Expr" << endl;
	std::unique_ptr<ExprAST> expr; 
	
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		{
			expr = Rval_Or();
			break;
		}
		// Non LL(1) production 
		// Must use extra look-ahead 
		case IDENT: 
		{
			TOKEN tmpToken = CurTok;

			TOKEN nextToken = getNextToken();
			putBackToken(nextToken); 

			CurTok = tmpToken; 

			if (nextToken.type == ASSIGN){
				
				TOKEN ident = GetIdentAndMatch(CurTok);
				Match(ASSIGN, "Expected '=' after variable identifer. ");

				auto var_expr = Expr();
				expr = std::make_unique<VariableAssignmentAST>(std::move(ident), std::move(var_expr));

			} else{
				expr = Rval_Or();
			}
			break;
		}
		case SC:
		{
			Match(SC, "Expected ';'. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
	}
	return expr; 
}

// return_stmt_prime ::= ";" | expr ";"   
static std::unique_ptr<ReturnAST> Return_Stmt_Prime(){
	std::unique_ptr<ExprAST> expr; 

	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		{
			expr = Expr();
			Match(SC, "Expected ';' after return expression. ");
			break;
		}
		case SC:
		{
			Match(SC, "Expected ';' after return keyword. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
	}
	
	return std::make_unique<ReturnAST>(std::move(expr));
}

// return_stmt ::= "return" return_stmt_prime 
static std::unique_ptr<ReturnAST> Return_Stmt(){
	std::unique_ptr<ReturnAST> return_stmt;

	switch (CurTok.type)
	{
		case RETURN:
		{
			Match(RETURN, "Expected 'return' keyword. ");
			return_stmt = Return_Stmt_Prime();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {RETURN}");
	}

	return return_stmt;
}

// else_stmt  ::= "else" block | epsilon
static std::unique_ptr<BlockAST> Else_Stmt(){
	std::unique_ptr<BlockAST> else_block; 

	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case NOT:
		case MINUS:
		case IDENT:
		case SC:
		case RETURN:
		case IF:
		case WHILE:
		case LBRA:
		case RBRA:
			break;
		case ELSE:
		{
			Match(ELSE, "Expected 'else' keyword after if statement. ");
			else_block = Block();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, ELSE}");
	}

	return else_block;
}

// if_stmt ::= "if" "(" expr ")" block else_stmt
static std::unique_ptr<IfAST> If_Stmt(){
	std::unique_ptr<ExprAST> condition_expr; 
	std::unique_ptr<BlockAST> true_block; 
	std::unique_ptr<BlockAST> else_block; 

	switch (CurTok.type)
	{
		case IF:
		{
			Match(IF, "Expected 'if' keyword. ");
			Match(LPAR, "Expected '(' before if condition. ");

			condition_expr = Expr();

			Match(RPAR, "Expected ')' after if condition. ");

			true_block = Block();
			else_block = Else_Stmt();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {IF}");
	}

	return std::make_unique<IfAST>(std::move(condition_expr), std::move(true_block), std::move(else_block)); 
}

// while_stmt ::= "while" "(" expr ")" stmt 
static std::unique_ptr<WhileAST> While_Stmt(){
	std::unique_ptr<ExprAST> condition_expr; 
	std::unique_ptr<StmtAST> loop_block; 

	switch (CurTok.type)
	{
		case WHILE:
		{
			Match(WHILE, "Expected 'While' keyword. ");
			Match(LPAR, "Expected '(' before loop condition. ");

			condition_expr = Expr();

			Match(RPAR, "Expected ')' after loop condition. ");

			loop_block = Stmt();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {WHILE}");
	}

	return std::make_unique<WhileAST>(std::move(condition_expr), std::move(loop_block));
}

// expr_stmt ::= expr ";" | ";"
static std::unique_ptr<ExprAST> Expr_Stmt(){
	// cout << "Expr_Stmt" << endl;
	std::unique_ptr<ExprAST> expr; 
	switch (CurTok.type)
	{
		case BOOL_LIT:
		case FLOAT_LIT:
		case INT_LIT:
		case LPAR:
		case IDENT:
		case NOT:
		case MINUS:
		{
			expr = Expr();
			Match(SC, "Expected ';' after expression. ");
			break;
		}
		case SC:
		{
			Match(SC, "Expected ';' after expression. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
	}
	return expr;
}

// stmt ::= expr_stmt |  block |  if_stmt |  while_stmt |  return_stmt
static std::unique_ptr<StmtAST> Stmt(){
	std::unique_ptr<StmtAST> stmt; 

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
		{
			stmt = Expr_Stmt();
			break;
		}
		case RETURN:
		{
			stmt = Return_Stmt();
			break;
		}	
		case IF:
		{
			stmt = If_Stmt();
			break;
		}
		case WHILE:
		{
			stmt = While_Stmt();
			break;
		}
		case LBRA:
		{
			stmt = Block();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, LBRA}");
	}

	return stmt;
}

// stmt_list ::= stmt stmt_list | epsilon 
static void Stmt_List(std::vector<std::unique_ptr<StmtAST>> &stmt_list){

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
		{
			auto stmt = Stmt();
			stmt_list.push_back(std::move(stmt));

			Stmt_List(stmt_list);
			break;
		}
		case RBRA:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA}");
	}
}

// local_decl ::= var_type IDENT ";"
static std::unique_ptr<VariableDeclAST> Local_Decl(){
	VAR_TYPE type; 
	TOKEN ident; 

	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			type = Var_Type();
			ident = GetIdentAndMatch(CurTok);

			Match(SC, "Expeceted ';' after variable decleration. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}

	return std::make_unique<VariableDeclAST>(std::move(ident), std::move(type));
}

// local_decls ::= local_decl local_decls | epsilon
static void Local_Decls(std::vector<std::unique_ptr<VariableDeclAST>> &variable_decls){

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
		{
			auto decl = Local_Decl();
			variable_decls.push_back(std::move(decl));

			Local_Decls(variable_decls);
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}
}

// block ::= "{" local_decls stmt_list "}"
static std::unique_ptr<BlockAST> Block(){
	std::vector<std::unique_ptr<VariableDeclAST>> variable_decls;
	std::vector<std::unique_ptr<StmtAST>> stmt_list; 
	switch (CurTok.type)
	{
		case LBRA:
		{
			Match(LBRA, "Expected '{' to declare new scope. ");

			Local_Decls(variable_decls);
			Stmt_List(stmt_list);

			Match(RBRA, "Expected '}' after statement. ");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {LBRA}");
	}

	return std::make_unique<BlockAST>(std::move(variable_decls), std::move(stmt_list));
}

// param ::= var_type IDENT
static std::unique_ptr<ParamAST> Param(){
	// cout << "Param" << endl;
	VAR_TYPE type;
	TOKEN ident; 

	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			type = Var_Type();
			ident = GetIdentAndMatch(CurTok);
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}

	return std::make_unique<ParamAST>(std::move(ident), std::move(type));
}

static void Param_List_Prime(std::vector<std::unique_ptr<ParamAST>> &param_list){
	// cout << "Param_List_Prime" << endl;
	switch (CurTok.type)
	{
		case COMMA:
		{
			Match(COMMA, "Expected ',' or ')' after function parameter");
			auto param = Param();
			param_list.push_back(std::move(param));

			Param_List_Prime(param_list);
			break;
		}
		case RPAR:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR}");
	}
}

// param_list ::= param param_list_prime 
static std::vector<std::unique_ptr<ParamAST>> Param_List(){
	std::vector<std::unique_ptr<ParamAST>> param_list; 
	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			auto param = Param();
			param_list.push_back(std::move(param));

			Param_List_Prime(param_list);
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}

	return param_list; 
}

// params ::= param_list  |  "void" | epsilon
static std::vector<std::unique_ptr<ParamAST>> Params(){
	std::vector<std::unique_ptr<ParamAST>> param_list; 
	switch (CurTok.type)
	{
		case RPAR:
			break;
		case VOID_TOK:
		{
			Match(VOID_TOK, "Expected 'void' token in function paramters");
			break;
		}
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			param_list = Param_List();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {RPAR, VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}

	return param_list; 
}



static VAR_TYPE Var_Type(){
	// cout << "Var_Type" << endl;
	VAR_TYPE type; 
	switch (CurTok.type)
	{
		case BOOL_TOK:
		{
			Match(BOOL_TOK, "Expected 'bool' keyword.");
			type = BOOL_TYPE;
			break;
		}
		case FLOAT_TOK:
		{
			Match(FLOAT_TOK, "Expected 'float' keyword.");
			type = FLOAT_TYPE;
			break;
		}
		case INT_TOK:
		{
			Match(INT_TOK, "Expected 'int' keyword.");
			type = INT_TYPE;
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}
	return type; 
}	

static VAR_TYPE Type_Spec(){
	// cout << "Type_Spec" << endl;
	VAR_TYPE type; 
	switch (CurTok.type)
	{
		case VOID_TOK:
		{
			Match(VOID_TOK, "Expected 'void' keyword.");
			type = VOID_TYPE; 
			break;
		}
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			type = Var_Type();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}");
	} 
	return type; 
}
// fun_decl ::= type_spec IDENT "(" params ")" block
// static std::unique_ptr<FuncDeclAST> Func_Decl(){
// 	// cout << "Func_Decl" << endl;
// 	VAR_TYPE type;
// 	std::unique_ptr<BlockAST> block;
// 	std::unique_ptr<ParamAST> params; 

// 	switch (CurTok.type)
// 	{
// 		case VOID_TOK:
// 		case BOOL_TOK:
// 		case FLOAT_TOK:
// 		case INT_TOK:
// 			type = Type_Spec();
// 			Match(IDENT, "Expected identifer after type decleration. ");
// 			Match(LPAR, "Expected '(' after function decleration. ");
// 			params = Params();
// 			Match(RPAR, "Expected ')' after function parameters");
// 			Block();
// 			break;
// 		default:
// 			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}");
// 	}
// }

// var_decl ::= var_type IDENT ";"
// static std::make_unique<VariableDeclAST> Var_Decl(){
// 	VAR_TYPE type;
// 	std::string ident; 

// 	switch (CurTok.type)
// 	{
// 		case BOOL_TOK:
// 		case FLOAT_TOK:
// 		case INT_TOK:
// 			type = Var_Type();

// 			TOKEN prev_token = CurTok;
// 			Match(IDENT, "Expected identifer after type decleration. ");
// 			ident = prev_token.lexeme;

// 			Match(SC, "Expeceted ';' after variable decleration. ");

// 			break;
// 		default:
// 			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
// 	} 
// 	return std::make_unique<VariableDeclAST>(ident, type);
// }

// decl_prime ::= ";" | "(" params ")" block
static void Decl_Prime(std::unique_ptr<FuncDeclAST> &func_decl, std::unique_ptr<VariableDeclAST> &var_decl, VAR_TYPE type, TOKEN ident){
	switch (CurTok.type)
	{
		case LPAR:
		{
			Match(LPAR, "Expected '(' after function decleration. ");

			auto params = Params();

			Match(RPAR, "Expected ')' after function paramters. ");

			auto block = Block();
			
			func_decl = std::make_unique<FuncDeclAST>(std::move(ident), std::move(type), std::move(params), std::move(block));
			break;
		}
		case SC:
		{
			Match(SC, "Expected ';' after variable decleration. ");

			var_decl = std::make_unique<VariableDeclAST>(std::move(ident), std::move(type));
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {LPAR, SC}");
	} 
}

// decl ::= var_type IDENT decl_prime | "void" IDENT "(" params ")" block
static std::unique_ptr<DeclAST> Decl() {
	std::unique_ptr<FuncDeclAST> func_decl; 
	std::unique_ptr<VariableDeclAST> var_decl; 

	switch (CurTok.type)
	{
		case VOID_TOK:
		{
			Match(VOID_TOK, "Expected 'void' token before function decleration. ");

			TOKEN ident = GetIdentAndMatch(CurTok);

			Match(LPAR, "Expeceted '(' after function identifer. ");
			
			auto params = Params();
			
			Match(RPAR, "Expected ')' after parameter list. ");
			
			auto block = Block();

			func_decl = std::make_unique<FuncDeclAST>(std::move(ident), std::move(VOID_TYPE), std::move(params), std::move(block));
			break;
		}
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			auto type = Var_Type();

			TOKEN ident = GetIdentAndMatch(CurTok);

			Decl_Prime(func_decl, var_decl, type, ident);
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}");
	}
	return std::make_unique<DeclAST>(std::move(func_decl), std::move(var_decl));
}

// decl_list_prime ::= decl decl_list_prime | epsilon
static void Decl_List_Prime(std::vector<std::unique_ptr<DeclAST>> &decl_list){
	// cout << "Decl_List_Prime" << endl;
	switch (CurTok.type)
	{
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			auto decl = Decl();
			decl_list.push_back(std::move(decl));

			Decl_List_Prime(decl_list);
			break;
		}
		case EOF_TOK:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EOF}");
	}
}

// decl_list ::= decl decl_list_prime 
static std::vector<std::unique_ptr<DeclAST>> Decl_List() {
	std::vector<std::unique_ptr<DeclAST>> decl_list;

	switch (CurTok.type)
	{
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			auto decl = Decl();
			decl_list.push_back(std::move(decl));

			Decl_List_Prime(decl_list);
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}
	return decl_list;
};


// extern ::= "extern" type_spec IDENT "(" params ")" ";"
static std::unique_ptr<FuncDeclAST> Extern(){
	TOKEN ident; 
	VAR_TYPE type; 
	std::vector<std::unique_ptr<ParamAST>> params; 
	std::unique_ptr<BlockAST> emptyblock; 
	switch (CurTok.type)
	{
		case EXTERN:
		{
			Match(EXTERN, "EXTERN");

			auto type = Type_Spec();
			
			ident = GetIdentAndMatch(CurTok);

			Match(LPAR, "Expected '(' after identifer keyword.");
			params = Params();
			Match(RPAR, "Expected ')' after function paramters.");

			Match(SC, "Expected ';' after function definition.");
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {EXTERN}");
	}

	return std::make_unique<FuncDeclAST>(std::move(ident), std::move(type), std::move(params), std::move(emptyblock));
}

// extern_list_prime ::= extern extern_list_prime | epsilon
static void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list){

	switch (CurTok.type)
	{
		case EXTERN:
		{
			auto e = Extern();
			extern_list.push_back(std::move(e));

			Extern_List_Prime(extern_list);
			break;
		}
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			break;  
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}");
	}
}


// extern_list ::= extern extern_list_prime 
static std::vector<std::unique_ptr<FuncDeclAST>> Extern_List() {
	std::vector<std::unique_ptr<FuncDeclAST>> extern_list; 

	switch (CurTok.type)
	{
		case EXTERN:
		{
			auto e = Extern();
			extern_list.push_back(std::move(e));

			Extern_List_Prime(extern_list);
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected {EXTERN}");
	}

	return extern_list;
};

// program ::= extern_list decl_list | decl_list
static std::unique_ptr<ProgramAST> Program(){
	std::vector<std::unique_ptr<FuncDeclAST>> extern_list;
	std::vector<std::unique_ptr<DeclAST>> decl_list;

	switch (CurTok.type)
    {
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
		{
			decl_list = Decl_List();
			break;
		}
		case EXTERN:
		{
			extern_list = Extern_List();
			decl_list = Decl_List();
			break;
		}
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}");
    }

	return std::make_unique<ProgramAST>(std::move(extern_list), std::move(decl_list));
} 	

static void parser(){
	getNextToken();

	try {
		auto P = Program();
		
		if (CurTok.type != EOF_TOK){
			throw ParseException("Invalid Token Error: \nExpected: {EOF}");
		}

		P->to_string("", "Program", false);

	} catch(const exception& e){
		cout << e.what() << endl << "Got: " << CurTok.lexeme << " (Type: " << CurTok.type << ") on-line: " << CurTok.lineNo << endl;
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

// inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
// 																		 const ASTnode &ast) {
// 	os << ast.to_string();
// 	return os;
// }

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
