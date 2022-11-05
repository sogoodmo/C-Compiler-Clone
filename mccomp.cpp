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
	// virtual Value *codegen() = 0;
	virtual string to_string() const {};
};

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

enum VAR_TYPE {
	VOID_TYPE = 0,
	INT_TYPE,
	FLOAT_TYPE,
	BOOL_TYPE
};

const std::string VarStr(VAR_TYPE type)
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
// 		return VarStr(Type) + " " + Ident;
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

class FuncDeclAST;
class VariableDeclAST;
class DeclAST; 
class BlockAST; 
class ExprAST; 
class StmtAST;
class IfAST;
class WhileAST; 
class BinaryExprAST;
class UnaryExprAST; 

/// ==================================== Program & Decls START !! =============================================== ///
#pragma region
class ProgramAST : public ASTnode {
	std::vector<std::unique_ptr<FuncDeclAST>> Extern_List; 
	std::vector<std::unique_ptr<DeclAST>> Decl_List;
	

public:
	ProgramAST(std::vector<std::unique_ptr<FuncDeclAST>> Extern_List, std::vector<std::unique_ptr<DeclAST>> Decl_List)
		: Extern_List(std::move(Extern_List)), Decl_List(std::move(Decl_List)) {}

	// virtual std::string to_string() const override{
	// 	return Extern_List->to_string() + " " + Decl_Func_List->to_string() + " " + Decl_Var_List->to_string();
	// };
};

class DeclAST : public ASTnode { 
	std::unique_ptr<FuncDeclAST> FuncDecl; 
	std::unique_ptr<VariableDeclAST> VarDecl;

public:
	DeclAST(std::unique_ptr<FuncDeclAST> FuncDecl, std::unique_ptr<VariableDeclAST> VarDecl)
		: FuncDecl(std::move(FuncDecl)), VarDecl(std::move(VarDecl)) {}

	// virtual std::string to_string() const override{
	// 	return Extern_List->to_string() + " " + Decl_Func_List->to_string() + " " + Decl_Var_List->to_string();
	// };
};

#pragma endregion
/// =================================== !! Program & Decls END !! ================================================ ///

/// =================================== !! Functions Start !! ================================================ ///
#pragma region

class ParamAST : public ASTnode {
	VAR_TYPE Type;
	std::string Ident;

public:
	ParamAST(const std::string &Ident, VAR_TYPE Type)
		: Type(Type), Ident(Ident) {}
	
	// virtual std::string to_string() const override{
	// 	return VarStr(Type) + " " + Ident;
	// };
};

class FuncDeclAST : public ASTnode{
	VAR_TYPE Type;
	std::string Ident; 
	std::vector<std::unique_ptr<ParamAST>> Params;
	
	//May be null - in the case of extern 
	std::unique_ptr<BlockAST> FuncBlock; 

public:
	FuncDeclAST(const std::string &Ident, VAR_TYPE Type, std::vector<std::unique_ptr<ParamAST>> Params, std::unique_ptr<BlockAST> FuncBlock)
		: Ident(Ident), Type(Type), Params(std::move(Params)), FuncBlock(std::move(FuncBlock)) {}
	
	// virtual std::string to_string() const override{
	// 	return VarStr(Type) + " " + Name + Params->to_string() + FuncBlock->to_string() ;
	// }
};

class FuncCallAST : public ASTnode {
	std::string FuncName; 
	std::vector<std::unique_ptr<ExprAST>> Args; 

public:
	FuncCallAST(const std::string &FuncName, std::vector<std::unique_ptr<ExprAST>> Args)
		: FuncName(FuncName), Args(std::move(Args)) {}

	// virtual std::string to_string() const override{
	// 	return FuncName + " Add call";
	// }
};

#pragma endregion
/// =================================== !! Functions End !! ================================================ ///

/// =================================== !! Variable's START !! ================================================ ///
#pragma region
class Variable : public ASTnode {
	std::string Ident; 

public: 
	Variable(const std::string &Ident)
		: Ident(Ident){}
	
	// virtual std::string to_string() const override{
	// 	return Ident;
	// };
};

class VariableDeclAST : public ASTnode {
	VAR_TYPE Type; 
	std::string Ident; 

public: 
	VariableDeclAST(const std::string &Ident, VAR_TYPE Type)
		: Ident(Ident), Type(Type) {}

	// virtual std::string to_string() const override{
	// 	return VarStr(Type) + " " + Name;
	// };
};
class VariableAssignment : public ASTnode {
	std::string Ident; 
	std::unique_ptr<ExprAST> Expr; 

public:
	VariableAssignment(const std::string &Ident, std::unique_ptr<ExprAST> Expr)
		: Ident(Ident), Expr(std::move(Expr)) {} 	

	// virtual std::string to_string() const override{
	// 	return Ident + " = " + Expr; 
	// };
};

#pragma endregion
/// =================================== !! Variable's END !! ================================================ ///

/// =================================== !! Block & Stmts Start !! ================================================ ///
#pragma region

class BlockAST : public ASTnode{
	std::vector<std::unique_ptr<VariableDeclAST>> VarDecls; 
	std::vector<std::unique_ptr<StmtAST>> StmtList;

public:
	BlockAST(std::vector<std::unique_ptr<VariableDeclAST>> VarDecls, std::vector<std::unique_ptr<StmtAST>> StmtList)
		: VarDecls(std::move(VarDecls)), StmtList(std::move(StmtList)) {}
	
	// virtual std::string to_string() const override{
	// 	return "fl";
	// };
};

class StmtAST : public ASTnode{
	std::unique_ptr<ExprAST> Expr;
	std::unique_ptr<IfAST> If;
	std::unique_ptr<WhileAST> While;
	std::unique_ptr<ExprAST> Return;
	std::unique_ptr<BlockAST> Block; 

public:
	StmtAST(std::unique_ptr<ExprAST> Expr, std::unique_ptr<IfAST> If, std::unique_ptr<WhileAST> While, std::unique_ptr<ExprAST> Return, std::unique_ptr<BlockAST> Block)
		: Expr(std::move(Expr)), If(std::move(If)), While(std::move(While)), Return(std::move(Return)), Block(std::move(Block)) {}
	
	// virtual std::string to_string() const override{
	// 	return "fk";
	// };
};

#pragma endregion
/// =================================== !! Block & Stmt End !! ================================================ ///

/// =================================== !! If & While & Expr AST Start !! ================================================ ///
#pragma region
class ExprAST : public ASTnode{
	std::unique_ptr<VariableAssignment> VarAss; 
	std::unique_ptr<BinaryExprAST> BinaryExpr; 
	std::unique_ptr<UnaryExprAST> UnaryExpr;

public:
	ExprAST(std::unique_ptr<VariableAssignment> VarAss, std::unique_ptr<BinaryExprAST> BinaryExpr, std::unique_ptr<UnaryExprAST> UnaryExpr)
		: VarAss(std::move(VarAss)), BinaryExpr(std::move(BinaryExpr)), UnaryExpr(std::move(UnaryExpr)) {}
	
	// virtual std::string to_string() const override{
	// 	return "Fix this";
	// };
};

class IfAST : public ASTnode{
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
class WhileAST : public ASTnode{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<BlockAST> LoopBlock; 

public: 
	WhileAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<BlockAST> LoopBlock)
		: ConditionExpr(std::move(ConditionExpr)), LoopBlock(std::move(LoopBlock)) {}
	
	// virtual std::string to_string() const override{
	// 	return "WHILE ( )" + "-finish this later";
	// };
};
#pragma endregion
/// =================================== !! If & While & Expr AST Start !! ================================================ ///

/// =================================== !! Binary / Unary AST Start !! ================================================ ///
#pragma region
/// BinaryExpAST - Class for variable names 
class BinaryExprAST : public ASTnode {
	char Op;
	std::unique_ptr<ExprAST> LHS; 
	std::unique_ptr<ExprAST> RHS; 

public:
	BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) 
		: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

	// virtual std::string to_string() const override {
	// 	return "need to implement";
	// };
};
/// UnaryExpAST - Class for variable names 
class UnaryExprAST : public ASTnode {
	char Op;
	std::unique_ptr<ExprAST> Expr; 

public:
	UnaryExprAST(char op, std::unique_ptr<ExprAST> Expr) 
		: Op(op), Expr(std::move(Expr)) {}

	// virtual std::string to_string() const override {
	// 	return "need to fix";
	// };
};
#pragma endregion
/// =================================== !! Binary / Unary AST End !! ================================================ ///

/// =================================== !! Literal AST Start !! ================================================ ///
#pragma region
//IntegerAST - Class for numeric literals like 1, 2, 10, 10
class IntegerAST : public ASTnode {
	int Val;

public:
	IntegerAST(int Val) 
		: Val(Val){}

	virtual string to_string() const override {
		return std::to_string(Val);
	};
};
/// FloatAST - Class for float literals like 1.3 2.1
class FloatAST : public ASTnode {
	float Val;

public:
	FloatAST(float Val) 
		: Val(Val){}

	virtual string to_string() const override {
		return std::to_string(Val);
	};
};

/// Bool - Class for bool literals like True, False
class BoolAST : public ASTnode {
	bool Val;

public:
	BoolAST(bool Val) 
		: Val(Val){}

	virtual string to_string() const override {
		return std::to_string(Val);
	};
};

#pragma endregion
// =================================== !! Literal AST End !! ================================================ ///

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
static void Match(TOKEN_TYPE expectedTokenType, string errMessage, const char * prodRule = __builtin_FUNCTION()){
	if (CurTok.type != expectedTokenType){
		throw ParseException("Invalid Token Error: " + errMessage);
	}
	cout << "[FOR TESTING] : Production Rule: " << prodRule << endl << "Matched: " << CurTok.lexeme << endl << endl; 
	getNextToken();
}
// ----- Helper Functions End ------ // 

static void Expr();
static void Block();
static void Stmt();
static void Var_Type();
static void Type_Spec();

static void Arg_List_Prime(){
	// cout << "Arg_List_Prime" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR}");
	}
}

static void Arg_List(){
	// cout << "Arg_List" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}

static void Args(){
	// cout << "Args" << endl;
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
			break;
		case RPAR:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, RPAR}");
	}
}


static void Rval_Term(){
	// cout << "Rval_Term" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT}");
	}
}
static void Rval_Ident_Prime(){
	// cout << "Rval_Ident_Prime" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, LPAR, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MOD, DIV, ASTERIX}");
	}
}
static void Rval_Ident(){
	// cout << "Rval_Ident" << endl;
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
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, IDENT}");
	}
}

static void Rval_Par(){
	// cout << "Rval_Par" << endl;
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
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT}");
	}
}

static void Rval_Neg(){
	// cout << "Rval_Neg" << endl;
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
			Rval_Neg();
			break;
		case MINUS:
			Match(MINUS, "MINUS");
			Rval_Neg();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}
static void Rval_Mul_Prime(){
	// cout << "Rval_Mul_Prime" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MINUS, MOD, DIV, ASTERIX}");
	}
}
static void Rval_Mul(){
	// cout << "Rval_Mul" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}
static void Rval_Add_Prime(){
	// cout << "Rval_Add_Prime" << endl;
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
		case MINUS:
			Match(MINUS, "MINUS");
			Rval_Mul();
			Rval_Add_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE, PLUS, MINUS}");
	}
}
static void Rval_Add(){
	// cout << "Rval_Add" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}
static void Rval_Cmp_Prime(){
	// cout << "Rval_Cmp_Prime" << endl;
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
			Match(LE, "LE");
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		case GE:
			Match(GE, "GE");
			Rval_Add();
			Rval_Cmp_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE, LT, GT, LE, GE}");
	}
}
static void Rval_Cmp(){
	// cout << "Rval_Cmp" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}
static void Rval_Eq_Prime(){
	// cout << "Rval_Eq_Prime" << endl;
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
			Rval_Eq_Prime();
			break;
		case NE:
			Match(NE, "NE");
			Rval_Cmp();
			Rval_Eq_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND, EQ, NE}");
	}
}
static void Rval_Eq(){
	// cout << "Rval_Eq" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}
static void Rval_And_Prime(){
	// cout << "Rval_And_Prime" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR, AND}");
	}
}
static void Rval_And(){
	// cout << "Rval_And" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}


static void Rval_Or_Prime(){
	// cout << "Rval_Or_Prime" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {COMMA, RPAR, SC, OR}");
	}
}

static void Rval_Or(){
	// cout << "Rval_Or" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS}");
	}
}

static void Expr(){
	// cout << "Expr" << endl;
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

			if (nextToken.type == ASSIGN){
				Match(IDENT, "IDENT");
				Match(ASSIGN, "ASSIGN");
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
	}
}

static void Return_Stmt_Prime(){
	// cout << "Return_Stmt_Prime" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
	}
}

static void Return_Stmt(){
	// cout << "Return_Stmt" << endl;
	switch (CurTok.type)
	{
		case RETURN:
			Match(RETURN, "RETURN");
			Return_Stmt_Prime();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {RETURN}");
	}
}

static void Else_Stmt(){
	// cout << "Else_Stmt" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, ELSE}");
	}
}

static void If_Stmt(){
	// cout << "If_Stmt" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {IF}");
	}
}

static void While_Stmt(){
	// cout << "While_Stmt" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {WHILE}");
	}
}

static void Expr_Stmt(){
	// cout << "Expr_Stmt" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
	}
}

static void Stmt(){
	// cout << "Stmt" << endl;
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
			break;
		case RETURN:	
			Return_Stmt();
			break;
		case IF:
			If_Stmt();
			break;
		case WHILE:
			While_Stmt();
			break;
		case LBRA:
			Block();
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, LBRA}");
	}
}

static void Stmt_List(){
	// cout << "Stmt_List" << endl;
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
			break;
		case RBRA:
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA}");
	}
}

static void Local_Decl(){
	// cout << "Local_Decl" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}
}

static void Local_Decls(){
	// cout << "Local_Decls" << endl;
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
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}
}

// block ::= "{" local_decls stmt_list "}"
static std::unique_ptr<BlockAST> Block(){
	// cout << "Block" << endl;
	std::vector<std::unique_ptr<VariableDeclAST>> variable_decls;
	std::vector<std::unique_ptr<StmtAST>> stmt_list; 
	switch (CurTok.type)
	{
		case LBRA:
			Match(LBRA, "Expected '{' to declare new scope. ");
			variable_decls = Local_Decls();
			stmt_list = Stmt_List();
			Match(RBRA, "Expected '}' after statement. ");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {LBRA}");
	}

	return std::make_unique<BlockAST>(variable_decls, stmt_list);
}

// param ::= var_type IDENT
static std::unique_ptr<ParamAST> Param(){
	// cout << "Param" << endl;
	VAR_TYPE type;
	std::string ident; 

	switch (CurTok.type)
	{
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			type = Var_Type();
			
			TOKEN prev_token = CurTok;
			Match(IDENT, "Expected identifer after paramter type. ");
			ident = prev_token.lexeme;

			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}

	return std::make_unique<ParamAST>(ident, type);
}

static void Param_List_Prime(std::vector<std::unique_ptr<ParamAST>> &param_list){
	// cout << "Param_List_Prime" << endl;
	switch (CurTok.type)
	{
		case COMMA:
			Match(COMMA, "Expected ',' or ')' after function parameter");
			auto param = Param();
			param_list.push_back(param);

			Param_List_Prime(param_list);
			break;
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
			auto param = Param();
			param_list.push_back(param);

			Param_List_Prime(param_list);
			break;
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
			Match(VOID_TOK, "Expected 'void' token in function paramters");
			break;
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			param_list = Param_List();
			break;
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
			Match(BOOL_TOK, "Expected 'bool' keyword.");
			type = BOOL_TYPE;
			break;
		case FLOAT_TOK:
			Match(FLOAT_TOK, "Expected 'float' keyword.");
			type = FLOAT_TYPE;
			break;
		case INT_TOK:
			Match(INT_TOK, "Expected 'int' keyword.");
			type = INT_TYPE;
			break;
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
			Match(VOID_TOK, "Expected 'void' keyword.");
			type = VOID_TYPE; 
			break;
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			type = Var_Type();
			break;
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
static void Decl_Prime(std::unique_ptr<FuncDeclAST> &func_decl, std::unique_ptr<VariableDeclAST> &var_decl, VAR_TYPE type, const std::string &ident){
	// cout << "Decl_Prime" << endl;
	switch (CurTok.type)
	{
		case LPAR:
			Match(LPAR, "Expected '(' after function decleration. ");

			auto params = Params();

			Match(RPAR, "Expected ')' after function paramters. ");

			auto block = Block();
			
			func_decl = std::make_unique<FuncDeclAST>(ident, type, params, block);
			break;
		case SC:
			Match(SC, "Expected ';' after variable decleration. ");

			var_decl = std::make_unique<VariableDeclAST>(ident, type);
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {LPAR, SC}");
	} 
}

// decl ::= var_type IDENT decl_prime | "void" IDENT "(" params ")" block
static std::unique_ptr<DeclAST> Decl() {
	// cout << "Decl" << endl;
	std::unique_ptr<FuncDeclAST> func_decl; 
	std::unique_ptr<VariableDeclAST> var_decl; 
	TOKEN prev_token; 

	switch (CurTok.type)
	{
		case VOID_TOK:
			Match(VOID_TOK, "Expected 'void' token before function decleration. ");

			prev_token = CurTok; 
			Match(IDENT, "Expected funtion identifer after 'void' token. ");
			std::string ident = prev_token.lexeme; 

			Match(LPAR, "Expeceted '(' after function identifer. ");
			
			auto params = Params();
			
			Match(RPAR, "Expected ')' after parameter list. ");
			
			auto block = Block();

			func_decl = std::make_unique<FuncDeclAST>(ident, VOID_TYPE, params, block);
			break;
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			auto type = Var_Type();

			prev_token = CurTok;
			Match(IDENT, "Expected identifer after type token. ");
			std::string ident = prev_token.lexeme; 


			Decl_Prime(func_decl, var_decl, type, ident);
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK, EXTERN}");
	}
	return std::make_unique<DeclAST>(func_decl, var_decl);
};

// decl_list_prime ::= decl decl_list_prime | epsilon
static void Decl_List_Prime(std::unique_ptr<DeclAST> &decl_list){
	// cout << "Decl_List_Prime" << endl;
	switch (CurTok.type)
	{
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			auto decl = Decl();
			decl_list.push_back(std::move(decl));

			Decl_List_Prime(std::move(decl_list));
			break;
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
			auto decl = Decl();
			decl_list.push_back(std::move(decl));

			Decl_List_Prime(std::move(decl_list));
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {VOID_TOK, BOOL_TOK, FLOAT_TOK, INT_TOK}");
	}
	return decl_list;
};


// extern ::= "extern" type_spec IDENT "(" params ")" ";"
static std::unique_ptr<FuncDeclAST> Extern(){
	// cout << "Extern" << endl;
	std::string ident; 
	VAR_TYPE type; 
	std::unique_ptr<ParamAST> params; 
	switch (CurTok.type)
	{
		case EXTERN:
			Match(EXTERN, "EXTERN");

			auto type = Type_Spec();
			
			// Error thrown if 'IDENT' not found 
			// Implies, if Match doesn't throw an error, previous token's lexeme was the identifer (Match gets the next token)
			TOKEN prev_token = CurTok;
			Match(IDENT, "Expected identifier after type keyword.");
			ident = prev_token.lexeme; 


			Match(LPAR, "Expected '(' after identifer keyword.");
			params = Params();
			Match(RPAR, "Expected ')' after function paramters.");

			Match(SC, "Expected ';' after function definition.");
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected: {EXTERN}");
	}

	return std::make_unique<FuncDeclAST>(ident, type, params, nullptr);
}

// extern_list_prime ::= extern extern_list_prime | epsilon
static void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list){

	// cout << "Extern_List_Prime" << endl;
	switch (CurTok.type)
	{
		case EXTERN:
			auto E = Extern();
			extern_list.push_back(std::move(E));

			Extern_List_Prime(std::move(extern_list));
			break;
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
			auto E = Extern();
			extern_list.push_back(std::move(E));

			Extern_List_Prime(extern_list);
			break;
		default:
			throw ParseException("Invalid Token Error: \nExpected {EXTERN}");
	}

	return extern_list;
};

// program ::= extern_list decl_list | decl_list
static std::unique_ptr<ASTnode> Program(){
	std::vector<std::unique_ptr<FuncDeclAST>> extern_list;
	std::vector<std::unique_ptr<DeclAST>> decl_list;

	switch (CurTok.type)
    {
		case VOID_TOK:
		case BOOL_TOK:
		case FLOAT_TOK:
		case INT_TOK:
			decl_list = Decl_List();
			break;
		case EXTERN:
			extern_list = Extern_List();
			decl_list = Decl_List();
			break;
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
	} catch(const exception& e){
		cout << e.what() << endl << "Got: " << CurTok.lexeme << " (Type: " << CurTok.type << ") on-line: " << CurTok.lineNo << endl;
	}
}

static void ExtendVec(std::vector<std::unique_ptr<ASTnode>> &v1, std::vector<std::unique_ptr<ASTnode>> &v2){
	v1.insert(v1.end(),v2.begin(),v2.end());
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
