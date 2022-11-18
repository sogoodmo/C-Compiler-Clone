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
#include "llvm/Support/TargetRegistry.h"
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
#include <unordered_map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>
#include <unordered_set>

using namespace llvm;
using namespace llvm::sys;
using namespace std;

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

/**
 * Contains a vector of maps- Where the index of the unordered_map indicates it's level of scope
 *
 * A block may access variables in a scope prior to it in the array, but not after it.
 *
 * If a variable is defined multiple times
 */
static std::vector<std::unordered_map<std::string, llvm::AllocaInst *>> ScopedNamedValues;
static std::unordered_map<std::string, llvm::GlobalVariable *> GlobalVariables;
static std::unordered_set<std::string> UndefinedVars;

static bool IfPathsReturn = false; 
static bool IfStmtLast = false; 

// Global variable to check if adding a new scope is due to a function, or braces. 
bool isFuncBlock = true; 


/**
 * Class to store warnings, but not crash program
 */
class Warning
{
	std::string Err;
	int lineno;
	int colno;

public:
	Warning(std::string err, int line, int col) : Err(err), lineno(line), colno(col) {}

	std::string to_string()
	{
		return Err + ". Line: " + std::to_string(lineno) + " Col: " + std::to_string(colno);
	}
};

static std::vector<Warning> Warnings;

/**
 * @brief Class for handling semenatic errors
 *
 */
class SemanticException : public exception
{
	string Err;
	int line;
	int col;

public:
	SemanticException(string err, int line, int col) : Err(err), line(line), col(col) {}

	virtual const char *what() const throw()
	{
		static std::string errMessage("\033[0;31mSemantic Error:\033[0m " + Err + " Line: " + std::to_string(line) + " Col: " + std::to_string(col));

		return errMessage.c_str();
	}
};

/**
 * @brief Custom exception class for parse errors
 *
 */
class ParseException : public exception
{
	string Err;

public:
	ParseException(string err) : Err(err) {}

	virtual const char *what() const throw()
	{
		return Err.c_str();
	}
};

template <typename K, typename V>
bool mapContainsKey(std::unordered_map<K, V> &map, K key)
{
	if (map.find(key) == map.end())
		return false;
	return true;
}


FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

#pragma region

// The lexer returns one of these for known things.
enum TOKEN_TYPE
{

	IDENT = -1,		   // [a-zA-Z_][a-zA-Z_0-9]*
	ASSIGN = int('='), // '='

	// delimiters
	LBRA = int('{'),  // left brace
	RBRA = int('}'),  // right brace
	LPAR = int('('),  // left parenthesis
	RPAR = int(')'),  // right parenthesis
	SC = int(';'),	  // semicolon
	COMMA = int(','), // comma

	// types
	INT_TOK = -2,	// "int"
	VOID_TOK = -3,	// "void"
	FLOAT_TOK = -4, // "float"
	BOOL_TOK = -5,	// "bool"

	// keywords
	EXTERN = -6,  // "extern"
	IF = -7,	  // "if"
	ELSE = -8,	  // "else"
	WHILE = -9,	  // "while"
	RETURN = -10, // "return"
	// TRUE   = -12,     // "true"
	// FALSE   = -13,     // "false"

	// literals
	INT_LIT = -14,	 // [0-9]+
	FLOAT_LIT = -15, // [0-9]+.[0-9]+
	BOOL_LIT = -16,	 // "true" or "false" key words

	// logical operators
	AND = -17, // "&&"
	OR = -18,  // "||"

	// operators
	PLUS = int('+'),	// addition or unary plus
	MINUS = int('-'),	// substraction or unary negative
	ASTERIX = int('*'), // multiplication
	DIV = int('/'),		// division
	MOD = int('%'),		// modular
	NOT = int('!'),		// unary negation

	// comparison operators
	EQ = -19,	   // equal
	NE = -20,	   // not equal
	LE = -21,	   // less than or equal to
	LT = int('<'), // less than
	GE = -23,	   // greater than or equal to
	GT = int('>'), // greater than

	// special tokens
	EOF_TOK = 0, // signal end of file

	// invalid
	INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN
{
	int type = -100;
	std::string lexeme;
	int lineNo;
	int columnNo;
};

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;				  // Filled in if INT_LIT
static bool BoolVal;			  // Filled in if BOOL_LIT
static float FloatVal;			  // Filled in if FLOAT_LIT
static std::string StringVal;	  // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type)
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
static TOKEN gettok()
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

#pragma endregion

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken()
{

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
class ASTnode
{
public:
	virtual ~ASTnode() {}
	virtual llvm::Value *codegen() = 0;
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const {};
};

enum VAR_TYPE
{
	VOID_TYPE = 0,
	INT_TYPE,
	FLOAT_TYPE,
	BOOL_TYPE
};

/**
 * @brief Get constant llvm value from a type 
 * 
 * @param type The type of the constant
 * @param signed If the constant is signed or not 
 * @return llvm::Value* 
 */
llvm::Value *GetConstant(VAR_TYPE type, bool isSigned=false)
{
	switch (type)
	{
		case INT_TYPE:
			return ConstantInt::get(TheContext, APInt(32, 0, isSigned));
		case FLOAT_TYPE:
			return ConstantFP::get(TheContext, APFloat(0.0f));
		case BOOL_TYPE:
			return ConstantInt::get(TheContext, APInt(1, 0, isSigned));
		default:
			throw SemanticException("",-1,-1);
	}
}

/**
 * @brief Returns the matching llvm type for the type passed in.
 *
 * @param type The type of the variable
 * @return const llvm::Type The LLVM Type of the variable passed
 */
llvm::Type *TypeToLLVM(VAR_TYPE type, TOKEN tokInfo)
{

	switch (type)
	{
	case VOID_TYPE:
		return Type::getVoidTy(TheContext);
	case INT_TYPE:
		return IntegerType::getInt32Ty(TheContext);
	case FLOAT_TYPE:
		return Type::getFloatTy(TheContext);
	case BOOL_TYPE:
		return IntegerType::getInt1Ty(TheContext);
	default:
		throw SemanticException("Unexpected Type. \nExpected one of ['int', 'void', 'bool', 'float']", tokInfo.lineNo, tokInfo.columnNo);
	}
}

/**
 * @brief Get the Highest Precision Type object
 *
 * @param t1 First Type
 * @param t2 Second Type
 * @return llvm::Type* Returns the type with the higher precision of the 2
 */
llvm::Type *GetHighestPrecisionType(llvm::Type *t1, llvm::Type *t2)
{
	if (t1->isFloatTy() || t2->isFloatTy())
	{
		return Type::getFloatTy(TheContext);
	}
	else if (t1->isIntegerTy(32) || t2->isIntegerTy(32))
	{
		return IntegerType::getInt32Ty(TheContext);
	}
	else if (t2->isIntegerTy(1) || t2->isIntegerTy(1))
	{
		return IntegerType::getInt1Ty(TheContext);
	}

	throw SemanticException("Unexpected Type.", -1, -1);
}

/**
 * @brief Converts type enum to string
 *
 * @param type The type to convert
 * @return const std::string The string of the converted type
 */
const std::string TypeToStr(VAR_TYPE type)
{
	switch (type)
	{
	case VOID_TYPE:
		return "void";
	case INT_TYPE:
		return "int";
	case FLOAT_TYPE:
		return "float";
	case BOOL_TYPE:
		return "bool";
	default:
		return "";
	}
}

/**
 * @brief Converts the LLVM Type into human readable string. 
 * 
 * @param type 
 * @return const std::string 
 */
const std::string llvmTypeToStr(llvm::Type *type){
	if (type->isFloatTy())
	{
		return "float";
	} 
	else if (type->isIntegerTy(1))
	{
		return "bool";
	}
	else if (type->isIntegerTy(32))
	{
		return "int";
	}
	else if (type->isVoidTy())
	{
		return "void";
	}
	return "";
}

// Create an alloca instruction in the entry block of the function.
// This is used for mutable variables etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, llvm::Type *type)
{
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(type, 0, VarName.c_str());
}

/**
 * @brief Convert the expression from it's original type to a boolean type
 *
 * @param val Value of the expressiom
 * @param type Type of the value of the expression
 * @return llvm::Value*
 */
static llvm::Value *GetBool(llvm::Value *val, llvm::Type *type, std::string loopStr)
{
	if (type->isFloatTy())
	{
		return Builder.CreateFCmpONE(val, ConstantFP::get(TheContext, APFloat(0.0f)), loopStr);
	}
	else if (type->isIntegerTy(32))
	{
		return Builder.CreateICmpNE(val, ConstantInt::get(Type::getInt32Ty(TheContext), true), loopStr);
	}
	else if (type->isIntegerTy(1))
	{
		return Builder.CreateLogicalOr(val, ConstantInt::get(Type::getInt1Ty(TheContext), 0), loopStr);
	}

	throw SemanticException("", -1, -1);
}

/**
 * @brief Add a warning to the buffer if a return has been written and there are lines following it in the same block.
 * 
 */
void addReturnWarning(int line, int col){
	Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Return statement will cause it's following lines to not be executed", line, col));
}

/**
 * @brief 
 * Checks if the variable has already been defined- if it has a warning is thrown.
 * 
 * Then it returns the stored object
 * 
 * @tparam T GlobalAlloca or Normal Alloca
 * @param alloca The alloca we want to load  
 * @param llvmType The type of the variable 
 * @param ident The name of the variable  
 * @return llvm::Value* 
 */
template <typename T>
llvm::Value *CheckDefinedAndLoad(T alloca, llvm::Type *llvmType, TOKEN Ident)
{
	if (UndefinedVars.count(Ident.lexeme) != 0)
	{
		UndefinedVars.erase(Ident.lexeme);
		Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Referencing undefined variable. Using default value", Ident.lineNo, Ident.columnNo));
	}

	return Builder.CreateLoad(llvmType, alloca, Ident.lexeme.c_str());
}

/**
 * @brief Adds warning to warning buffer if attempting to implicitly convert and may lose precision
 * Returns new value after conversion.
 *
 * @param val Value
 * @param alloca Alloca to update
 */
static llvm::Value *ImplicitCasting(llvm::Value *val, llvm::Type *newType, TOKEN tokInfo, std::string optionalError="")
{
	llvm::Type *oldType = val->getType();

	/**
	 * If casting same types, no extra work needs to be done. Can just return given value
	 */
	if (oldType == newType)
	{
		return val;
	}

	// Float -> Int Conversion - Precision Loss
	if (oldType->isFloatTy() && newType->isIntegerTy(32))
	{
		Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Implict conversion from Float to Int32. May lose precision" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateFPToSI(val, newType, tokInfo.lexeme.c_str());
	}

	// Float -> Bool Conversion - Precision Loss
	if (oldType->isFloatTy() && newType->isIntegerTy(1))
	{
		Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Implict conversion from Bool to Float. May lose precision" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateFCmpONE(val, ConstantFP::get(TheContext, APFloat(0.0f)), tokInfo.lexeme.c_str());
	}

	// Int -> Bool Conversion - Precision Loss
	if (oldType->isIntegerTy(32) && newType->isIntegerTy(1))
	{
		Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Implict conversion from Int32 to Bool. May lose precision" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateICmpNE(val, ConstantInt::get(Type::getInt32Ty(TheContext), 0), tokInfo.lexeme.c_str());
	}

	// Int -> Float Conversion - No Precision Loss
	if (oldType->isIntegerTy(32) && newType->isFloatTy())
	{
		// Warnings.push_back(Warning("Warning: Implict conversion from Int32 to Float. May result in unexpected behaivour" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateSIToFP(val, newType, tokInfo.lexeme.c_str());
	}

	// Bool -> Float Conversion - No Precision Loss
	if (oldType->isIntegerTy(1) && newType->isFloatTy())
	{
		// Warnings.push_back(Warning("Warning: Implict conversion from Bool to Float. May result in unexpected behaivour" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateUIToFP(val, newType, tokInfo.lexeme.c_str());
	}

	// Bool -> Int Conversion - No Precision Loss
	if (oldType->isIntegerTy(1) && newType->isIntegerTy(32))
	{

		// Warnings.push_back(Warning("Warning: Implict conversion from Bool to Int32. May result in unexpected behaivour" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateIntCast(val, newType, false, tokInfo.lexeme.c_str());
	}

	throw SemanticException("Unexpected Type. Expected int, bool or float.", tokInfo.lineNo, tokInfo.columnNo);
}

// /**
//  * @brief Generic function to print a vector of ast nodes
//  *
//  *
//  *
//  * @tparam T Generic type of ASTnode
//  * @param NodeVec The vector of nodes to print out
//  * @param prefix The prefix of the vector
//  * @param isLeft Weather the vector was a left subtree
//  * @param extraCon Any auxillary conditions to checking if an item is the right subtree
//  */
// template <typename T>
// static void PrintVectorAST(std::vector<std::unique_ptr<T>> NodeVec, const std::string &prefix, bool isLeft, bool extraCon = false)
// {
// 	for (int i = 0; i < NodeVec.size(); i++)
// 	{
// 		NodeVec[i]->to_string(prefix + (isLeft ? "│   " : "    "), "", (i != NodeVec.size() - 1) || extraCon);
// 	}
// }

// ---- AST Declerations ---- //
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

class StmtAST : public ASTnode
{
};
class ExprAST : public StmtAST
{
};

#pragma endregion
// ---- AST Declerations ---- //

/// =================================== !! Variable's START !! ================================================ ///
#pragma region
class Variable : public ExprAST
{
	TOKEN Ident;

public:
	Variable(TOKEN Ident)
		: Ident(std::move(Ident)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "Variable: " << Ident.lexeme << std::endl;
	};

	/**
	 * @brief Attempts to find referenced variable in any scope level higher or equal to (Or global scope finally)
	 * If referenced variable not defined already- throw a warning and return a null value.
	 *
	 * @return llvm::Value* Value of referenced variable
	 */
	llvm::Value *codegen() override
	{
		// Look up the values from the scopes, stopping at the deepest layer of scope it finds
		// If doesn't exist in any scope- try one more time in the global scope
		// If it doesn't exist in the scope, then it will throw an error.

		for (int i = ScopedNamedValues.size() - 1; i > -1; i--)
		{
			if (mapContainsKey(ScopedNamedValues[i], Ident.lexeme))
			{
				llvm::AllocaInst *alloca = ScopedNamedValues[i].at(Ident.lexeme);
				return CheckDefinedAndLoad(alloca, alloca->getAllocatedType(), Ident);
			}
		}
		

		if (mapContainsKey(GlobalVariables, Ident.lexeme))
		{
			GlobalVariable *globalAlloca = GlobalVariables.at(Ident.lexeme);
			return CheckDefinedAndLoad(globalAlloca, globalAlloca->getValueType(), Ident);
		}

		throw SemanticException("Undecleared variable referenced. Perhaps your variable is out of scope?", Ident.lineNo, Ident.columnNo);
	};
};

class VariableDeclAST : public ASTnode
{
	VAR_TYPE Type;
	TOKEN Ident;

public:
	VariableDeclAST(TOKEN Ident, VAR_TYPE Type)
		: Ident(std::move(Ident)), Type(std::move(Type)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << nodeStr << TypeToStr(Type) << " " << Ident.lexeme << std::endl;
	};

	/**
	 * @brief Adds a null decleration of the variable to the stack
	 * However, throws an error if the program is redeclaring a global variable in the global scope
	 * It allows the program to redeclare a global variable within a local scope.
	 *
	 */
	llvm::Value *codegen() override
	{
		bool isGlobal = ScopedNamedValues.size() == 0;
		llvm::Type *llvmType = TypeToLLVM(Type, Ident);
		

		if (isGlobal)
		{
			if (mapContainsKey(GlobalVariables, Ident.lexeme))
			{
				throw SemanticException("Cannot redeclare global variable.", Ident.lineNo, Ident.columnNo);
			}

			auto G = new GlobalVariable(*(TheModule.get()), llvmType, false, GlobalValue::CommonLinkage, Constant::getNullValue(llvmType), Ident.lexeme);

			UndefinedVars.insert(Ident.lexeme);
			GlobalVariables.insert({Ident.lexeme, G});
		}
		else
		{
			if (mapContainsKey(ScopedNamedValues.back(), Ident.lexeme))
			{
				throw SemanticException("Cannot redeclare variable within same scope.", Ident.lineNo, Ident.columnNo);
			}	

			Function *TheFunction = Builder.GetInsertBlock()->getParent();

			AllocaInst *alloca = CreateEntryBlockAlloca(TheFunction, Ident.lexeme, llvmType); 
			Builder.CreateStore(Constant::getNullValue(llvmType), alloca);

			UndefinedVars.insert(Ident.lexeme);
			ScopedNamedValues.back().insert({Ident.lexeme, alloca});
		}

		return nullptr;
	};
};

class VariableAssignmentAST : public ExprAST
{
	TOKEN Ident;
	std::unique_ptr<ExprAST> Expr;

public:
	VariableAssignmentAST(TOKEN Ident, std::unique_ptr<ExprAST> Expr)
		: Ident(std::move(Ident)), Expr(std::move(Expr)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "AssignExpr: " << Ident.lexeme << " =" << std::endl;

		Expr->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
	};

	/**
	 * @brief Generates the value of the expression of our variable assignment
	 * Attempts to update the variable (Given it's defined), aswell as throwing warnings for any loss of precision from implicit type casting
	 *
	 * If it succesfully updates the value of the variable in it's current scope,
	 * It must go through all of it's parents scope (and global scope) and update the value there if it exists
	 *
	 * Finally we must store the fact that the variable is no longer undefined.
	 *
	 * @return llvm::Value* The value of the variable as it may be used as `x=y=z` and needs to propogated
	 */
	llvm::Value *codegen() override
	{
		llvm::Value *E = Expr->codegen();
		llvm::Value *CastedValue;

		for (int i = ScopedNamedValues.size() - 1; i > -1; i--)
		{
			if (mapContainsKey(ScopedNamedValues[i], Ident.lexeme))
			{

				llvm::AllocaInst *alloca = ScopedNamedValues[i].at(Ident.lexeme);

				CastedValue = ImplicitCasting(E, alloca->getAllocatedType(), Ident);

				Builder.CreateStore(CastedValue, alloca);

				ScopedNamedValues[i][Ident.lexeme] = alloca;
				UndefinedVars.erase(Ident.lexeme);

				return CastedValue;
			}
		}

		/**
		 * If we're assining the value of a global variable
		 *
		 * It has no higher scope, so we don't need to propagate this change through higher scopes
		 */
		if (mapContainsKey(GlobalVariables, Ident.lexeme))
		{

			GlobalVariable *globalAlloca = GlobalVariables.at(Ident.lexeme);

			CastedValue = ImplicitCasting(E, globalAlloca->getValueType(), Ident);

			Builder.CreateStore(CastedValue, globalAlloca);
			GlobalVariables[Ident.lexeme] = globalAlloca;

			UndefinedVars.erase(Ident.lexeme);
			return CastedValue;
		}

		throw SemanticException("Undecleared variable referenced. Perhaps your variable is out of scope?", Ident.lineNo, Ident.columnNo);
	};
};
#pragma endregion
/// =================================== !! Variable's END !! ================================================ ///

/// =================================== !! Block & Statements Start !! ================================================ ///
class ReturnAST : public StmtAST
{
	std::unique_ptr<ExprAST> ReturnExpr;
	TOKEN returnTok; 

public:
	ReturnAST(std::unique_ptr<ExprAST> ReturnExpr, TOKEN returnTok)
		: ReturnExpr(std::move(ReturnExpr)), returnTok(std::move(returnTok)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "Return" << std::endl;

		if (ReturnExpr != nullptr)
		{
			ReturnExpr->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
		}
	};

	/**
	 * @brief Create a return llvm object if it exists in the code. 
	 * 
	 * Must check if the return type is equal to the type of the function.
	 * 
	 * @return llvm::Value* 
	 */
	llvm::Value *codegen() override
	{	
		
		llvm::Type *FuncReturnType = Builder.getCurrentFunctionReturnType();

		if (ReturnExpr != nullptr)
		{
			llvm::Value *RetValue = ReturnExpr->codegen(); 
			
			if (RetValue->getType() != FuncReturnType)
			{
				throw SemanticException("Incorrect Function Return Type. Expected: " + llvmTypeToStr(FuncReturnType) + " Got: " + llvmTypeToStr(RetValue->getType()), returnTok.lineNo, returnTok.columnNo);
			}

			Builder.CreateRet(RetValue);
		}
		else
		{
			if (!(FuncReturnType->isVoidTy()))
			{
				throw SemanticException("Incorrect Function Return Type. Expected: " + llvmTypeToStr(FuncReturnType) + " Got: void", returnTok.lineNo, returnTok.columnNo);
			}
			Builder.CreateRetVoid();
		}


		return GetConstant(BOOL_TYPE);
	};

	/**
	 * @brief Get the token of the return object.
	 * 
	 * @return TOKEN 
	 */
	TOKEN getRetTok()
	{
		return returnTok;
	}
};

class BlockAST : public StmtAST
{
	std::vector<std::unique_ptr<VariableDeclAST>> VarDecls;
	std::vector<std::unique_ptr<StmtAST>> StmtList;

public:
	BlockAST(std::vector<std::unique_ptr<VariableDeclAST>> VarDecls, std::vector<std::unique_ptr<StmtAST>> StmtList)
		: VarDecls(std::move(VarDecls)), StmtList(std::move(StmtList)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "Block" << std::endl;

		for (int i = 0; i < VarDecls.size(); i++)
		{
			VarDecls[i]->to_string(prefix + (isLeft ? "│   " : "    "), "LocalVarDecl: ", (i != VarDecls.size() - 1) || (StmtList.size() != 0));
		}

		for (int i = 0; i < StmtList.size(); i++)
		{
			StmtList[i]->to_string(prefix + (isLeft ? "│   " : "    "), "", (i != StmtList.size() - 1));
		}
	};

	/**
	 * @brief When we enter a new scope, we must create a new unordered_map for strings to allocas
	 * Then once this scope is exited, we destroy any variables that are now out of scope
	 * 
	 * We must also check if we havn't created any local scopes yet- as we may redeclare function arguments within the body. This isn't allowed by c99 standards 
	 *
	 */
	llvm::Value *codegen() override
	{
		bool addScope = !isFuncBlock;
		llvm::Value *containsReturn;

		// If this block is called from a function defintion
		// We must NOT add a layer of scope (As it has already been add for arguments)
		// We must also flip the flag, to allow any subsequent calls to BlockCodeGen create a layer of scope
		if (addScope)
		{
			ScopedNamedValues.push_back(std::unordered_map<std::string, llvm::AllocaInst *>());
		}
		else
		{
			isFuncBlock = false;
		}
		
		for (auto &varDecl : VarDecls)
		{
			varDecl->codegen();
		}

		int stmtListIdx = -1;
		for (auto &stmt : StmtList)
		{	
			stmtListIdx++;
			std::cout << "here " << std::endl;

			// JUST NEED TO GET THIS FIXED 
			// CAN BE FIXED SPLITTING UP FILES 
			// 
			// Used to keep track if an ifstmt was the last stmt in the block 
			// In case the IF is guranteed to return a value & there are lines of code proceeding it (In which case a warning should be thrown)			
			// IfStmtLast = dynamic_cast<IfAST*>(stmt.get()) != nullptr;


			/**
			 * If the current statement is a return statement, throw a warning only if it's not the last statement in a block
			 * 
			 * Generate the statement and stop generating any more IR- as this code would never be reached
			 */
			auto returnStmt = dynamic_cast<ReturnAST*>(stmt.get());
			if (returnStmt != nullptr){

				if (stmtListIdx != StmtList.size() - 1){
					addReturnWarning(returnStmt->getRetTok().lineNo, returnStmt->getRetTok().columnNo);
				}

				stmt->codegen();

				if (addScope){
					ScopedNamedValues.pop_back();
				}

				// Just returning any non-null value if we're at a return stmt 
				return ConstantInt::get(TheContext, APInt(1, 0, false));
			}
			else 
			{
				containsReturn = stmt->codegen();
			}

		}

		if (addScope){
			ScopedNamedValues.pop_back();
		}

		return containsReturn;
	};
};

#pragma region
class IfAST : public StmtAST
{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<BlockAST> TrueBlock;
	std::unique_ptr<BlockAST> ElseBlock;

public:
	IfAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<BlockAST> TrueBlock, std::unique_ptr<BlockAST> ElseBlock)
		: ConditionExpr(std::move(ConditionExpr)), TrueBlock(std::move(TrueBlock)), ElseBlock(std::move(ElseBlock)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "IfStmt" << std::endl;

		ConditionExpr->to_string(prefix + (isLeft ? "│   " : "    "), "", true);

		if (TrueBlock != nullptr)
		{
			TrueBlock->to_string(prefix + (isLeft ? "│   " : "    "), "", (ElseBlock != nullptr));
		}

		if (ElseBlock != nullptr)
		{
			ElseBlock->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
		}
	};

	/**
	 * @brief Genereate the expression and coerce it into a boolean
	 * Generate all the codeblocks for each path in the if statment,
	 *
	 * Set appropriate insert points and scopes for each basic block of the if statement
	 *
	 */
	llvm::Value *codegen() override
	{
		bool trueBlockReturn = false;
		bool elseBlockReturn = false;

		Function *TheFunction = Builder.GetInsertBlock()->getParent();
		llvm::Value *CondValue = ConditionExpr->codegen();
		llvm::Type *CondType = CondValue->getType();

		CondValue = GetBool(CondValue, CondType, "ifcond");

		BasicBlock *TrueBB = BasicBlock::Create(TheContext, "then", TheFunction);
		BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
		BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

		if (ElseBlock != nullptr)
		{
			Builder.CreateCondBr(CondValue, TrueBB, ElseBB);
		}
		else
		{
			Builder.CreateCondBr(CondValue, TrueBB, MergeBB);
		}

		// ----- True ------ //
		ScopedNamedValues.push_back(std::unordered_map<std::string, llvm::AllocaInst *>());

		Builder.SetInsertPoint(TrueBB);

		trueBlockReturn = TrueBlock->codegen() != nullptr;

		Builder.CreateBr(MergeBB);
		TrueBB = Builder.GetInsertBlock();

		ScopedNamedValues.pop_back();
		// ----- True ------ //

		if (ElseBlock != nullptr)
		{
			// ----- False ------ //
			ScopedNamedValues.push_back(std::unordered_map<std::string, llvm::AllocaInst *>());

			TheFunction->getBasicBlockList().push_back(ElseBB);
			Builder.SetInsertPoint(ElseBB);

			elseBlockReturn = ElseBlock->codegen() != nullptr;

			Builder.CreateBr(MergeBB);
			ElseBB = Builder.GetInsertBlock();

			ScopedNamedValues.pop_back();
			// ----- False ------ //
		}

		// ----- Continue ------ //
		TheFunction->getBasicBlockList().push_back(MergeBB);
		Builder.SetInsertPoint(MergeBB);
		// ----- Continue ------ //

		// If an if statement gurantees a return statement 
		// Keep track of this 
		if (trueBlockReturn && ElseBlock != nullptr && elseBlockReturn)
		{
			IfPathsReturn = true; 
		}

		return nullptr;
	};
};

class WhileAST : public StmtAST
{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<StmtAST> LoopBlock;

public:
	WhileAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<StmtAST> LoopBlock)
		: ConditionExpr(std::move(ConditionExpr)), LoopBlock(std::move(LoopBlock)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "WhileLoop" << std::endl;
		ConditionExpr->to_string(prefix + (isLeft ? "│   " : "    "), "", (LoopBlock != nullptr));

		if (LoopBlock != nullptr)
		{
			LoopBlock->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
		}
	};

	/**
	 * @brief Generate 3 branches. The loop header, loop block and loop continue 
	 * 
	 * Then create a conditional branch depending on the loop condition (Which is evalualted in the loop header)
	 * Within the loop body, unconditionally branch to the loop header, this will re-evalulate the condition and correctly exeucte the loop 
	 * 
	 * Then continue the remaining code 
	 * 
	 * @return llvm::Value* 
	 */
	llvm::Value *codegen() override
	{
		llvm::Value *CondValue;
		llvm::Value *BoolCondValue;

		Function *TheFunction = Builder.GetInsertBlock()->getParent();

		BasicBlock *LoopBB = BasicBlock::Create(TheContext, "while");
		BasicBlock *ContBB = BasicBlock::Create(TheContext, "whilecont");
		BasicBlock *HeaderBB = BasicBlock::Create(TheContext, "loopcond", TheFunction);
		
		// Unconditionally branch to the header and generate condition for the loop
		Builder.CreateBr(HeaderBB);
		Builder.SetInsertPoint(HeaderBB);

		CondValue = ConditionExpr->codegen();
		BoolCondValue = GetBool(CondValue, CondValue->getType(), "whilecond");

		Builder.CreateCondBr(BoolCondValue, LoopBB, ContBB);

		// Generate the code that will be looped 
		TheFunction->getBasicBlockList().push_back(LoopBB);
		Builder.SetInsertPoint(LoopBB);

		ScopedNamedValues.push_back(std::unordered_map<std::string, llvm::AllocaInst *>());
		LoopBlock->codegen();
		ScopedNamedValues.pop_back();

		//Branch back to checking conditional branch 
		Builder.CreateBr(HeaderBB);


		// Exiting loop and continuing code 
		TheFunction->getBasicBlockList().push_back(ContBB);
		Builder.SetInsertPoint(ContBB);
		return nullptr;
	};
};

#pragma endregion
/// =================================== !! Block & Stmts End !! ================================================ ///

/// =================================== !! Binary / Unary AST Start !! ================================================ ///
#pragma region
class BinaryExprAST : public ExprAST
{
	TOKEN Op;
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> RHS;

public:
	BinaryExprAST(TOKEN Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
		: Op(std::move(Op)), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "BinaryExpr: " << Op.lexeme << std::endl;

		LHS->to_string(prefix + (isLeft ? "│   " : "    "), "", true);
		RHS->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
	};

	/**
	 * @brief Generates the LHS, RHS of the binary expression
	 * And depending on the highest level type of the hirearchy coerces the other type.
	 *
	 * Then generates the correct binary expression depending on the type.
	 *
	 * @return llvm::Value*
	 */
	llvm::Value *codegen() override
	{
		llvm::Value *BinExprVal;
		llvm::Type *HighestPrecisionType;
		llvm::Value *CastedLHS;
		llvm::Value *CastedRHS;

		/**
		 * Not generating the implicit cast for AND/OR as that will be done 
		 * Later for the case of lazy evaluation. 
		 */
		switch (Op.type)
		{
		case AND:
		case OR:
			break;
		case EQ:
		case NE:
		case LE:
		case LT:
		case GE:
		case GT:
		case PLUS:
		case MINUS:
		case ASTERIX:
		case DIV:
		case MOD:
		{
			llvm::Value *LHSVal = LHS->codegen();
			llvm::Value *RHSVal = RHS->codegen();

			HighestPrecisionType = GetHighestPrecisionType(LHSVal->getType(), RHSVal->getType());

			CastedLHS = ImplicitCasting(LHSVal, HighestPrecisionType, Op);
			CastedRHS = ImplicitCasting(RHSVal, HighestPrecisionType, Op);
			break;
		}
		default:
			throw SemanticException("Invalid Binary Operator", Op.lineNo, Op.columnNo);
		}

		// Three blocks LHS,RHS, Continue block
		// Create a temporary variable
		// Jump to the LHS and evaluate the expression
		// OR:
		// If LHS is false, set the alloca to false and jump to the RHS (set the alloca to the result of the RHS and then jump to continue)
		// If the LHS is true, set the alloca to true and jump to the continue block
		// so above two with conditional jump

		/**
		 * If we are creating an expression for && or || we can try to apply lazy evaluation 
		 * 
		 * We can create basic blocks, and branch to either evaluating the RHS or just continuing the block. 
		 * Depending on the outcome of the LHS. 
		 * 
		 * We just need to store the value in the temporary alloca 
		 *
		 */
		if (Op.type == OR || Op.type == AND)
		{
			Function *TheFunction = Builder.GetInsertBlock()->getParent();

			AllocaInst *tmpAlloca = CreateEntryBlockAlloca(TheFunction, "tmpLazy", TypeToLLVM(BOOL_TYPE, Op));

			BasicBlock *RightBB = BasicBlock::Create(TheContext, "RExpr", TheFunction);
			BasicBlock *SkipRightBB = BasicBlock::Create(TheContext, "SkipRExpr");
			BasicBlock *ContBB = BasicBlock::Create(TheContext, "Cont");


			llvm::Value *BoolRHS;
			llvm::Value *BoolLHS = ImplicitCasting(LHS->codegen(), TypeToLLVM(BOOL_TYPE, Op), Op);

			/**
			 * If we have (True || ...) -> We don't have to evaluate RHS. So we can branch to the continue block
			 * If we have (False && ...) -> We don't have to evalaute RHS. So we can branch to the continue block÷
			 */
			if (Op.type == OR)
			{
				Builder.CreateCondBr(BoolLHS, SkipRightBB, RightBB);

				// Eval RHS- Set tmp to RHS Value 
				TheFunction->getBasicBlockList().push_back(RightBB);
				Builder.SetInsertPoint(RightBB);

				BoolRHS = ImplicitCasting(RHS->codegen(), TypeToLLVM(BOOL_TYPE, Op), Op);
				Builder.CreateStore(BoolRHS, tmpAlloca);

				Builder.CreateBr(ContBB);

				// Skip Eval RHS- Set tmp to True 
				TheFunction->getBasicBlockList().push_back(SkipRightBB);
				Builder.SetInsertPoint(SkipRightBB);

				Builder.CreateStore(ConstantInt::get(TheContext, APInt(1, 1, false)), tmpAlloca);
	
				Builder.CreateBr(ContBB);

				TheFunction->getBasicBlockList().push_back(ContBB);
				Builder.SetInsertPoint(ContBB);

				return Builder.CreateLoad(TypeToLLVM(BOOL_TYPE, Op), tmpAlloca, "exprBool");
			}
			else if (Op.type == AND)
			{
				Builder.CreateCondBr(BoolLHS, RightBB, SkipRightBB);

				// Eval RHS- Set tmp to RHS Value 
				TheFunction->getBasicBlockList().push_back(RightBB);
				Builder.SetInsertPoint(RightBB);

				BoolRHS = ImplicitCasting(RHS->codegen(), TypeToLLVM(BOOL_TYPE, Op), Op);
				Builder.CreateStore(BoolRHS, tmpAlloca);

				Builder.CreateBr(ContBB);

				// Skip Eval RHS- Set tmp to False 
				TheFunction->getBasicBlockList().push_back(SkipRightBB);
				Builder.SetInsertPoint(SkipRightBB);

				Builder.CreateStore(ConstantInt::get(TheContext, APInt(1, 0, false)), tmpAlloca);

				Builder.CreateBr(ContBB);

				TheFunction->getBasicBlockList().push_back(ContBB);
				Builder.SetInsertPoint(ContBB);

				return Builder.CreateLoad(TypeToLLVM(BOOL_TYPE, Op), tmpAlloca, "exprBool");
			}
		}

		switch (Op.type)
		{
		case PLUS:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFAdd(CastedLHS, CastedRHS, "faddtmp");
			}
			else
			{
				BinExprVal = Builder.CreateAdd(CastedLHS, CastedRHS, "addtmp");
			}

			break;
		case MINUS:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFSub(CastedLHS, CastedRHS, "fsubtmp");
			}
			else
			{
				BinExprVal = Builder.CreateSub(CastedLHS, CastedRHS, "subtmp");
			}
			
			break;
		case ASTERIX:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFMul(CastedLHS, CastedRHS, "fmultmp");
			}
			else
			{
				BinExprVal = Builder.CreateMul(CastedLHS, CastedRHS, "multmp");
			}

			break;
		case DIV:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFDiv(CastedLHS, CastedRHS, "fdivtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(1))
			{
				BinExprVal = Builder.CreateUDiv(CastedLHS, CastedRHS, "udivtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(32))
			{
				BinExprVal = Builder.CreateSDiv(CastedLHS, CastedRHS, "sdivtmp");
			}

			break;
		case MOD:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFRem(CastedLHS, CastedRHS, "fmodtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(1))
			{
				BinExprVal = Builder.CreateURem(CastedLHS, CastedRHS, "umodtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(32))
			{
				BinExprVal = Builder.CreateSRem(CastedLHS, CastedRHS, "smodtmp");
			}

			break;
		case EQ:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFCmpOEQ(CastedLHS, CastedRHS, "feqtmp");
			}
			else
			{
				BinExprVal = Builder.CreateICmpEQ(CastedLHS, CastedRHS, "ieqtmp");
			}

			break;
		case NE:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFCmpONE(CastedLHS, CastedRHS, "fneqtmp");
			}
			else
			{
				BinExprVal = Builder.CreateICmpNE(CastedLHS, CastedRHS, "ineqtmp");
			}

			break;
		case LE:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFCmpOLE(CastedLHS, CastedRHS, "flteqtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(1))
			{
				BinExprVal = Builder.CreateICmpULE(CastedLHS, CastedRHS, "blteqtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(32))
			{
				BinExprVal = Builder.CreateICmpSLE(CastedLHS, CastedRHS, "ilteqtmp");
			}

			break;
		case LT:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFCmpOLT(CastedLHS, CastedRHS, "flttmp");
			}
			else if (HighestPrecisionType->isIntegerTy(1))
			{
				BinExprVal = Builder.CreateICmpULT(CastedLHS, CastedRHS, "blttmp");
			}
			else if (HighestPrecisionType->isIntegerTy(32))
			{
				BinExprVal = Builder.CreateICmpSLT(CastedLHS, CastedRHS, "ilttmp");
			}

			break;
		case GE:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFCmpOGE(CastedLHS, CastedRHS, "fgteqtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(1))
			{
				BinExprVal = Builder.CreateICmpUGE(CastedLHS, CastedRHS, "bgteqtmp");
			}
			else if (HighestPrecisionType->isIntegerTy(32))
			{
				BinExprVal = Builder.CreateICmpSGE(CastedLHS, CastedRHS, "igteqtmp");
			}

			break;
		case GT:
			if (HighestPrecisionType->isFloatTy())
			{
				BinExprVal = Builder.CreateFCmpOGT(CastedLHS, CastedRHS, "fgttmp");
			}
			else if (HighestPrecisionType->isIntegerTy(1))
			{
				BinExprVal = Builder.CreateICmpUGT(CastedLHS, CastedRHS, "bgttmp");
			}
			else if (HighestPrecisionType->isIntegerTy(32))
			{
				BinExprVal = Builder.CreateICmpSGT(CastedLHS, CastedRHS, "Igttmp");
			}

			break;
		default:
			throw SemanticException("Invalid Binary Operator", Op.lineNo, Op.columnNo);
		}

		return BinExprVal;
	};
};

class UnaryExprAST : public ExprAST
{
	TOKEN Op;
	std::unique_ptr<ExprAST> Expr;

public:
	UnaryExprAST(TOKEN Op, std::unique_ptr<ExprAST> Expr)
		: Op(std::move(Op)), Expr(std::move(Expr)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "UnaryExpr: " << Op.lexeme << std::endl;

		Expr->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
	};

	/**
	 * @brief Generates the Value of the given unary expression. Performs appropriate type casting
	 *
	 * @return llvm::Value*
	 */
	llvm::Value *codegen() override
	{
		llvm::Value *E = Expr->codegen();
		llvm::Value *UnaryExpr;
		llvm::Value *CastedValue = E;
		switch (Op.type)
		{
		case MINUS:
			// Only need to cast a -foo if `foo` is a boolean.
			// Since -int and -float are semantically correct
			if (E->getType()->isIntegerTy(1))
			{
				CastedValue = ImplicitCasting(E, TypeToLLVM(BOOL_TYPE, Op), Op);
				UnaryExpr = Builder.CreateNeg(CastedValue, "bminustmp");
			}
			else if (E->getType()->isIntegerTy(32))
			{
				UnaryExpr = Builder.CreateNeg(E, "fminustmp");
			}
			else if (E->getType()->isFloatTy())
			{
				UnaryExpr = Builder.CreateFNeg(E, "iminustmp");
			}

			break;
		case NOT:
			// Must cast both an int and a float to bool before we apply negation to it
			if (!E->getType()->isIntegerTy(1))
			{
				CastedValue = ImplicitCasting(E, TypeToLLVM(BOOL_TYPE, Op), Op);
				UnaryExpr = Builder.CreateNot(CastedValue, "finottmp");
			}
			else
			{
				UnaryExpr = Builder.CreateNot(E, "nottmp");
			}

			break;
		default:
			throw SemanticException("Unexepected unary operator", Op.lineNo, Op.columnNo);
		}

		return UnaryExpr;
	};
};
#pragma endregion
/// =================================== !! Binary / Unary AST End !! ================================================ ///

/// =================================== !! Functions Start !! ================================================ ///
#pragma region
class ParamAST : public ASTnode
{
	VAR_TYPE Type;
	TOKEN Ident;

public:
	ParamAST(TOKEN Ident, VAR_TYPE Type)
		: Type(std::move(Type)), Ident(std::move(Ident)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << nodeStr << TypeToStr(Type) << " " << Ident.lexeme << std::endl;
	};

	llvm::Value *codegen() override
	{
		return nullptr;
	};

	VAR_TYPE getType()
	{
		return Type;
	}
	TOKEN getIdent()
	{
		return Ident;
	}
};

class FuncCallAST : public ExprAST
{
	TOKEN FuncName;
	std::vector<std::unique_ptr<ExprAST>> Args;

public:
	FuncCallAST(TOKEN FuncName, std::vector<std::unique_ptr<ExprAST>> Args)
		: FuncName(std::move(FuncName)), Args(std::move(Args)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "FuncCall: " << FuncName.lexeme << std::endl;

		for (int i = 0; i < Args.size(); i++)
		{
			Args[i]->to_string(prefix + (isLeft ? "│   " : "    "), "", (i != Args.size() - 1));
		}
	}

	/**
	 * @brief Ensures the function call is valid and generates the values
	 *
	 * @return llvm::Value* The value generated from the function call
	 */
	llvm::Value *codegen() override
	{
		Function *CalleeFunc = TheModule->getFunction(FuncName.lexeme);

		if (!CalleeFunc)
		{
			throw SemanticException("Unknown Function Referenced. Perhaps you have misspelt your function.", FuncName.lineNo, FuncName.columnNo);
		}

		if (Args.size() != CalleeFunc->arg_size())
		{
			throw SemanticException("Invalid Argument Size. Expected " + std::to_string(CalleeFunc->arg_size()) + ". Got " + std::to_string(Args.size()), FuncName.lineNo, FuncName.columnNo);
		}

		std::vector<Value *> ArgsV;
		llvm::Type *funcArgType; 

		int idx = 0; 
		for (auto &Arg : Args)
		{
			llvm::Value *argExpr = Arg->codegen();
			funcArgType = CalleeFunc->getArg(idx++)->getType();

			argExpr = ImplicitCasting(argExpr, funcArgType, FuncName, " in Function Call (" + FuncName.lexeme + ")");

			ArgsV.push_back(argExpr);
		}

		return Builder.CreateCall(CalleeFunc, ArgsV, "calltmp");
	};
};

class FuncDeclAST : public ASTnode
{
	VAR_TYPE Type;
	TOKEN Ident;
	std::vector<std::unique_ptr<ParamAST>> Params;

	// May be null - in the case of extern
	std::unique_ptr<BlockAST> FuncBlock;

public:
	FuncDeclAST(TOKEN Ident, VAR_TYPE Type, std::vector<std::unique_ptr<ParamAST>> Params, std::unique_ptr<BlockAST> FuncBlock)
		: Ident(std::move(Ident)), Type(std::move(Type)), Params(std::move(Params)), FuncBlock(std::move(FuncBlock)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << nodeStr << TypeToStr(Type) << " " << Ident.lexeme << std::endl;

		for (int i = 0; i < Params.size(); i++)
		{
			Params[i]->to_string(prefix + (isLeft ? "│  " : "    "), "Param: ", (i != Params.size() - 1) || (FuncBlock != nullptr));
		}
		if (FuncBlock != nullptr)
		{
			FuncBlock->to_string(prefix + (isLeft ? "│  " : "    "), "Block", false);
		}
	};

	/**
	 * @brief Generate prototype if function doesn't already exist, and add a new-level of scope.
	 *
	 * @return llvm::Function* The function prototype and body generated
	 */
	llvm::Function *codegen() override
	{	
		llvm::FunctionType *FT;
		llvm::Function *FuncDef;
		std::vector<llvm::Type *> Args;
		bool FuncContainsReturn = false; 
		IfStmtLast = false;

		Function *ExternFuncDef = TheModule->getFunction(Ident.lexeme);
		
		/**
		 * Generating IR for function Prototype if it doesn't already exist
		 */
		if (!ExternFuncDef)
		{
			for (auto &Param : Params)
			{
				llvm::Type *type = TypeToLLVM(Param->getType(), Ident);
				Args.push_back(type);
			}

			// FunctionType::get has different arguments for no-argument functions
			if (Args.size() == 0)
			{
				FT = FunctionType::get(TypeToLLVM(Type, Ident), false);
			}
			else
			{
				FT = FunctionType::get(TypeToLLVM(Type, Ident), Args, false);
			}

			FuncDef = Function::Create(FT, Function::ExternalLinkage, Ident.lexeme, TheModule.get());

			int Idx = 0;
			for (auto &Arg : FuncDef->args())
			{
				Arg.setName(Params[Idx++]->getIdent().lexeme);
			}

			// In the case of __Defining__ an extern function
			// We do not create a BasicBlock for a body
			// We just return the prototype
			if (FuncBlock == nullptr)
			{
				return FuncDef;
			}
		}


		BasicBlock *BB = BasicBlock::Create(TheContext, "entry", FuncDef);
		Builder.SetInsertPoint(BB);


		// Create a new level of scope
		std::unordered_map<std::string, llvm::AllocaInst *> FuncScope;
		ScopedNamedValues.push_back(FuncScope);

		for (auto &Arg : FuncDef->args())
		{
			llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(FuncDef, Arg.getName().data(), Arg.getType());
			Builder.CreateStore(&Arg, Alloca);

			ScopedNamedValues.back()[Arg.getName().data()] = Alloca;
		}

		// Flag used to check if we need to add another layer of scope
		isFuncBlock = true; 

		FuncContainsReturn = FuncBlock->codegen() != nullptr;
		
		bool AllPathsReturn = IfPathsReturn || FuncContainsReturn;
		llvm::Type *FuncReturnType = Builder.getCurrentFunctionReturnType();

		// If the function isn't void
		// and not all paths return a value this must throw an exception
		if (!AllPathsReturn)
		{
			if (FuncReturnType->isVoidTy())
			{
				Builder.CreateRetVoid();
			}
			else
			{
				throw SemanticException("Not all code paths return value in non-void function.", Ident.lineNo, Ident.columnNo);
			}
		}
		
		/**
		 * Need to create empty return value if a if-statement will gurantee a return from a function.
		 */
		if (IfPathsReturn && !FuncContainsReturn){
			if (!IfStmtLast){
				Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Any code after if-statement will not be executed in Function: "+Ident.lexeme, Ident.lineNo, Ident.columnNo));
			}
			
			if (FuncReturnType->isVoidTy())
			{
				Builder.CreateRetVoid();
			}
			else if (FuncReturnType->isFloatTy())
			{
				Builder.CreateRet(ConstantFP::get(TheContext, APFloat(0.0f)));
			}
			else if (FuncReturnType->isIntegerTy(1))
			{
				Builder.CreateRet(ConstantInt::get(TheContext, APInt(1, 0, false)));
			}
			else if (FuncReturnType->isIntegerTy(32))
			{
				Builder.CreateRet(ConstantInt::get(TheContext, APInt(32, 0, true)));
			}
		}

		llvm::verifyFunction(*FuncDef);

		ScopedNamedValues.pop_back();
		return FuncDef;
	};
};

#pragma endregion
/// =================================== !! Functions End !! ================================================ ///

/// ==================================== Program & Decls START !! =============================================== ///
#pragma region
class DeclAST : public ASTnode
{
	std::unique_ptr<FuncDeclAST> FuncDecl;
	std::unique_ptr<VariableDeclAST> VarDecl;

public:
	DeclAST(std::unique_ptr<FuncDeclAST> FuncDecl, std::unique_ptr<VariableDeclAST> VarDecl)
		: FuncDecl(std::move(FuncDecl)), VarDecl(std::move(VarDecl)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		if (FuncDecl != nullptr)
		{
			FuncDecl->to_string(prefix, "GlobalFuncDef: ", isLeft);
		}
		if (VarDecl != nullptr)
		{
			VarDecl->to_string(prefix, "GlobalVarDecl: ", isLeft);
		}
	};

	/**
	 * @brief Generates the function declerations and all the variable declerations
	 * 
	 * @return llvm::Value* 
	 */
	llvm::Value *codegen() override
	{
		if (FuncDecl != nullptr)
		{
			FuncDecl->codegen();
		}

		if (VarDecl != nullptr)
		{
			VarDecl->codegen();
		}
		return nullptr;
	};
};

class ProgramAST : public ASTnode
{
	std::vector<std::unique_ptr<FuncDeclAST>> ExternList;
	std::vector<std::unique_ptr<DeclAST>> DeclList;

public:
	ProgramAST(std::vector<std::unique_ptr<FuncDeclAST>> ExternList, std::vector<std::unique_ptr<DeclAST>> DeclList)
		: ExternList(std::move(ExternList)), DeclList(std::move(DeclList)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{

		std::cout << nodeStr << std::endl;

		for (int i = 0; i < ExternList.size(); i++)
		{
			ExternList[i]->to_string(prefix + (isLeft ? "│   " : "    "), "ExternFunc: ", (i != ExternList.size() - 1) || (DeclList.size() != 0));
		}

		for (int i = 0; i < DeclList.size(); i++)
		{
			DeclList[i]->to_string(prefix + (isLeft ? "│   " : "    "), "GlobalDecl", (i != DeclList.size() - 1));
		}
	};

	/**
	 * @brief Genereates all the extern declerations and the function/variable decelerations 
	 * 
	 * @return llvm::Value* 
	 */
	llvm::Value *codegen() override
	{
		for (auto &Extern : ExternList)
		{
			Extern->codegen();
		}

		for (auto &Decl : DeclList)
		{
			Decl->codegen();
		}
		return nullptr;
	};
};
#pragma endregion
/// =================================== !! Program & Decls END !! ================================================ ///

/// =================================== !! Literal AST Start !! ================================================ ///
#pragma region

class IntegerAST : public ExprAST
{
	TOKEN Val;

public:
	IntegerAST(TOKEN Val)
		: Val(std::move(Val)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "Int Literal: " << Val.lexeme << std::endl;
	};

	/**
	 * @brief Returns constant int value
	 *
	 * @return llvm::Value*
	 */
	llvm::Value *codegen() override
	{
		return ConstantInt::get(TheContext, APInt(32, std::stoi(Val.lexeme), true));
	};
};
class FloatAST : public ExprAST
{
	TOKEN Val;

public:
	FloatAST(TOKEN Val)
		: Val(std::move(Val)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "Float Literal: " << Val.lexeme << std::endl;
	};

	/**
	 * @brief Returns constant float value
	 *
	 * @return llvm::Value*
	 */
	llvm::Value *codegen() override
	{
		return ConstantFP::get(TheContext, APFloat(std::stof(Val.lexeme)));
	};
};

class BoolAST : public ExprAST
{
	TOKEN Val;

public:
	BoolAST(TOKEN Val)
		: Val(std::move(Val)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const override
	{
		std::cout << prefix;

		std::cout << (isLeft ? "├──" : "└──");

		std::cout << "Bool Literal: " << Val.lexeme << std::endl;
	};

	/**
	 * @brief Returns constant bool value
	 *
	 * @return llvm::Value*
	 */
	llvm::Value *codegen() override
	{
		int boolVal = (Val.lexeme == "false" ? 0 : 1);

		return ConstantInt::get(TheContext, APInt(1, boolVal, false));
	};
};
#pragma endregion
/// =================================== !! Literal AST End !! ================================================ ///

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

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
static std::unique_ptr<DeclAST> Decl();
static void Decl_List_Prime(std::unique_ptr<DeclAST> &decl_list);
static std::vector<std::unique_ptr<DeclAST>> Decl_List();
static std::unique_ptr<FuncDeclAST> Extern();
static void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list);
static std::vector<std::unique_ptr<FuncDeclAST>> Extern_List();
static std::unique_ptr<ProgramAST> Program();
#pragma endregion
// ----- Function Declerations End ----- //

// ----- Helper Functions ------ //
#pragma region
/**
 * @brief Checks if the current token is the same as the expected token. If not an error is thrown
 *
 * @param expectedTokenType
 * @param errMessage
 * @param prodRule
 */
static void Match(TOKEN_TYPE expectedTokenType, string errMessage, const char *prodRule = __builtin_FUNCTION())
{
	if (CurTok.type != expectedTokenType)
	{
		throw ParseException("\033[0;31mInvalid Token Error:\033[0m " + errMessage);
	}
	getNextToken();
}

/**
 * @brief Get the Ident And Match object
 *
 * @param CurTok
 * @return TOKEN Token of identifer if no error, otherwise an error is a thrown
 */
static TOKEN GetIdentAndMatch()
{
	TOKEN prev_token = CurTok;
	Match(IDENT, "Expected identifer token. ");
	return prev_token;
}

/**
 * @brief Looks at the next token in the queue without removing it
 *
 * @return TOKEN Next token in the queue
 */
static TOKEN PeekToken()
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
static bool ValidType()
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
static bool ValidExprStart()
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

#pragma endregion
// ------- Helper Functions End ------- //

#pragma region

// arg_list_prime ::= "," expr arg_list_prime | epsilon
static void Arg_List_Prime(std::vector<std::unique_ptr<ExprAST>> &args)
{
	if (CurTok.type == RPAR)
	{
		return;
	}

	if (CurTok.type == COMMA)
	{
		Match(COMMA, "Expected ',' after argument");
		auto arg = Expr();
		args.push_back(std::move(arg));

		Arg_List_Prime(args);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: End of Function Call or New Argument. ");
	}
}

// arg_list ::= expr arg_list_prime
static std::vector<std::unique_ptr<ExprAST>> Arg_List()
{
	std::vector<std::unique_ptr<ExprAST>> args;

	if (ValidExprStart())
	{
		auto expr = Expr();
		args.push_back(std::move(expr));

		Arg_List_Prime(args);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Function Argument. ");
	}

	return args;
}

// args ::= arg_list |  epsilon
static std::vector<std::unique_ptr<ExprAST>> Args()
{
	std::vector<std::unique_ptr<ExprAST>> args;

	if (ValidExprStart())
	{
		args = Arg_List();
	}
	else if (CurTok.type != RPAR)
	{
		throw ParseException("Invalid Token Error: \nExpected: Function Argument or End of Function Call. ");
	}

	return args;
}

// rval_term ::= INT_LIT | FLOAT_LIT | BOOL_LIT
static std::unique_ptr<ExprAST> Rval_Term()
{
	std::unique_ptr<ExprAST> expr;
	TOKEN lit_tok = CurTok;

	switch (CurTok.type)
	{
	case BOOL_LIT:
	{
		Match(BOOL_LIT, "Expected bool literal. ");

		expr = std::make_unique<BoolAST>(std::move(lit_tok));
		break;
	}
	case FLOAT_LIT:
	{
		Match(FLOAT_LIT, "Expected float literal. ");

		expr = std::make_unique<FloatAST>(std::move(lit_tok));
		break;
	}
	case INT_LIT:
	{
		Match(INT_LIT, "Expected int literal. ");

		expr = std::make_unique<IntegerAST>(std::move(lit_tok));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: Literal. ");
	}

	return expr;
}

// rval_ident_prime ::= epsilon | "(" args ")"
static std::unique_ptr<ExprAST> Rval_Ident_Prime(TOKEN ident)
{
	std::vector<std::unique_ptr<ExprAST>> args;
	std::unique_ptr<ExprAST> expr;

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
		expr = std::make_unique<Variable>(std::move(ident));
		break;
	case LPAR:
	{
		Match(LPAR, "Expected '(' before function call. ");
		args = Args();
		Match(RPAR, "Expected ')' after function call. ");

		expr = std::make_unique<FuncCallAST>(std::move(ident), std::move(args));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}

	return expr;
}

// rval_ident ::= IDENT rval_ident_prime | rval_term
static std::unique_ptr<ExprAST> Rval_Ident()
{
	std::unique_ptr<ExprAST> expr;

	if (CurTok.type == BOOL_LIT || CurTok.type == FLOAT_LIT || CurTok.type == INT_LIT)
	{
		expr = Rval_Term();
	}
	else if (CurTok.type == IDENT)
	{
		TOKEN ident = GetIdentAndMatch();

		expr = Rval_Ident_Prime(std::move(ident));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Variable, Literal or Function Call. ");
	}

	return expr;
}

// rval_par ::= "(" expr ")" | rval_ident
static std::unique_ptr<ExprAST> Rval_Par()
{
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
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}

	return expr;
}

// rval_neg ::= "-" rval_neg | "!" rval_neg | rval_par
static std::unique_ptr<ExprAST> Rval_Neg()
{
	std::unique_ptr<ExprAST> unary_expr;
	TOKEN Op_Token = CurTok;

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
		Match(NOT, "Expected '!' operator. ");

		auto expr = Rval_Neg();

		unary_expr = std::make_unique<UnaryExprAST>(std::move(Op_Token), std::move(expr));
		break;
	}
	case MINUS:
	{
		Match(MINUS, "Expected '-' operator. ");

		auto expr = Rval_Neg();

		unary_expr = std::make_unique<UnaryExprAST>(std::move(Op_Token), std::move(expr));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}

	return unary_expr;
}

// rval_mul_prime ::= "*" rval_neg  | "/" rval_neg  | "%" rval_neg | epsilon
static std::unique_ptr<ExprAST> Rval_Mul_Prime(std::unique_ptr<ExprAST> LHS)
{
	// cout << "Rval_Mul_Prime" << endl;
	std::unique_ptr<ExprAST> LHS_Prime;
	std::unique_ptr<ExprAST> RHS;
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token = CurTok;

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
		expr = std::move(LHS);
		break;
	case MOD:
	{
		Match(MOD, "Expected '%' operator. ");

		LHS_Prime = Rval_Neg();
		RHS = Rval_Mul_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case DIV:
	{
		Match(DIV, "Expected '/' operator. ");

		LHS_Prime = Rval_Neg();
		RHS = Rval_Mul_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case ASTERIX:
	{
		Match(ASTERIX, "Expected '*' operator. ");

		LHS_Prime = Rval_Neg();
		RHS = Rval_Mul_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}
	return expr;
}

// rval_mul ::= rval_neg rval_mul_prime
static std::unique_ptr<ExprAST> Rval_Mul()
{
	std::unique_ptr<ExprAST> LHS = Rval_Neg();

	while (CurTok.type == ASTERIX || CurTok.type == MOD || CurTok.type == DIV){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Neg();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}
	
	return Rval_Mul_Prime(std::move(LHS));
}

// rval_add_prime ::= "+" rval_mul  | "-" rval_mul | epsilon
static std::unique_ptr<ExprAST> Rval_Add_Prime(std::unique_ptr<ExprAST> LHS)
{
	// cout << "Rval_Add_Prime" << endl;
	std::unique_ptr<ExprAST> LHS_Prime;
	std::unique_ptr<ExprAST> RHS;
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token = CurTok;

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
		expr = std::move(LHS);
		break;
	case PLUS:
	{
		Match(PLUS, "Expected '+' operator. ");

		LHS_Prime = Rval_Mul();
		RHS = Rval_Add_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case MINUS:
	{
		Match(MINUS, "Expected '-' operator. ");

		LHS_Prime = Rval_Mul();
		RHS = Rval_Add_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}
	return expr;
}

// rval_add ::= rval_mul rval_add_prime
static std::unique_ptr<ExprAST> Rval_Add()
{
	std::unique_ptr<ExprAST> LHS = Rval_Mul();
	
	while (CurTok.type == PLUS || CurTok.type == MINUS){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Mul();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Add_Prime(std::move(LHS));
}

// rval_cmp_prime ::= "<=" rval_add | "<" rval_add | ">=" rval_add | ">" rval_add | epsilon
static std::unique_ptr<ExprAST> Rval_Cmp_Prime(std::unique_ptr<ExprAST> LHS)
{
	std::unique_ptr<ExprAST> LHS_Prime;
	std::unique_ptr<ExprAST> RHS;
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token = CurTok;

	switch (CurTok.type)
	{
	case COMMA:
	case RPAR:
	case SC:
	case OR:
	case AND:
	case EQ:
	case NE:
		expr = std::move(LHS);
		break;
	case LT:
	{
		Match(LT, "Expected '<' operator. ");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case GT:
	{
		Match(GT, "Expected '>' operator. ");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case LE:
	{
		Match(LE, "Expected '<=' operator. ");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case GE:
	{
		Match(GE, "Exepcted '>=' operator. ");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}
	return expr;
}

// rval_cmp ::= rval_add rval_cmp_prime
static std::unique_ptr<ExprAST> Rval_Cmp()
{
	std::unique_ptr<ExprAST> LHS = Rval_Add();
	
	while (CurTok.type == LT || CurTok.type == LE || CurTok.type == GT || CurTok.type == GE){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Add();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Cmp_Prime(std::move(LHS));
}

// rval_eq_prime ::= "==" rval_cmp | "!=" rval_cmp | epsilon
static std::unique_ptr<ExprAST> Rval_Eq_Prime(std::unique_ptr<ExprAST> LHS)
{
	std::unique_ptr<ExprAST> LHS_Prime;
	std::unique_ptr<ExprAST> RHS;
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token = CurTok;

	switch (CurTok.type)
	{
	case COMMA:
	case RPAR:
	case SC:
	case OR:
	case AND:
		expr = std::move(LHS);
		break;
	case EQ:
	{
		Match(EQ, "Expected '==' operator. ");

		LHS_Prime = Rval_Cmp();
		RHS = Rval_Eq_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case NE:
	{
		Match(NE, "Expected '!=' operator. ");

		LHS_Prime = Rval_Cmp();
		RHS = Rval_Eq_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}

	return expr;
}

// rval_eq ::= rval_cmp rval_eq_prime
static std::unique_ptr<ExprAST> Rval_Eq()
{
	std::unique_ptr<ExprAST> LHS = Rval_Cmp();
	
	while (CurTok.type == EQ || CurTok.type == NE){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Cmp();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Eq_Prime(std::move(LHS));
}

// rval_and_prime ::= "&&" rval_eq rval_and_prime | epsilon
static std::unique_ptr<ExprAST> Rval_And_Prime(std::unique_ptr<ExprAST> LHS)
{
	std::unique_ptr<ExprAST> LHS_Prime;
	std::unique_ptr<ExprAST> RHS;
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token = CurTok;

	switch (CurTok.type)
	{
	case COMMA:
	case RPAR:
	case SC:
	case OR:
		expr = std::move(LHS);
		break;
	case AND:
	{
		Match(AND, "Expected '&&' operator. ");

		LHS_Prime = Rval_Eq();
		RHS = Rval_And_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}
	return expr;
}

// rval_and ::= rval_eq rval_and_prime
static std::unique_ptr<ExprAST> Rval_And()
{
	std::unique_ptr<ExprAST> LHS = Rval_Eq();

	while (CurTok.type == AND){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Eq();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}
	
	return Rval_And_Prime(std::move(LHS));
}

// rval_or_prime ::= "||" rval_and rval_or_prime | epsilon
static std::unique_ptr<ExprAST> Rval_Or_Prime(std::unique_ptr<ExprAST> LHS)
{
	std::unique_ptr<ExprAST> LHS_Prime;
	std::unique_ptr<ExprAST> RHS;
	std::unique_ptr<ExprAST> expr;
	TOKEN Op_Token = CurTok;

	switch (CurTok.type)
	{
	case COMMA:
	case RPAR:
	case SC:
		expr = std::move(LHS);
		break;
	case OR:
	{
		Match(OR, "Expected '||' operator. ");

		LHS_Prime = Rval_And();
		RHS = Rval_Or_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}

	return expr;
}

// rval_or ::= rval_and rval_or_prime
static std::unique_ptr<ExprAST> Rval_Or()
{
	std::unique_ptr<ExprAST> LHS = Rval_And();
	
	while (CurTok.type == OR){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_And();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Or_Prime(std::move(LHS));
}

// expr ::= IDENT "=" expr | rval_or
static std::unique_ptr<ExprAST> Expr()
{
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
		TOKEN NextTok = PeekToken();

		if (NextTok.type == ASSIGN)
		{
			TOKEN ident = GetIdentAndMatch();

			Match(ASSIGN, "Expected '=' after variable identifer. ");

			auto var_expr = Expr();

			expr = std::make_unique<VariableAssignmentAST>(std::move(ident), std::move(var_expr));
		}
		else
		{
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
		throw ParseException("Invalid Token Error: \nExpected: Expected Assignment or Start of Expression. ");
	}
	return expr;
}

// return_stmt_prime ::= ";" | expr ";"
static std::unique_ptr<ReturnAST> Return_Stmt_Prime()
{
	std::unique_ptr<ExprAST> expr;
	TOKEN returnTok; 
	if (ValidExprStart())
	{
		expr = Expr();

		returnTok = CurTok;
		Match(SC, "Expected ';' after return expression. ");
	}
	else if (CurTok.type == SC)
	{
		returnTok = CurTok;
		Match(SC, "Expected ';' after return keyword. ");
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of expression. ");
	}

	return std::make_unique<ReturnAST>(std::move(expr), std::move(returnTok));
}

// return_stmt ::= "return" return_stmt_prime
static std::unique_ptr<ReturnAST> Return_Stmt()
{
	std::unique_ptr<ReturnAST> return_stmt;

	if (CurTok.type == RETURN)
	{
		Match(RETURN, "Expected 'return' keyword. ");
		return_stmt = Return_Stmt_Prime();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: {RETURN}");
	}

	return return_stmt;
}

// else_stmt  ::= "else" block | epsilon
static std::unique_ptr<BlockAST> Else_Stmt()
{
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
		throw ParseException("Invalid Token Error: \nExpected: Else block of IF Statment or New Valid Statement ");
	}

	return else_block;
}

// if_stmt ::= "if" "(" expr ")" block else_stmt
static std::unique_ptr<IfAST> If_Stmt()
{
	std::unique_ptr<ExprAST> condition_expr;
	std::unique_ptr<BlockAST> true_block;
	std::unique_ptr<BlockAST> else_block;

	if (CurTok.type == IF)
	{
		Match(IF, "Expected 'if' keyword. ");
		Match(LPAR, "Expected '(' before if condition. ");

		condition_expr = Expr();

		Match(RPAR, "Expected ')' after if condition. ");

		true_block = Block();
		else_block = Else_Stmt();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: 'if' keyword. ");
	}

	return std::make_unique<IfAST>(std::move(condition_expr), std::move(true_block), std::move(else_block));
}

// while_stmt ::= "while" "(" expr ")" stmt
static std::unique_ptr<WhileAST> While_Stmt()
{
	std::unique_ptr<ExprAST> condition_expr;
	std::unique_ptr<StmtAST> loop_block;

	if (CurTok.type == WHILE)
	{
		Match(WHILE, "Expected 'While' keyword. ");
		Match(LPAR, "Expected '(' before loop condition. ");

		condition_expr = Expr();

		Match(RPAR, "Expected ')' after loop condition. ");

		loop_block = Stmt();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: 'while' keyword. ");
	}

	return std::make_unique<WhileAST>(std::move(condition_expr), std::move(loop_block));
}

// expr_stmt ::= expr ";" | ";"
static std::unique_ptr<ExprAST> Expr_Stmt()
{
	std::unique_ptr<ExprAST> expr;
	if (ValidExprStart())
	{
		expr = Expr();
		Match(SC, "Expected ';' after expression. ");
	}
	else if (CurTok.type == SC)
	{
		Match(SC, "Expected ';' after expression. ");
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of expression. ");
	}

	return expr;
}

// stmt ::= expr_stmt |  block |  if_stmt |  while_stmt |  return_stmt
static std::unique_ptr<StmtAST> Stmt()
{
	std::unique_ptr<StmtAST> stmt;

	if (ValidExprStart() || CurTok.type == SC)
	{
		stmt = Expr_Stmt();
	}
	else if (CurTok.type == RETURN)
	{
		stmt = Return_Stmt();
	}
	else if (CurTok.type == IF)
	{
		stmt = If_Stmt();
	}
	else if (CurTok.type == WHILE)
	{
		stmt = While_Stmt();
	}
	else if (CurTok.type == LBRA)
	{
		stmt = Block();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: One Of [IfStatment, WhileLoop, ReturnStmt, '{' (Start of New Block), Expression]");
	}

	return stmt;
}

// stmt_list ::= stmt stmt_list | epsilon
static void Stmt_List(std::vector<std::unique_ptr<StmtAST>> &stmt_list)
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
	case SC:
	case RETURN:
	case IF:
	case WHILE:
	case LBRA:
	{
		auto stmt = Stmt();

		if (stmt != nullptr)
		{
			stmt_list.push_back(std::move(stmt));
		}

		Stmt_List(stmt_list);
		break;
	}
	case RBRA:
		break;
	default:
		throw ParseException("Invalid Token Error: \nCannot declare variables after a statement. ");
	}
}

// local_decl ::= var_type IDENT ";"
static std::unique_ptr<VariableDeclAST> Local_Decl()
{
	VAR_TYPE type;
	TOKEN ident;

	if (ValidType())
	{
		type = Var_Type();
		ident = GetIdentAndMatch();

		Match(SC, "Expeceted ';' after variable decleration. ");
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Type decleration. ");
	}

	return std::make_unique<VariableDeclAST>(std::move(ident), std::move(type));
}

// local_decls ::= local_decl local_decls | epsilon
static void Local_Decls(std::vector<std::unique_ptr<VariableDeclAST>> &variable_decls)
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
static std::unique_ptr<BlockAST> Block()
{
	std::vector<std::unique_ptr<VariableDeclAST>> variable_decls;
	std::vector<std::unique_ptr<StmtAST>> stmt_list;

	if (CurTok.type == LBRA)
	{
		Match(LBRA, "Expected '{' to declare new scope. ");

		Local_Decls(variable_decls);
		Stmt_List(stmt_list);

		Match(RBRA, "Expected '}' after statement. ");
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: '{' to declare new scope. ");
	}

	return std::make_unique<BlockAST>(std::move(variable_decls), std::move(stmt_list));
}

// param ::= var_type IDENT
static std::unique_ptr<ParamAST> Param()
{
	VAR_TYPE type;
	TOKEN ident;

	if (ValidType())
	{
		type = Var_Type();
		ident = GetIdentAndMatch();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Parameter type decleration. ");
	}

	return std::make_unique<ParamAST>(std::move(ident), std::move(type));
}

// param_list_prime ::= "," param param_list_prime | epsilon
static void Param_List_Prime(std::vector<std::unique_ptr<ParamAST>> &param_list)
{
	if (CurTok.type == RPAR)
	{
		return;
	}

	if (CurTok.type == COMMA)
	{
		Match(COMMA, "Expected ',' or ')' after function parameter");
		auto param = Param();
		param_list.push_back(std::move(param));

		Param_List_Prime(param_list);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: ',' or ')' in function paramters");
	}
}

// param_list ::= param param_list_prime
static std::vector<std::unique_ptr<ParamAST>> Param_List()
{
	std::vector<std::unique_ptr<ParamAST>> param_list;

	if (ValidType())
	{
		auto param = Param();
		param_list.push_back(std::move(param));

		Param_List_Prime(param_list);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Paramter Type. ");
	}

	return param_list;
}

// params ::= param_list |  "void" | epsilon
static std::vector<std::unique_ptr<ParamAST>> Params()
{
	std::vector<std::unique_ptr<ParamAST>> param_list;

	if (CurTok.type == RPAR)
	{
		return param_list;
	}

	if (CurTok.type == VOID_TOK)
	{
		Match(VOID_TOK, "Expected 'void' token in function paramters");
	}
	else if (ValidType())
	{
		param_list = Param_List();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: End of Paramters or Paramter Type. ");
	}

	return param_list;
}

// var_type  ::= "int" |  "float" |  "bool"
static VAR_TYPE Var_Type()
{
	VAR_TYPE type;

	if (CurTok.type == BOOL_TOK)
	{
		Match(BOOL_TOK, "Expected 'bool' keyword.");
		type = BOOL_TYPE;
	}
	else if (CurTok.type == FLOAT_TOK)
	{
		Match(FLOAT_TOK, "Expected 'float' keyword.");
		type = FLOAT_TYPE;
	}
	else if (CurTok.type == INT_TOK)
	{
		Match(INT_TOK, "Expected 'int' keyword.");
		type = INT_TYPE;
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Variable type decleration. ");
	}

	return type;
}

// type_spec ::= "void" | var_type
static VAR_TYPE Type_Spec()
{
	VAR_TYPE type;

	if (CurTok.type == VOID_TOK)
	{
		Match(VOID_TOK, "Expected 'void' keyword.");
		type = VOID_TYPE;
	}
	else if (ValidType())
	{
		type = Var_Type();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Type decleration. ");
	}

	return type;
}

// decl_prime ::= ";" | "(" params ")" block
static void Decl_Prime(std::unique_ptr<FuncDeclAST> &func_decl, std::unique_ptr<VariableDeclAST> &var_decl, VAR_TYPE type, TOKEN ident)
{

	if (CurTok.type == LPAR)
	{
		Match(LPAR, "Expected '(' after function decleration. ");

		auto params = Params();

		Match(RPAR, "Expected ')' after function paramters. ");

		auto block = Block();

		func_decl = std::make_unique<FuncDeclAST>(std::move(ident), std::move(type), std::move(params), std::move(block));
	}
	else if (CurTok.type == SC)
	{
		Match(SC, "Expected ';' after variable decleration. ");

		var_decl = std::make_unique<VariableDeclAST>(std::move(ident), std::move(type));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: ';' or Function Paramters");
	}
}

// decl ::= var_type IDENT decl_prime | "void" IDENT "(" params ")" block
static std::unique_ptr<DeclAST> Decl()
{
	std::unique_ptr<FuncDeclAST> func_decl;
	std::unique_ptr<VariableDeclAST> var_decl;

	if (CurTok.type == VOID_TOK)
	{
		Match(VOID_TOK, "Expected 'void' token before function decleration. ");

		TOKEN ident = GetIdentAndMatch();

		Match(LPAR, "Expeceted '(' after function identifer. ");

		auto params = Params();

		Match(RPAR, "Expected ')' after parameter list. ");

		auto block = Block();

		func_decl = std::make_unique<FuncDeclAST>(std::move(ident), std::move(VOID_TYPE), std::move(params), std::move(block));
	}
	else if (ValidType())
	{
		auto type = Var_Type();

		TOKEN ident = GetIdentAndMatch();

		Decl_Prime(func_decl, var_decl, type, ident);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Variable or Function type decleration");
	}

	return std::make_unique<DeclAST>(std::move(func_decl), std::move(var_decl));
}

// decl_list_prime ::= decl decl_list_prime | epsilon
static void Decl_List_Prime(std::vector<std::unique_ptr<DeclAST>> &decl_list)
{
	if (CurTok.type == EOF_TOK)
	{
		return;
	}

	if (ValidType() || CurTok.type == VOID_TOK)
	{
		auto decl = Decl();
		decl_list.push_back(std::move(decl));

		Decl_List_Prime(decl_list);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: decleration type or end_of_file");
	}
}

// decl_list ::= decl decl_list_prime
static std::vector<std::unique_ptr<DeclAST>> Decl_List()
{
	std::vector<std::unique_ptr<DeclAST>> decl_list;

	if (ValidType() || CurTok.type == VOID_TOK)
	{
		auto decl = Decl();
		decl_list.push_back(std::move(decl));

		Decl_List_Prime(decl_list);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected Variable or Function type decleration. ");
	}

	return decl_list;
};

// extern ::= "extern" type_spec IDENT "(" params ")" ";"
static std::unique_ptr<FuncDeclAST> Extern()
{
	TOKEN ident;
	VAR_TYPE type;
	std::vector<std::unique_ptr<ParamAST>> params;
	std::unique_ptr<BlockAST> emptyblock;

	if (CurTok.type == EXTERN)
	{
		Match(EXTERN, "EXTERN");

		type = Type_Spec();

		ident = GetIdentAndMatch();

		Match(LPAR, "Expected '(' after identifer keyword.");
		params = Params();
		Match(RPAR, "Expected ')' after function paramters.");

		Match(SC, "Expected ';' after function definition.");
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected 'extern' keyword. ");
	}

	return std::make_unique<FuncDeclAST>(std::move(ident), std::move(type), std::move(params), std::move(emptyblock));
}

// extern_list_prime ::= extern extern_list_prime | epsilon
static void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list)
{

	if (ValidType() || CurTok.type == VOID_TOK)
	{
		return;
	}

	if (CurTok.type == EXTERN)
	{
		auto e = Extern();
		extern_list.push_back(std::move(e));

		Extern_List_Prime(extern_list);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: 'extern' keyword or extern function type. ");
	}
}

// extern_list ::= extern extern_list_prime
static std::vector<std::unique_ptr<FuncDeclAST>> Extern_List()
{
	std::vector<std::unique_ptr<FuncDeclAST>> extern_list;

	if (CurTok.type == EXTERN)
	{
		auto e = Extern();
		extern_list.push_back(std::move(e));

		Extern_List_Prime(extern_list);
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected 'extern' Keyword. ");
	}

	return extern_list;
};

// program ::= extern_list decl_list | decl_list
static std::unique_ptr<ProgramAST> Program()
{
	std::vector<std::unique_ptr<FuncDeclAST>> extern_list;
	std::vector<std::unique_ptr<DeclAST>> decl_list;

	if (ValidType() || CurTok.type == VOID_TOK)
	{
		decl_list = Decl_List();
	}
	else if (CurTok.type == EXTERN)
	{
		extern_list = Extern_List();
		decl_list = Decl_List();
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Extern declerations or Function declerations. ");
	}

	return std::make_unique<ProgramAST>(std::move(extern_list), std::move(decl_list));
}

#pragma endregion

static std::unique_ptr<ProgramAST> root;

/**
 * @brief Attempts to create a Root Program AST Node
 * Any syntax errors are thrown and caught and reported,
 *
 * If no syntax errors exist, the AST Printer is ran and prints the AST for the given program
 *
 */
static void parser()
{
	getNextToken();
	try
	{
		root = Program();

		if (CurTok.type != EOF_TOK)
		{
			throw ParseException("Invalid Token Error: \nExpected: end_of_file. ");
		}

		// AST Printer
		root->to_string("", "Program", false);
	}
	catch (const std::exception &e)
	{
		std::cout << e.what() << std::endl
				  << "Got: " << CurTok.lexeme << " line: " << CurTok.lineNo << " col: " << CurTok.columnNo << endl;
		exit(1);
	}
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv)
{
	if (argc == 2)
	{
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
	 */
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
	raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

	if (EC)
	{
		errs() << "Could not open file: " << EC.message();
		return 1;
	}
	TheModule->print(errs(), nullptr); // print IR to terminal#

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

	TheModule->print(dest, nullptr);
	//********************* End printing final IR ****************************

	fclose(pFile); // close the file that contains the code that was parsed
	return 0;
}
