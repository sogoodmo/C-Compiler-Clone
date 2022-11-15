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

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

/**
 * Contains a vector of maps- Where the index of the map indicates it's level of scope 
 * 
 * A block may access variables in a scope prior to it in the array, but not after it.  
 * 
 * If a variable is defined multiple times 
 */
static std::vector<std::map<std::string, llvm::AllocaInst*>> ScopedNamedValues;
static std::map<std::string, llvm::GlobalVariable*> GlobalVariables;


/**
 * Class to store warnings, but not crash program 
*/
class Warning
{
	string Err;
	int lineno;
	int colno;

public:
	Warning(string err, int line, int col) : Err(err), lineno(line), colno(col) {}

	std::string to_string(){
		return Err + " On Line: " + std::to_string(lineno) + " Col: " + std::to_string(colno); 
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

public:
	SemanticException(string err) : Err(err) {}

	virtual const char *what() const throw()
	{
		return Err.c_str();
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
bool mapContainsKey(std::map<K, V>& map, K key)
{
  if (map.find(key) == map.end()) return false;
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
 * @brief Returns the matching llvm type for the type passed in. 
 * 
 * @param type The type of the variable 
 * @return const llvm::Type The LLVM Type of the variable passed 
 */
llvm::Type* TypeToLLVM(VAR_TYPE type){

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
		throw SemanticException("Type Error:\nExpected one of ['int', 'void', 'bool', 'float']");
	}
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

// Create an alloca instruction in the entry block of the function.  
// This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, llvm::Type *type) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, 0, VarName.c_str());
}


/**
 * @brief Adds warning to warning buffer if attempting to implicitly convert and may lose precision
 * Returns new value after conversion.
 * 
 * @param val Value 
 * @param alloca Alloca to update
 */
static llvm::Value* ImplicitCasting(llvm::Value *val,  llvm::Type *newType, TOKEN tokInfo){
	llvm::Type *oldType = val->getType();
	
	//Float -> Int Conversion - Precision Loss 
	if (oldType->isFloatTy() && newType->isIntegerTy(32))
	{
		Warnings.push_back(Warning("Warning: Implict conversion from Float to Int32. May lose precision", tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateIntCast(val, newType, false);
	} 
	
	//Float -> Bool Conversion - Precision Loss 
	if (oldType->isFloatTy() && newType->isIntegerTy(1)) 
	{
		Warnings.push_back(Warning("Warning: Implict conversion from Bool to Float. May lose precision", tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateFCmpONE(val, ConstantFP::get(TheContext, APFloat(0.0f)));
	}
	
	//Int -> Bool Conversion - Precision Loss 
	if (oldType->isIntegerTy(32) && newType->isIntegerTy(1))
	{
		Warnings.push_back(Warning("Warning: Implict conversion from Float to Int32. May lose precision", tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateICmpNE(val, ConstantInt::get(Type::getInt32Ty(TheContext), 0));
	}
	
	//Int -> Float Conversion - No Precision Loss 
	if (oldType->isIntegerTy(32) && newType->isFloatTy()) 
	{
		Warnings.push_back(Warning("Warning: Implict conversion from Int32 to Float. May result in unexpected behaivour", tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateUIToFP(val, newType);
	}

	//Bool -> Float Conversion - No Precision Loss 
	if (oldType->isIntegerTy(1) && newType->isFloatTy()) 
	{
		Warnings.push_back(Warning("Warning: Implict conversion from Bool to Float. May result in unexpected behaivour", tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateUIToFP(val, newType);
	}

	//Bool -> Int Conversion - No Precision Loss 
	if (oldType->isIntegerTy(1) && newType->isIntegerTy(32)) 
	{

		Warnings.push_back(Warning("Warning: Implict conversion from Bool to Int32. May result in unexpected behaivour", tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateIntCast(val, newType, false);
	}

	throw SemanticException("Unexpected Type Error: Expected int, bool or float. ");
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
class VoidAST;

class StmtAST : public ASTnode{};
class ExprAST : public StmtAST{};

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

	llvm::Value *codegen() override{
		llvm::Value *V; 

		// Look up the values from the scopes, stopping at the deepest layer of scope it finds
		// If doesn't exist in any scope- try one more time in the global scope 
		// If it doesn't exist in the scope, then it will throw an error. 

		for (int i=ScopedNamedValues.size()-1; i > -1; i--){
			if (mapContainsKey(ScopedNamedValues[i], Ident.lexeme)){
				
				AllocaInst *alloca = ScopedNamedValues[i].at(Ident.lexeme);
				V = Builder.CreateLoad(alloca->getAllocatedType(), alloca, Ident.lexeme.c_str());
				
				return V;
			}
		}

		if (mapContainsKey(GlobalVariables, Ident.lexeme)){
			GlobalVariable *alloca = GlobalVariables.at(Ident.lexeme);
			V = Builder.CreateLoad(alloca->getValueType(), alloca, Ident.lexeme.c_str());
			return V;
		} 

		throw SemanticException("Undecleared variable referenced. Perhaps your variable is out of scope?");
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

	llvm::Value *codegen() override{
		bool isGlobal = ScopedNamedValues.size() == 0;
		llvm::Type* llvmType = TypeToLLVM(Type);


		if (isGlobal)
		{
			auto G = new GlobalVariable(*(TheModule.get()), llvmType, false, GlobalValue::CommonLinkage, nullptr);
			GlobalVariables.insert({Ident.lexeme, G});		
		}
		else
		{
			//nullptr may be replaced with `Constant::getNullValue(llvmType)`
			auto V = Builder.CreateAlloca(llvmType, nullptr, Ident.lexeme); 		
			ScopedNamedValues.back().insert({Ident.lexeme, V});
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
	 * @return llvm::Value* Nullptr as assigning a variable doesn't return a value 
	 */
	llvm::Value *codegen() override{
		llvm::Value *E = Expr->codegen();

		for (int i=ScopedNamedValues.size()-1; i > -1; i--){
			if (mapContainsKey(ScopedNamedValues[i], Ident.lexeme)){
				AllocaInst *alloca = ScopedNamedValues[i].at(Ident.lexeme);
				
				llvm::Value *CastedValue = ImplicitCasting(E, alloca->getAllocatedType(), Ident);

				Builder.CreateStore(CastedValue, alloca);
				ScopedNamedValues[i][Ident.lexeme] = alloca;
				return nullptr; 
			}
		}

		if (mapContainsKey(GlobalVariables, Ident.lexeme)){
			GlobalVariable *alloca = GlobalVariables.at(Ident.lexeme);

			llvm::Value *CastedValue = ImplicitCasting(E, alloca->getValueType(), Ident);

			Builder.CreateStore(CastedValue, alloca);
			GlobalVariables[Ident.lexeme] = alloca;
			return nullptr;
		} 

		throw SemanticException("Undecleared variable referenced. Perhaps your variable is out of scope?");
	}; 
};
#pragma endregion
/// =================================== !! Variable's END !! ================================================ ///

/// =================================== !! Block & Statements Start !! ================================================ ///
#pragma region
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

	llvm::Value *codegen() override{
		return nullptr;
	}; 
};
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

	llvm::Value *codegen() override{
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

	llvm::Value *codegen() override{
		return nullptr;
	}; 
};
class ReturnAST : public StmtAST
{
	std::unique_ptr<ExprAST> ReturnExpr;

public:
	ReturnAST(std::unique_ptr<ExprAST> ReturnExpr)
		: ReturnExpr(std::move(ReturnExpr)) {}

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

	llvm::Value *codegen() override{
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

		RHS->to_string(prefix + (isLeft ? "│   " : "    "), "", true);
		LHS->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
	};

	llvm::Value *codegen() override{
		return nullptr;
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

	llvm::Value *codegen() override{
		return nullptr;
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

	llvm::Value *codegen() override{
		return nullptr;
	};

	VAR_TYPE getType(){
		return Type; 
	} 
	TOKEN getIdent(){
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
	llvm::Value *codegen() override{
		Function *CalleeFunc = TheModule->getFunction(FuncName.lexeme);

		if (!CalleeFunc){
			throw SemanticException("Unknown Function Referenced: Perhaps you have misspelt your function. ");
		}

		if (Args.size() != CalleeFunc->arg_size()){
			throw SemanticException("Invalid Argument Size: Expected " + std::to_string(CalleeFunc->arg_size()) + ". Got " + std::to_string(Args.size()));
		}

		std::vector<Value *> ArgsV;
		for (auto &Arg : Args){
			ArgsV.push_back(Arg->codegen());
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
	llvm::Function *codegen() override {
		llvm::FunctionType *FT;
		llvm::Function *FuncDef;
		std::vector<llvm::Type*> Args;

		Function *ExternFuncDef = TheModule->getFunction(Ident.lexeme);

		/**
		 * Generating IR for function Prototype if it doesn't already exist 
		 */
		if (!ExternFuncDef){
			for (auto &Param : Params){
				llvm::Type* type = TypeToLLVM(Param->getType());
				Args.push_back(type);
			}

			// FunctionType::get has different arguments for no-argument functions 
			if (Args.size() == 0){
				FT = FunctionType::get(TypeToLLVM(Type), false); 
			} else {
				FT = FunctionType::get(TypeToLLVM(Type), Args, false); 
			}

			FuncDef = Function::Create(FT, Function::ExternalLinkage, Ident.lexeme, TheModule.get());


			unsigned Idx = 0;
			for (auto &Arg : FuncDef->args()){
				Arg.setName(Params[Idx++]->getIdent().lexeme);
			}


			// In the case of __Defining__ an extern function 
			// We do not create a BasicBlock for a body
			// We just return the prototype 
			if (FuncBlock == nullptr){
				return FuncDef; 
			}
		}

		BasicBlock *BB = BasicBlock::Create(TheContext, "entry", FuncDef);
		Builder.SetInsertPoint(BB);


		//Create a new level of scope 
		std::map<std::string, llvm::AllocaInst*> FuncScope; 
		ScopedNamedValues.push_back(FuncScope);

		for (auto &Arg : FuncDef->args()){
			AllocaInst *Alloca = CreateEntryBlockAlloca(FuncDef, Arg.getName().data(), Arg.getType());
			Builder.CreateStore(&Arg, Alloca);

			ScopedNamedValues.back()[std::string(Arg.getName())] = Alloca; 
		}


		if (Value *RetValue = FuncBlock->codegen()){
			Builder.CreateRet(RetValue);
			verifyFunction(*FuncDef);

			ScopedNamedValues.pop_back();
			return FuncDef;
		}

		// If there was a error within the function block codegen
		// We must revert this changes. 
		ScopedNamedValues.pop_back();
		FuncDef->eraseFromParent();
		return nullptr; 
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

	llvm::Value *codegen() override{
		if (FuncDecl != nullptr){
			FuncDecl->codegen();
		}

		if (VarDecl != nullptr){
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


	llvm::Value *codegen() override{
		for (auto &Extern : ExternList){
			Extern->codegen();
		}

		for (auto &Decl : DeclList){
			Decl->codegen();
		}
		return nullptr;
	}; 
};
#pragma endregion
/// =================================== !! Program & Decls END !! ================================================ ///

/// =================================== !! Literal AST Start !! ================================================ ///
#pragma region
class VoidAST : public ExprAST
{
public:
	llvm::Value *codegen() override{
		return nullptr;
	}; 
};

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

	llvm::Value *codegen() override{
		return nullptr;
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

	llvm::Value *codegen() override{
		return nullptr;
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

	llvm::Value *codegen() override{
		return nullptr;
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
		throw ParseException("Invalid Token Error: " + errMessage);
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
 * @brief Checks if the current token is a valid token for adding a layer of presedence
 *
 * @param type
 * @return true If the next token is valid token for adding a layer of presedence
 * @return false Otherwise
 */
static bool ValidPresedenceLayer(int type)
{
	bool addlayer;
	switch (type)
	{
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
	else if (CurTok.type == RPAR)
	{
		auto voidArg = std::make_unique<VoidAST>();

		args.push_back(std::move(voidArg));
	}
	else
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
static std::vector<std::unique_ptr<ExprAST>> Rval_Ident_Prime()
{
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
		throw ParseException("Invalid Token Error: \nExpected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation. ");
	}
	return args;
}

// rval_ident ::= IDENT rval_ident_prime | rval_term
static std::unique_ptr<ExprAST> Rval_Ident()
{
	std::unique_ptr<ExprAST> expr;
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
		TOKEN ident = GetIdentAndMatch();

		auto args = Rval_Ident_Prime();

		// Since identifer could be function call or variable
		// We must identify first if it is a function call or variable
		// Then create the correct node depending on which
		if (args.size() == 0)
		{
			expr = std::make_unique<Variable>(std::move(ident));
		}
		else
		{
			expr = std::make_unique<FuncCallAST>(std::move(ident), std::move(args));
		}

		break;
	}
	default:
		throw ParseException("Invalid Token Error: \nExpected: Variable or Literal. ");
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
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type))
	{
		LHS = Rval_Neg();
		expr = Rval_Mul_Prime(std::move(LHS));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}

	return expr;
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
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type))
	{
		LHS = Rval_Mul();
		expr = Rval_Add_Prime(std::move(LHS));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}
	return expr;
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
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type))
	{
		LHS = Rval_Add();
		expr = Rval_Cmp_Prime(std::move(LHS));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}
	return expr;
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
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type))
	{
		LHS = Rval_Cmp();
		expr = Rval_Eq_Prime(std::move(LHS));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}

	return expr;
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
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type))
	{
		LHS = Rval_Eq();
		expr = Rval_And_Prime(std::move(LHS));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of Expression. ");
	}

	return expr;
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
	std::unique_ptr<ExprAST> LHS;
	std::unique_ptr<ExprAST> expr;

	if (ValidPresedenceLayer(CurTok.type))
	{
		LHS = Rval_And();
		expr = Rval_Or_Prime(std::move(LHS));
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Expression. ");
	}

	return expr;
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

		// switch (NextTok.type)
		// {
		// 	case ASSIGN:
		// 	{
		// 		TOKEN ident = GetIdentAndMatch();
		// 		Match(ASSIGN, "Expected '=' after variable identifer. ");

		// 		auto var_expr = Expr();
		// 		expr = std::make_unique<VariableAssignmentAST>(std::move(ident), std::move(var_expr));
		// 		break;
		// 	}
		// 	case BOOL_LIT:
		// 	case FLOAT_LIT:
		// 	case INT_LIT:
		// 	case LPAR:
		// 	case NOT:
		// 	case MINUS:
		// 	case IDENT:
		// 	{
		// 		expr = Rval_Or();
		// 		break;
		// 	}
		// 	default:
		// 		std::cout << CurTok.lexeme << " CUR TOK " << CurTok.lineNo << std::endl;
		// 		std::cout << NextTok.lexeme << " NEXT TOK " << NextTok.lineNo << std::endl;
		// 		throw ParseException("Invalid Token Error: \nExpected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC}");
		// }

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

	if (ValidExprStart())
	{
		expr = Expr();
		Match(SC, "Expected ';' after return expression. ");
	}
	else if (CurTok.type == SC)
	{
		Match(SC, "Expected ';' after return keyword. ");
	}
	else
	{
		throw ParseException("Invalid Token Error: \nExpected: Start of expression. ");
	}

	return std::make_unique<ReturnAST>(std::move(expr));
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

		//AST Printer
		// root->to_string("", "Program", false);
		 
	}
	catch (const exception &e)
	{
		cout << e.what() << endl
			 << "Got: " << CurTok.lexeme << " (Type: " << CurTok.type << ") line: " << CurTok.lineNo  << " col: " << CurTok.columnNo<< endl;
		exit(0);
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



	try{
		root->codegen();
	}
	catch (const exception &e)
	{
		cout << e.what() << endl
			 << "Got: " << CurTok.lexeme << " (Type: " << CurTok.type << ") line: " << CurTok.lineNo  << " col: " << CurTok.columnNo<< endl;
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
	if (Warnings.size() != 0){
		std::cout << "Warnings: " << std::endl; 
		int warningCnt = 0;
		for (auto &Warn : Warnings){
			std::cout << std::to_string(++warningCnt) << ": " << Warn.to_string() << std::endl;
		}
	}

	// TheModule->print(dest, nullptr);
	//********************* End printing final IR ****************************

	fclose(pFile); // close the file that contains the code that was parsed
	return 0;
}
