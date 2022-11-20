#ifndef COMMON_H
#define COMMON_H

#include "token.hpp"
#include "exceptions.hpp"


extern llvm::LLVMContext TheContext;
extern llvm::IRBuilder<> Builder;
extern std::unique_ptr<llvm::Module> TheModule;

extern std::string filename; 
extern std::ifstream filereader;
/**
 * Contains a vector of maps- Where the index of the unordered_map indicates it's level of scope
 *
 * A block may access variables in a scope prior to it in the array, but not after it.
 *
 * If a variable is defined multiple times
 */
extern std::vector<std::unordered_map<std::string, llvm::AllocaInst *>> ScopedNamedValues;
extern std::unordered_map<std::string, llvm::GlobalVariable *> GlobalVariables;
extern std::unordered_set<std::string> UndefinedVars;

extern bool IfPathsReturn;
extern bool IfStmtLast;

// Global variable to check if adding a new scope is due to a function, or braces.
extern bool isFuncBlock;

extern std::vector<Warning> Warnings;

extern std::string IdentifierStr; // Filled in if IDENT
extern int IntVal;				  // Filled in if INT_LIT
extern bool BoolVal;			  // Filled in if BOOL_LIT
extern float FloatVal;			  // Filled in if FLOAT_LIT
extern std::string StringVal;	  // Filled in if String Literal
extern int lineNo, columnNo;

extern TOKEN CurTok;
extern std::deque<TOKEN> tok_buffer;

extern FILE *pFile;

enum VAR_TYPE
{
	VOID_TYPE = 0,
	INT_TYPE,
	FLOAT_TYPE,
	BOOL_TYPE
};

#endif