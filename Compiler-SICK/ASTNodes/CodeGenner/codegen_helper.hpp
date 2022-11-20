//llvm helper functions 
#ifndef CODEGEN_HELPER_H
#define CODEGEN_HELPER_H

#include "../../General/common.hpp"

llvm::Value *GetConstant(VAR_TYPE type, float Val, bool isSigned);
llvm::Type *TypeToLLVM(VAR_TYPE type, TOKEN tokInfo);
llvm::Type *GetHighestPrecisionType(llvm::Type *t1, llvm::Type *t2);
const std::string llvmTypeToStr(llvm::Type *type);
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction, const std::string &VarName, llvm::Type *type);
llvm::Value *GetBool(llvm::Value *val, llvm::Type *type, std::string loopStr);
llvm::Value *ImplicitCasting(llvm::Value *val, llvm::Type *newType, TOKEN tokInfo, std::string optionalError);



// Defining templated functions in Header

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
};
/**
 * @brief Simple function to check if a map contains a key
 * 
 * @tparam K Generic key of map 
 * @tparam V Generic value of map
 * @param map The map of generic key, value types
 * @param key The generic key in the map 
 * @return true If key found in map 
 * @return false If key not in map 
 */
template <typename K, typename V>
bool mapContainsKey(std::unordered_map<K, V> &map, K key)
{
	if (map.find(key) == map.end())
		return false;
	return true;
}

#endif