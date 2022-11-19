#include "common.hpp"

//llvm helper functions 
#ifndef LLVM_HELPER_H
#define LLVM_HELPER_H

llvm::Value *GetConstant(VAR_TYPE type, float Val, bool isSigned = false);
llvm::Type *TypeToLLVM(VAR_TYPE type, TOKEN tokInfo);
llvm::Type *GetHighestPrecisionType(llvm::Type *t1, llvm::Type *t2);
const std::string llvmTypeToStr(llvm::Type *type);
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction, const std::string &VarName, llvm::Type *type);
llvm::Value *GetBool(llvm::Value *val, llvm::Type *type, std::string loopStr);

template <typename T>
llvm::Value *CheckDefinedAndLoad(T alloca, llvm::Type *llvmType, TOKEN Ident);
llvm::Value *ImplicitCasting(llvm::Value *val, llvm::Type *newType, TOKEN tokInfo, std::string optionalError = "");


#endif 