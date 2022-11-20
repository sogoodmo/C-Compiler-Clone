//llvm helper functions 
#ifndef CODEGEN_HELPER_H
#define CODEGEN_HELPER_H

#include "common.hpp"

llvm::Value *GetConstant(VAR_TYPE type, float Val, bool isSigned);
llvm::Type *TypeToLLVM(VAR_TYPE type, TOKEN tokInfo);
llvm::Type *GetHighestPrecisionType(llvm::Type *t1, llvm::Type *t2);
const std::string llvmTypeToStr(llvm::Type *type);
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction, const std::string &VarName, llvm::Type *type);
llvm::Value *GetBool(llvm::Value *val, llvm::Type *type, std::string loopStr);

template <typename T>
llvm::Value *CheckDefinedAndLoad(T alloca, llvm::Type *llvmType, TOKEN Ident);
llvm::Value *ImplicitCasting(llvm::Value *val, llvm::Type *newType, TOKEN tokInfo, std::string optionalError);


#endif 