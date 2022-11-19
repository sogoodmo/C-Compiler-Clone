#include "codgen_helper.hpp"


/**
 * @brief Get the constant llvm value from a type and value
 *
 * @param type The type of the constant
 * @param Val The value of the constant
 * @param signed If the constant is signed or not
 * @return llvm::Value*
 */
llvm::Value *GetConstant(VAR_TYPE type, float Val, bool isSigned = false)
{
	// Since float is highest precision type
	// Downcasting won't lose any information.
	if (type != FLOAT_TYPE)
	{
		Val = (int)Val;
	}

	switch (type)
	{
	case INT_TYPE:
		return llvm::ConstantInt::get(TheContext, llvm::APInt(32, Val, isSigned));
	case FLOAT_TYPE:
		return llvm::ConstantFP::get(TheContext, llvm::APFloat(Val));
	case BOOL_TYPE:
		return llvm::ConstantInt::get(TheContext, llvm::APInt(1, Val, isSigned));
	default:
		throw SemanticException("", -1, -1);
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
		return llvm::Type::getVoidTy(TheContext);
	case INT_TYPE:
		return llvm::IntegerType::getInt32Ty(TheContext);
	case FLOAT_TYPE:
		return llvm::Type::getFloatTy(TheContext);
	case BOOL_TYPE:
		return llvm::IntegerType::getInt1Ty(TheContext);
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
		return llvm::Type::getFloatTy(TheContext);
	}
	else if (t1->isIntegerTy(32) || t2->isIntegerTy(32))
	{
		return llvm::IntegerType::getInt32Ty(TheContext);
	}
	else if (t2->isIntegerTy(1) || t2->isIntegerTy(1))
	{
		return llvm::IntegerType::getInt1Ty(TheContext);
	}

	throw SemanticException("Unexpected Type.", -1, -1);
}

/**
 * @brief Converts the LLVM Type into human readable string.
 *
 * @param type
 * @return const std::string
 */
const std::string llvmTypeToStr(llvm::Type *type)
{
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
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction, const std::string &VarName, llvm::Type *type)
{
	llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(type, 0, VarName.c_str());
}

/**
 * @brief Convert the expression from it's original type to a boolean type
 *
 * @param val Value of the expressiom
 * @param type Type of the value of the expression
 * @return llvm::Value*
 */
llvm::Value *GetBool(llvm::Value *val, llvm::Type *type, std::string loopStr)
{
	if (type->isFloatTy())
	{
		return Builder.CreateFCmpONE(val, GetConstant(VAR_TYPE::FLOAT_TYPE, 0.0f), loopStr);
	}
	else if (type->isIntegerTy(32))
	{
		return Builder.CreateICmpNE(val, GetConstant(VAR_TYPE::INT_TYPE, 0.0f, true), loopStr);
	}
	else if (type->isIntegerTy(1))
	{
		return Builder.CreateOr(val, GetConstant(VAR_TYPE::BOOL_TYPE, 0.0f, false), loopStr);
	}

	throw SemanticException("", -1, -1);
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
llvm::Value *ImplicitCasting(llvm::Value *val, llvm::Type *newType, TOKEN tokInfo, std::string optionalError = "")
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
		return Builder.CreateFCmpONE(val, GetConstant(VAR_TYPE::FLOAT_TYPE, 0.0f), tokInfo.lexeme.c_str());
	}

	// Int -> Bool Conversion - Precision Loss
	if (oldType->isIntegerTy(32) && newType->isIntegerTy(1))
	{
		Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Implict conversion from Int32 to Bool. May lose precision" + optionalError, tokInfo.lineNo, tokInfo.columnNo));
		return Builder.CreateICmpNE(val, GetConstant(VAR_TYPE::INT_TYPE, 0, true), tokInfo.lexeme.c_str());
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