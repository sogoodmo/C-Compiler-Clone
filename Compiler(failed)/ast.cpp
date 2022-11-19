#include "ast.hpp"

#include "codegen_helper.cpp"


/// ASTnode - Base class for all AST nodes.
class ASTnode
{
public:
	virtual ~ASTnode() {}
	virtual llvm::Value *codegen() = 0;
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) const {};
};


class StmtAST : public ASTnode{};
class ExprAST : public StmtAST{};

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
			llvm::GlobalVariable *globalAlloca = GlobalVariables.at(Ident.lexeme);
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

			auto G = new llvm::GlobalVariable(*(TheModule.get()), llvmType, false, llvm::GlobalValue::CommonLinkage, llvm::Constant::getNullValue(llvmType), Ident.lexeme);

			UndefinedVars.insert(Ident.lexeme);
			GlobalVariables.insert({Ident.lexeme, G});
		}
		else
		{
			if (mapContainsKey(ScopedNamedValues.back(), Ident.lexeme))
			{
				throw SemanticException("Cannot redeclare variable within same scope.", Ident.lineNo, Ident.columnNo);
			}

			llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

			llvm::AllocaInst *alloca = CreateEntryBlockAlloca(TheFunction, Ident.lexeme, llvmType);
			Builder.CreateStore(llvm::Constant::getNullValue(llvmType), alloca);

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

			llvm::GlobalVariable *globalAlloca = GlobalVariables.at(Ident.lexeme);

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
#pragma region

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

		return GetConstant(VAR_TYPE::BOOL_TYPE, 1.0f, false);
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

			/**
			 * If the current statement is a return statement, throw a warning only if it's not the last statement in a block
			 *
			 * Generate the statement and stop generating any more IR- as this code would never be reached
			 */

			auto returnStmt = dynamic_cast<ReturnAST *>(stmt.get());
			if (returnStmt != nullptr)
			{

				if (stmtListIdx != StmtList.size() - 1)
				{
					int lineno = returnStmt->getRetTok().lineNo;
					int colno = returnStmt->getRetTok().columnNo;

					Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Return statement will cause it's following lines to not be executed", lineno, colno));
				}

				stmt->codegen();

				if (addScope)
				{
					ScopedNamedValues.pop_back();
				}

				// Just returning any non-null value if we're at a return stmt
				return GetConstant(VAR_TYPE::BOOL_TYPE, 1, false);
			}
			else
			{
				containsReturn = stmt->codegen();
			}
		}

		if (addScope)
		{
			ScopedNamedValues.pop_back();
		}

		return containsReturn;
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

		llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
		llvm::Value *CondValue = ConditionExpr->codegen();
		llvm::Type *CondType = CondValue->getType();

		CondValue = GetBool(CondValue, CondType, "ifcond");

		llvm::BasicBlock *TrueBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
		llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else");
		llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(TheContext, "ifcont");

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
		else
		{
			IfPathsReturn = false;
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

		llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

		llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "while");
		llvm::BasicBlock *ContBB = llvm::BasicBlock::Create(TheContext, "whilecont");
		llvm::BasicBlock *HeaderBB = llvm::BasicBlock::Create(TheContext, "loopcond", TheFunction);

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

		// Branch back to checking conditional branch
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

		/**
		 * If we are creating an expression for && or || we can try to apply lazy evaluation
		 *
		 * We can create basic blocks, and branch to either evaluating the RHS or just continuing the block.
		 * Depending on the outcome of the LHS.
		 *
		 * We just need to store the value in the temporary alloca
		 *
		 *
		 * If we have (True || ...) -> We don't have to evaluate RHS. So we can branch to the continue block
		 * If we have (False && ...) -> We don't have to evalaute RHS. So we can branch to the continue block÷
		 */

		switch (Op.type)
		{
		case OR:
			return CheckLazyOr(Op, std::move(LHS), std::move(RHS));
		case AND:
			return CheckLazyAnd(Op, std::move(LHS), std::move(RHS));
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
		llvm::Function *CalleeFunc = TheModule->getFunction(FuncName.lexeme);

		if (!CalleeFunc)
		{
			throw SemanticException("Unknown Function Referenced. Perhaps you have misspelt your function.", FuncName.lineNo, FuncName.columnNo);
		}

		if (Args.size() != CalleeFunc->arg_size())
		{
			throw SemanticException("Invalid Argument Size. Expected " + std::to_string(CalleeFunc->arg_size()) + ". Got " + std::to_string(Args.size()), FuncName.lineNo, FuncName.columnNo);
		}

		std::vector<llvm::Value *> ArgsV;
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
	 * @brief Generate prototype if function doesn't already exist, and add a new-level of scope
	 *
	 * Also perform checks to see if all code paths in a function contain a return in a non-void function.
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

		llvm::Function *ExternFuncDef = TheModule->getFunction(Ident.lexeme);

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
				FT = llvm::FunctionType::get(TypeToLLVM(Type, Ident), false);
			}
			else
			{
				FT = llvm::FunctionType::get(TypeToLLVM(Type, Ident), Args, false);
			}

			FuncDef = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Ident.lexeme, TheModule.get());

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

		llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", FuncDef);
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
		if (IfPathsReturn && !FuncContainsReturn)
		{
			if (!IfStmtLast)
			{
				Warnings.push_back(Warning("\033[0;33mWarning:\033[0m Any code after if-statement may not be reachable in Function: " + Ident.lexeme, Ident.lineNo, Ident.columnNo));
			}

			if (FuncReturnType->isVoidTy())
			{
				Builder.CreateRetVoid();
			}
			else if (FuncReturnType->isFloatTy())
			{
				Builder.CreateRet(GetConstant(VAR_TYPE::FLOAT_TYPE, 0.0f));
			}
			else if (FuncReturnType->isIntegerTy(1))
			{
				Builder.CreateRet(GetConstant(VAR_TYPE::BOOL_TYPE, 0.0f, false));
			}
			else if (FuncReturnType->isIntegerTy(32))
			{
				Builder.CreateRet(GetConstant(VAR_TYPE::INT_TYPE, 0.0f, true));
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
		return GetConstant(VAR_TYPE::INT_TYPE, std::stof(Val.lexeme), true);
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
		return GetConstant(VAR_TYPE::FLOAT_TYPE, std::stof(Val.lexeme));
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
		float boolVal = (Val.lexeme == "false" ? 0.0f : 1.0f);

		return GetConstant(VAR_TYPE::BOOL_TYPE, boolVal, false);
	};
};
#pragma endregion
/// =================================== !! Literal AST End !! ================================================ ///




/**
 * @brief Creates 3 branches when checking LazyAND. Right Branch, Skip Right Branch, Continue Branch
 *
 * We also create a temp alloca
 *
 * If the LHS Expression evaluates to FALSE -> We jump to the skip right branch which sets the value of
 * of this temp alloca to FALSE.
 *
 * If the LHS Expression evalautes to TRUE -> We jump to the right branch and set the value of this alloca
 * to the value of the RHS
 *
 * Then we return the value of this alloca
 *
 * @param Op Token info for error info
 * @param LHS LHS Of Bin Expression
 * @param RHS RHS Of Bin Expression
 * @return llvm::Value*
 */
llvm::Value *CheckLazyAnd(TOKEN Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
{
	llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

	llvm::AllocaInst *tmpAlloca = CreateEntryBlockAlloca(TheFunction, "tmpLazy", TypeToLLVM(BOOL_TYPE, Op));

	llvm::BasicBlock *RightBB = llvm::BasicBlock::Create(TheContext, "RExpr", TheFunction);
	llvm::BasicBlock *SkipRightBB = llvm::BasicBlock::Create(TheContext, "SkipRExpr");
	llvm::BasicBlock *ContBB = llvm::BasicBlock::Create(TheContext, "Cont");

	llvm::Value *BoolRHS;
	llvm::Value *BoolLHS = ImplicitCasting(LHS->codegen(), TypeToLLVM(BOOL_TYPE, Op), Op);

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

	Builder.CreateStore(GetConstant(BOOL_TYPE, 0.0f, false), tmpAlloca);

	Builder.CreateBr(ContBB);

	TheFunction->getBasicBlockList().push_back(ContBB);
	Builder.SetInsertPoint(ContBB);

	return Builder.CreateLoad(TypeToLLVM(BOOL_TYPE, Op), tmpAlloca, "exprBool");
}

/**
 * @brief Creates 3 branches when checking LazyOR. Right Branch, Skip Right Branch, Continue Branch
 *
 * We also create a temp alloca
 *
 * If the LHS Expression evaluates to TRUE -> We jump to the skip right branch which sets the value of
 * of this temp alloca to TRUE.
 *
 * If the LHS Expression evalautes to FALSE -> We jump to the right branch and set the value of this alloca
 * to the value of the RHS
 *
 * Then we return the value of this alloca
 *
 * @param Op Token info for error info
 * @param LHS LHS Of Bin Expression
 * @param RHS RHS Of Bin Expression
 * @return llvm::Value*
 */
llvm::Value *CheckLazyOr(TOKEN Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
{
	llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

	llvm::AllocaInst *tmpAlloca = CreateEntryBlockAlloca(TheFunction, "tmpLazy", TypeToLLVM(BOOL_TYPE, Op));

	llvm::BasicBlock *RightBB = llvm::BasicBlock::Create(TheContext, "RExpr", TheFunction);
	llvm::BasicBlock *SkipRightBB = llvm::BasicBlock::Create(TheContext, "SkipRExpr");
	llvm::BasicBlock *ContBB = llvm::BasicBlock::Create(TheContext, "Cont");

	llvm::Value *BoolRHS;
	llvm::Value *BoolLHS = ImplicitCasting(LHS->codegen(), TypeToLLVM(BOOL_TYPE, Op), Op);

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

	Builder.CreateStore(GetConstant(BOOL_TYPE, 1.0f, false), tmpAlloca);

	Builder.CreateBr(ContBB);

	TheFunction->getBasicBlockList().push_back(ContBB);
	Builder.SetInsertPoint(ContBB);

	return Builder.CreateLoad(TypeToLLVM(BOOL_TYPE, Op), tmpAlloca, "exprBool");
}