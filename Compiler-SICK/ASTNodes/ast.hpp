//ast.hpp
#ifndef AST_H
#define AST_H

#include "../General/common.hpp"

/// ASTnode - Base class for all AST nodes.
class ASTnode
{
public:
	virtual ~ASTnode() {}
	virtual llvm::Value *codegen() = 0;
	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) {};
};

class StmtAST : public ASTnode{};
class ExprAST : public StmtAST{};

/// =================================== !! Variable's START !! ================================================ ///
#pragma region

class VariableAST : public ExprAST
{
	TOKEN Ident;

public:
	VariableAST(TOKEN Ident)
		: Ident(std::move(Ident)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override; 

	llvm::Value *codegen() override;
};

class VariableDeclAST : public ASTnode
{
	VAR_TYPE Type;
	TOKEN Ident;

public:
	VariableDeclAST(TOKEN Ident, VAR_TYPE Type)
		: Ident(std::move(Ident)), Type(std::move(Type)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};

class VariableAssignmentAST : public ExprAST
{
	TOKEN Ident;
	std::unique_ptr<ExprAST> Expr;

public:
	VariableAssignmentAST(TOKEN Ident, std::unique_ptr<ExprAST> Expr)
		: Ident(std::move(Ident)), Expr(std::move(Expr)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;


	llvm::Value *codegen() override;

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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};

class IfAST : public StmtAST
{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<BlockAST> TrueBlock;
	std::unique_ptr<BlockAST> ElseBlock;

public:
	IfAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<BlockAST> TrueBlock, std::unique_ptr<BlockAST> ElseBlock)
		: ConditionExpr(std::move(ConditionExpr)), TrueBlock(std::move(TrueBlock)), ElseBlock(std::move(ElseBlock)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};

class WhileAST : public StmtAST
{
	std::unique_ptr<ExprAST> ConditionExpr;
	std::unique_ptr<StmtAST> LoopBlock;

public:
	WhileAST(std::unique_ptr<ExprAST> ConditionExpr, std::unique_ptr<StmtAST> LoopBlock)
		: ConditionExpr(std::move(ConditionExpr)), LoopBlock(std::move(LoopBlock)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;

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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};

class UnaryExprAST : public ExprAST
{
	TOKEN Op;
	std::unique_ptr<ExprAST> Expr;

public:
	UnaryExprAST(TOKEN Op, std::unique_ptr<ExprAST> Expr)
		: Op(std::move(Op)), Expr(std::move(Expr)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;


	llvm::Value *codegen() override;
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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;

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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Function *codegen() override;
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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;

};

class ProgramAST : public ASTnode
{
	std::vector<std::unique_ptr<FuncDeclAST>> ExternList;
	std::vector<std::unique_ptr<DeclAST>> DeclList;

public:
	ProgramAST(std::vector<std::unique_ptr<FuncDeclAST>> ExternList, std::vector<std::unique_ptr<DeclAST>> DeclList)
		: ExternList(std::move(ExternList)), DeclList(std::move(DeclList)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;

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

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};

class FloatAST : public ExprAST
{
	TOKEN Val;

public:
	FloatAST(TOKEN Val)
		: Val(std::move(Val)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};

class BoolAST : public ExprAST
{
	TOKEN Val;

public:
	BoolAST(TOKEN Val)
		: Val(std::move(Val)) {}

	virtual void to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft) override;

	llvm::Value *codegen() override;
};
#pragma endregion
/// =================================== !! Literal AST End !! ================================================ ///


/// =================================== !! Lazy Eval Functions !! ================================================ ///
llvm::Value *CheckLazyAnd(TOKEN Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS);
llvm::Value *CheckLazyOr(TOKEN Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS);

#endif