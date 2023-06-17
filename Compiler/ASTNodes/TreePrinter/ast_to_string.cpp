#include "../ast.hpp"

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

// Program & Decl
#pragma region
void ProgramAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
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

void DeclAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
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
#pragma endregion

// Functions  
#pragma region
void ParamAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << nodeStr << TypeToStr(Type) << " " << Ident.lexeme << std::endl;
};

void FuncDeclAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
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

void FuncCallAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "FuncCall: " << FuncName.lexeme << std::endl;

    for (int i = 0; i < Args.size(); i++)
    {
        Args[i]->to_string(prefix + (isLeft ? "│   " : "    "), "", (i != Args.size() - 1));
    }
};
#pragma endregion

// Variables 
#pragma region
void VariableAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "Variable: " << Ident.lexeme << std::endl;
};

void VariableDeclAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << nodeStr << TypeToStr(Type) << " " << Ident.lexeme << std::endl;
};

void VariableAssignmentAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "AssignExpr: " << Ident.lexeme << " =" << std::endl;

    Expr->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
};
#pragma endregion

// Statements
#pragma region
void BlockAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
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

void IfAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
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
void WhileAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
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

void ReturnAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "Return" << std::endl;

    if (ReturnExpr != nullptr)
    {
        ReturnExpr->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
    }
};
#pragma endregion

// Expressions
#pragma region
void BinaryExprAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "BinaryExpr: " << Op.lexeme << std::endl;

    LHS->to_string(prefix + (isLeft ? "│   " : "    "), "", true);
    RHS->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
};

void UnaryExprAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "UnaryExpr: " << Op.lexeme << std::endl;

    Expr->to_string(prefix + (isLeft ? "│   " : "    "), "", false);
};

#pragma endregion

// Literals
#pragma region
void IntegerAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "Int Literal: " << Val.lexeme << std::endl;
};

void FloatAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "Float Literal: " << Val.lexeme << std::endl;
};

void BoolAST::to_string(const std::string &prefix, const std::string &nodeStr, bool isLeft)
{
    std::cout << prefix;

    std::cout << (isLeft ? "├──" : "└──");

    std::cout << "Bool Literal: " << Val.lexeme << std::endl;
};
#pragma endregion