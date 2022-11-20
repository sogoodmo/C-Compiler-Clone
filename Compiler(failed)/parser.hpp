// parser.hpp
#ifndef PARSER_H
#define PARSER_H

#include "parser_helper.hpp"
#include "ast.hpp"


void Arg_List_Prime(std::vector<std::unique_ptr<ExprAST>> &args);
std::vector<std::unique_ptr<ExprAST>> Arg_List();
std::vector<std::unique_ptr<ExprAST>> Args();
std::unique_ptr<ExprAST> Rval_Term();
std::vector<std::unique_ptr<ExprAST>> Rval_Ident_Prime();
std::unique_ptr<ExprAST> Rval_Ident();
std::unique_ptr<ExprAST> Rval_Par();
std::unique_ptr<ExprAST> Rval_Neg();
std::unique_ptr<ExprAST> Rval_Mul_Prime(std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> Rval_Mul();
std::unique_ptr<ExprAST> Rval_Add_Prime(std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> Rval_Add();
std::unique_ptr<ExprAST> Rval_Cmp_Prime(std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> Rval_Cmp();
std::unique_ptr<ExprAST> Rval_Eq_Prime(std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> Rval_Eq();
std::unique_ptr<ExprAST> Rval_And_Prime(std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> Rval_And();
std::unique_ptr<ExprAST> Rval_Or_Prime(std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> Rval_Or();
std::unique_ptr<ExprAST> Expr();
std::unique_ptr<ReturnAST> Return_Stmt_Prime();
std::unique_ptr<ReturnAST> Return_Stmt();
std::unique_ptr<BlockAST> Else_Stmt();
std::unique_ptr<IfAST> If_Stmt();
std::unique_ptr<WhileAST> While_Stmt();
std::unique_ptr<ExprAST> Expr_Stmt();
std::unique_ptr<StmtAST> Stmt();
void Stmt_List(std::vector<std::unique_ptr<StmtAST>> &stmt_list);
std::unique_ptr<VariableDeclAST> Local_Decl();
void Local_Decls(std::vector<std::unique_ptr<VariableDeclAST>> &variable_decls);
std::unique_ptr<BlockAST> Block();
std::unique_ptr<ParamAST> Param();
void Param_List_Prime(std::vector<std::unique_ptr<ParamAST>> &param_list);
std::vector<std::unique_ptr<ParamAST>> Param_List();
std::vector<std::unique_ptr<ParamAST>> Params();
VAR_TYPE Var_Type();
VAR_TYPE Type_Spec();
void Decl_Prime(std::unique_ptr<FuncDeclAST> &func_decl, std::unique_ptr<VariableDeclAST> &var_decl, VAR_TYPE type, const std::string &ident);
std::unique_ptr<DeclAST> Decl();
void Decl_List_Prime(std::unique_ptr<DeclAST> &decl_list);
std::vector<std::unique_ptr<DeclAST>> Decl_List();
std::unique_ptr<FuncDeclAST> Extern();
void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list);
std::vector<std::unique_ptr<FuncDeclAST>> Extern_List();
std::unique_ptr<ProgramAST> Program();

#endif 