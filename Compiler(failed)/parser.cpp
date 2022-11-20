#include "parser.hpp"

TOKEN CurTok;
std::deque<TOKEN> tok_buffer;

#pragma region

// arg_list_prime ::= "," expr arg_list_prime | epsilon
void Arg_List_Prime(std::vector<std::unique_ptr<ExprAST>> &args)
{
	if (CurTok.type == RPAR)
	{
		return;
	}

	if (CurTok.type == COMMA)
	{
		Match(COMMA, "Expected: ',' after argument");
		auto arg = Expr();
		args.push_back(std::move(arg));

		Arg_List_Prime(args);
	}
	else
	{
		throw ParseException("Expected: End of Function Call or New Argument.", CurTok.lineNo, CurTok.columnNo);
	}
}

// arg_list ::= expr arg_list_prime
std::vector<std::unique_ptr<ExprAST>> Arg_List()
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
		throw ParseException("Expected: Function Argument.", CurTok.lineNo, CurTok.columnNo);
	}

	return args;
}

// args ::= arg_list |  epsilon
std::vector<std::unique_ptr<ExprAST>> Args()
{
	std::vector<std::unique_ptr<ExprAST>> args;

	if (ValidExprStart())
	{
		args = Arg_List();
	}
	else if (CurTok.type != RPAR)
	{
		throw ParseException("Expected: Function Argument or End of Function Call.", CurTok.lineNo, CurTok.columnNo);
	}

	return args;
}

// rval_term ::= INT_LIT | FLOAT_LIT | BOOL_LIT
std::unique_ptr<ExprAST> Rval_Term()
{
	std::unique_ptr<ExprAST> expr;
	TOKEN lit_tok = CurTok;

	switch (CurTok.type)
	{
	case BOOL_LIT:
	{
		Match(BOOL_LIT, "Expected: bool literal.");

		expr = std::make_unique<BoolAST>(std::move(lit_tok));
		break;
	}
	case FLOAT_LIT:
	{
		Match(FLOAT_LIT, "Expected: float literal.");

		expr = std::make_unique<FloatAST>(std::move(lit_tok));
		break;
	}
	case INT_LIT:
	{
		Match(INT_LIT, "Expected: int literal.");

		expr = std::make_unique<IntegerAST>(std::move(lit_tok));
		break;
	}
	default:
		throw ParseException("Expected: Literal.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// rval_ident_prime ::= epsilon | "(" args ")"
std::unique_ptr<ExprAST> Rval_Ident_Prime(TOKEN ident)
{
	std::vector<std::unique_ptr<ExprAST>> args;
	std::unique_ptr<ExprAST> expr;

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
		expr = std::make_unique<VariableAST>(std::move(ident));
		break;
	case LPAR:
	{
		Match(LPAR, "Expected: '(' before function call.");
		args = Args();
		Match(RPAR, "Expected: ')' after function call.");

		expr = std::make_unique<FuncCallAST>(std::move(ident), std::move(args));
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// rval_ident ::= IDENT rval_ident_prime | rval_term
std::unique_ptr<ExprAST> Rval_Ident()
{
	std::unique_ptr<ExprAST> expr;

	if (CurTok.type == BOOL_LIT || CurTok.type == FLOAT_LIT || CurTok.type == INT_LIT)
	{
		expr = Rval_Term();
	}
	else if (CurTok.type == IDENT)
	{
		TOKEN ident = GetIdentAndMatch();

		expr = Rval_Ident_Prime(std::move(ident));
	}
	else
	{
		throw ParseException("Expected: Variable, Literal or Function Call.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// rval_par ::= "(" expr ")" | rval_ident
std::unique_ptr<ExprAST> Rval_Par()
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
		Match(LPAR, "Expected: '(' before expression.");
		expr = Expr();
		Match(RPAR, "Expected: ')' after expression.");
		break;
	}
	default:
		throw ParseException("Expected: Start of Expression.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// rval_neg ::= "-" rval_neg | "!" rval_neg | rval_par
std::unique_ptr<ExprAST> Rval_Neg()
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
		Match(NOT, "Expected: '!' operator.");

		auto expr = Rval_Neg();

		unary_expr = std::make_unique<UnaryExprAST>(std::move(Op_Token), std::move(expr));
		break;
	}
	case MINUS:
	{
		Match(MINUS, "Expected: '-' operator.");

		auto expr = Rval_Neg();

		unary_expr = std::make_unique<UnaryExprAST>(std::move(Op_Token), std::move(expr));
		break;
	}
	default:
		throw ParseException("Expected: Start of Expression.", CurTok.lineNo, CurTok.columnNo);
	}

	return unary_expr;
}

// rval_mul_prime ::= "*" rval_neg  | "/" rval_neg  | "%" rval_neg | epsilon
std::unique_ptr<ExprAST> Rval_Mul_Prime(std::unique_ptr<ExprAST> LHS)
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
		Match(MOD, "Expected: '%' operator.");

		LHS_Prime = Rval_Neg();
		RHS = Rval_Mul_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case DIV:
	{
		Match(DIV, "Expected: '/' operator.");

		LHS_Prime = Rval_Neg();
		RHS = Rval_Mul_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case ASTERIX:
	{
		Match(ASTERIX, "Expected: '*' operator.");

		LHS_Prime = Rval_Neg();
		RHS = Rval_Mul_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}
	return expr;
}

// rval_mul ::= rval_neg rval_mul_prime
std::unique_ptr<ExprAST> Rval_Mul()
{
	std::unique_ptr<ExprAST> LHS = Rval_Neg();

	while (CurTok.type == ASTERIX || CurTok.type == MOD || CurTok.type == DIV){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Neg();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}
	
	return Rval_Mul_Prime(std::move(LHS));
}

// rval_add_prime ::= "+" rval_mul  | "-" rval_mul | epsilon
std::unique_ptr<ExprAST> Rval_Add_Prime(std::unique_ptr<ExprAST> LHS)
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
		Match(PLUS, "Expected: '+' operator.");

		LHS_Prime = Rval_Mul();
		RHS = Rval_Add_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case MINUS:
	{
		Match(MINUS, "Expected: '-' operator.");

		LHS_Prime = Rval_Mul();
		RHS = Rval_Add_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}
	return expr;
}

// rval_add ::= rval_mul rval_add_prime
std::unique_ptr<ExprAST> Rval_Add()
{
	std::unique_ptr<ExprAST> LHS = Rval_Mul();
	
	while (CurTok.type == PLUS || CurTok.type == MINUS){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Mul();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Add_Prime(std::move(LHS));
}

// rval_cmp_prime ::= "<=" rval_add | "<" rval_add | ">=" rval_add | ">" rval_add | epsilon
std::unique_ptr<ExprAST> Rval_Cmp_Prime(std::unique_ptr<ExprAST> LHS)
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
		Match(LT, "Expected: '<' operator.");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case GT:
	{
		Match(GT, "Expected: '>' operator.");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case LE:
	{
		Match(LE, "Expected: '<=' operator.");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case GE:
	{
		Match(GE, "Exepcted: '>=' operator.");

		LHS_Prime = Rval_Add();
		RHS = Rval_Cmp_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}
	return expr;
}

// rval_cmp ::= rval_add rval_cmp_prime
std::unique_ptr<ExprAST> Rval_Cmp()
{
	std::unique_ptr<ExprAST> LHS = Rval_Add();
	
	while (CurTok.type == LT || CurTok.type == LE || CurTok.type == GT || CurTok.type == GE){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Add();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Cmp_Prime(std::move(LHS));
}

// rval_eq_prime ::= "==" rval_cmp | "!=" rval_cmp | epsilon
std::unique_ptr<ExprAST> Rval_Eq_Prime(std::unique_ptr<ExprAST> LHS)
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
		Match(EQ, "Expected: '==' operator.");

		LHS_Prime = Rval_Cmp();
		RHS = Rval_Eq_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	case NE:
	{
		Match(NE, "Expected:'!=' operator.");

		LHS_Prime = Rval_Cmp();
		RHS = Rval_Eq_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// rval_eq ::= rval_cmp rval_eq_prime
std::unique_ptr<ExprAST> Rval_Eq()
{
	std::unique_ptr<ExprAST> LHS = Rval_Cmp();
	
	while (CurTok.type == EQ || CurTok.type == NE){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Cmp();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Eq_Prime(std::move(LHS));
}

// rval_and_prime ::= "&&" rval_eq rval_and_prime | epsilon
std::unique_ptr<ExprAST> Rval_And_Prime(std::unique_ptr<ExprAST> LHS)
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
		Match(AND, "Expected: '&&' operator.");

		LHS_Prime = Rval_Eq();
		RHS = Rval_And_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}
	return expr;
}

// rval_and ::= rval_eq rval_and_prime
std::unique_ptr<ExprAST> Rval_And()
{
	std::unique_ptr<ExprAST> LHS = Rval_Eq();

	while (CurTok.type == AND){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_Eq();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}
	
	return Rval_And_Prime(std::move(LHS));
}

// rval_or_prime ::= "||" rval_and rval_or_prime | epsilon
std::unique_ptr<ExprAST> Rval_Or_Prime(std::unique_ptr<ExprAST> LHS)
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
		Match(OR, "Expected: '||' operator.");

		LHS_Prime = Rval_And();
		RHS = Rval_Or_Prime(std::move(LHS));

		expr = std::make_unique<BinaryExprAST>(std::move(Op_Token), std::move(LHS_Prime), std::move(RHS));
		break;
	}
	default:
		throw ParseException("Expected: One Of [',' New Argument, ')' End of Arguments, ';' End of Expression] or Operation.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// rval_or ::= rval_and rval_or_prime
std::unique_ptr<ExprAST> Rval_Or()
{
	std::unique_ptr<ExprAST> LHS = Rval_And();
	
	while (CurTok.type == OR){
		TOKEN op = CurTok;
		getNextToken();

		auto RHS = Rval_And();

		LHS = std::make_unique<BinaryExprAST>(op, std::move(LHS), std::move(RHS));
	}

	return Rval_Or_Prime(std::move(LHS));
}

// expr ::= IDENT "=" expr | rval_or
std::unique_ptr<ExprAST> Expr()
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

		if (NextTok.type == ASSIGN)
		{
			TOKEN ident = GetIdentAndMatch();

			Match(ASSIGN, "Expected: '=' after variable identifer.");

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
		Match(SC, "Expected: ';'.");
		break;
	}
	default:
		throw ParseException("Expected: Assignment or Start of Expression.", CurTok.lineNo, CurTok.columnNo);
	}
	return expr;
}

// return_stmt_prime ::= ";" | expr ";"
std::unique_ptr<ReturnAST> Return_Stmt_Prime()
{
	std::unique_ptr<ExprAST> expr;
	TOKEN returnTok; 
	if (ValidExprStart())
	{
		expr = Expr();

		returnTok = CurTok;
		Match(SC, "Expected: ';' after return expression.");
	}
	else if (CurTok.type == SC)
	{
		returnTok = CurTok;
		Match(SC, "Expected: ';' after return keyword.");
	}
	else
	{
		throw ParseException("Expected: Start of expression.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<ReturnAST>(std::move(expr), std::move(returnTok));
}

// return_stmt ::= "return" return_stmt_prime
std::unique_ptr<ReturnAST> Return_Stmt()
{
	std::unique_ptr<ReturnAST> return_stmt;

	if (CurTok.type == RETURN)
	{
		Match(RETURN, "Expected: 'return' keyword.");
		return_stmt = Return_Stmt_Prime();
	}
	else
	{
		throw ParseException("Expected: 'return' keyword.", CurTok.lineNo, CurTok.columnNo);
	}

	return return_stmt;
}

// else_stmt  ::= "else" block | epsilon
std::unique_ptr<BlockAST> Else_Stmt()
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
		Match(ELSE, "Expected: 'else' keyword after if statement.");
		else_block = Block();
		break;
	}
	default:
		throw ParseException("Expected: Else block of IF Statment or New Valid Statement.", CurTok.lineNo, CurTok.columnNo);
	}

	return else_block;
}

// if_stmt ::= "if" "(" expr ")" block else_stmt
std::unique_ptr<IfAST> If_Stmt()
{
	std::unique_ptr<ExprAST> condition_expr;
	std::unique_ptr<BlockAST> true_block;
	std::unique_ptr<BlockAST> else_block;

	if (CurTok.type == IF)
	{
		Match(IF, "Expected: 'if' keyword.");
		Match(LPAR, "Expected: '(' before if condition.");

		condition_expr = Expr();

		Match(RPAR, "Expected: ')' after if condition.");

		true_block = Block();
		else_block = Else_Stmt();
	}
	else
	{
		throw ParseException("Expected: 'if' keyword.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<IfAST>(std::move(condition_expr), std::move(true_block), std::move(else_block));
}

// while_stmt ::= "while" "(" expr ")" stmt
std::unique_ptr<WhileAST> While_Stmt()
{
	std::unique_ptr<ExprAST> condition_expr;
	std::unique_ptr<StmtAST> loop_block;

	if (CurTok.type == WHILE)
	{
		Match(WHILE, "Expected: 'While' keyword.");
		Match(LPAR, "Expected: '(' before loop condition.");

		condition_expr = Expr();

		Match(RPAR, "Expected: ')' after loop condition.");

		loop_block = Stmt();
	}
	else
	{
		throw ParseException("Expected: 'while' keyword.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<WhileAST>(std::move(condition_expr), std::move(loop_block));
}

// expr_stmt ::= expr ";" | ";"
std::unique_ptr<ExprAST> Expr_Stmt()
{
	std::unique_ptr<ExprAST> expr;
	if (ValidExprStart())
	{
		expr = Expr();
		Match(SC, "Expected: ';' after expression.");
	}
	else if (CurTok.type == SC)
	{
		Match(SC, "Expected: ';' after expression.");
	}
	else
	{
		throw ParseException("Expected: Start of expression.", CurTok.lineNo, CurTok.columnNo);
	}

	return expr;
}

// stmt ::= expr_stmt |  block |  if_stmt |  while_stmt |  return_stmt
std::unique_ptr<StmtAST> Stmt()
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
		throw ParseException("Expected: One Of [IfStatment, WhileLoop, ReturnStmt, '{' (Start of New Block), Expression].", CurTok.lineNo, CurTok.columnNo);
	}

	return stmt;
}

// stmt_list ::= stmt stmt_list | epsilon
void Stmt_List(std::vector<std::unique_ptr<StmtAST>> &stmt_list)
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

		if (stmt)
		{
			stmt_list.push_back(std::move(stmt));
		}

		Stmt_List(stmt_list);
		break;
	}
	case RBRA:
		break;
	default:
		throw ParseException("Expected: New Statement, Cannot declare variables after a statement.", CurTok.lineNo, CurTok.columnNo);
	}
}

// local_decl ::= var_type IDENT ";"
std::unique_ptr<VariableDeclAST> Local_Decl()
{
	VAR_TYPE type;
	TOKEN ident;

	if (ValidType())
	{
		type = Var_Type();
		ident = GetIdentAndMatch();

		Match(SC, "Expected ';' after variable decleration.");
	}
	else
	{
		throw ParseException("Expected: Type decleration.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<VariableDeclAST>(std::move(ident), std::move(type));
}

// local_decls ::= local_decl local_decls | epsilon
void Local_Decls(std::vector<std::unique_ptr<VariableDeclAST>> &variable_decls)
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
		throw ParseException("Expected: {BOOL_LIT, FLOAT_LIT, INT_LIT, LPAR, IDENT, NOT, MINUS, SC, RETURN, IF, WHILE, RBRA, LBRA, BOOL_TOK, FLOAT_TOK, INT_TOK}", CurTok.lineNo, CurTok.columnNo);
	}
}

// block ::= "{" local_decls stmt_list "}"
std::unique_ptr<BlockAST> Block()
{
	std::vector<std::unique_ptr<VariableDeclAST>> variable_decls;
	std::vector<std::unique_ptr<StmtAST>> stmt_list;

	if (CurTok.type == LBRA)
	{
		Match(LBRA, "Expected: '{' to declare new scope.");

		Local_Decls(variable_decls);
		Stmt_List(stmt_list);

		Match(RBRA, "Expected: '}' after statement.");
	}
	else
	{
		throw ParseException("Expected: '{' to declare new scope.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<BlockAST>(std::move(variable_decls), std::move(stmt_list));
}

// param ::= var_type IDENT
std::unique_ptr<ParamAST> Param()
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
		throw ParseException("Expected: Parameter type decleration.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<ParamAST>(std::move(ident), std::move(type));
}

// param_list_prime ::= "," param param_list_prime | epsilon
void Param_List_Prime(std::vector<std::unique_ptr<ParamAST>> &param_list)
{
	if (CurTok.type == RPAR)
	{
		return;
	}

	if (CurTok.type == COMMA)
	{
		Match(COMMA, "Expected: ',' or ')' after function parameter");
		auto param = Param();
		param_list.push_back(std::move(param));

		Param_List_Prime(param_list);
	}
	else
	{
		throw ParseException("Expected: ',' or ')' in function paramters.", CurTok.lineNo, CurTok.columnNo);
	}
}

// param_list ::= param param_list_prime
std::vector<std::unique_ptr<ParamAST>> Param_List()
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
		throw ParseException("Expected: Paramter Type.", CurTok.lineNo, CurTok.columnNo);
	}

	return param_list;
}

// params ::= param_list |  "void" | epsilon
std::vector<std::unique_ptr<ParamAST>> Params()
{
	std::vector<std::unique_ptr<ParamAST>> param_list;

	if (CurTok.type == RPAR)
	{
		return param_list;
	}

	if (CurTok.type == VOID_TOK)
	{
		Match(VOID_TOK, "Expected: 'void' token in function paramters");
	}
	else if (ValidType())
	{
		param_list = Param_List();
	}
	else
	{
		throw ParseException("Expected: End of Paramters or Paramter Type.", CurTok.lineNo, CurTok.columnNo);
	}

	return param_list;
}

// var_type  ::= "int" |  "float" |  "bool"
VAR_TYPE Var_Type()
{
	VAR_TYPE type;

	if (CurTok.type == BOOL_TOK)
	{
		Match(BOOL_TOK, "Expected: 'bool' keyword.");
		type = BOOL_TYPE;
	}
	else if (CurTok.type == FLOAT_TOK)
	{
		Match(FLOAT_TOK, "Expected: 'float' keyword.");
		type = FLOAT_TYPE;
	}
	else if (CurTok.type == INT_TOK)
	{
		Match(INT_TOK, "Expected: 'int' keyword.");
		type = INT_TYPE;
	}
	else
	{
		throw ParseException("Expected: Variable type decleration.", CurTok.lineNo, CurTok.columnNo);
	}

	return type;
}

// type_spec ::= "void" | var_type
VAR_TYPE Type_Spec()
{
	VAR_TYPE type;

	if (CurTok.type == VOID_TOK)
	{
		Match(VOID_TOK, "Expected: 'void' keyword.");
		type = VOID_TYPE;
	}
	else if (ValidType())
	{
		type = Var_Type();
	}
	else
	{
		throw ParseException("Expected: Type decleration.", CurTok.lineNo, CurTok.columnNo);
	}

	return type;
}

// decl_prime ::= ";" | "(" params ")" block
void Decl_Prime(std::unique_ptr<FuncDeclAST> &func_decl, std::unique_ptr<VariableDeclAST> &var_decl, VAR_TYPE type, TOKEN ident)
{

	if (CurTok.type == LPAR)
	{
		Match(LPAR, "Expected: '(' after function decleration.");

		auto params = Params();

		Match(RPAR, "Expected: ')' after function paramters.");

		auto block = Block();

		func_decl = std::make_unique<FuncDeclAST>(std::move(ident), std::move(type), std::move(params), std::move(block));
	}
	else if (CurTok.type == SC)
	{
		Match(SC, "Expected: ';' after variable decleration.");

		var_decl = std::make_unique<VariableDeclAST>(std::move(ident), std::move(type));
	}
	else
	{
		throw ParseException("Expected: ';' or Function Paramters.", CurTok.lineNo, CurTok.columnNo);
	}
}

// decl ::= var_type IDENT decl_prime | "void" IDENT "(" params ")" block
std::unique_ptr<DeclAST> Decl()
{
	std::unique_ptr<FuncDeclAST> func_decl;
	std::unique_ptr<VariableDeclAST> var_decl;

	if (CurTok.type == VOID_TOK)
	{
		Match(VOID_TOK, "Expected: 'void' token before function decleration.");

		TOKEN ident = GetIdentAndMatch();

		Match(LPAR, "Expected: '(' after function identifer.");

		auto params = Params();

		Match(RPAR, "Expected: ')' after parameter list.");

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
		throw ParseException("Expected: Variable or Function type decleration.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<DeclAST>(std::move(func_decl), std::move(var_decl));
}

// decl_list_prime ::= decl decl_list_prime | epsilon
void Decl_List_Prime(std::vector<std::unique_ptr<DeclAST>> &decl_list)
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
		throw ParseException("Expected: decleration type or end_of_file.", CurTok.lineNo, CurTok.columnNo);
	}
}

// decl_list ::= decl decl_list_prime
std::vector<std::unique_ptr<DeclAST>> Decl_List()
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
		throw ParseException("Expected: Variable or Function type decleration.", CurTok.lineNo, CurTok.columnNo);
	}

	return decl_list;
};

// extern ::= "extern" type_spec IDENT "(" params ")" ";"
std::unique_ptr<FuncDeclAST> Extern()
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

		Match(LPAR, "Expected: '(' after identifer keyword.");
		params = Params();
		Match(RPAR, "Expected: ')' after function paramters.");

		Match(SC, "Expected: ';' after function definition.");
	}
	else
	{
		throw ParseException("Expected: 'extern' keyword.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<FuncDeclAST>(std::move(ident), std::move(type), std::move(params), std::move(emptyblock));
}

// extern_list_prime ::= extern extern_list_prime | epsilon
void Extern_List_Prime(std::vector<std::unique_ptr<FuncDeclAST>> &extern_list)
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
		throw ParseException("Expected: 'extern' keyword or extern function type.", CurTok.lineNo, CurTok.columnNo);
	}
}

// extern_list ::= extern extern_list_prime
std::vector<std::unique_ptr<FuncDeclAST>> Extern_List()
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
		throw ParseException("Expected: 'extern' Keyword.", CurTok.lineNo, CurTok.columnNo);
	}

	return extern_list;
};

// program ::= extern_list decl_list | decl_list
std::unique_ptr<ProgramAST> Program()
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
		throw ParseException("Expected: Extern declerations or Function declerations.", CurTok.lineNo, CurTok.columnNo);
	}

	return std::make_unique<ProgramAST>(std::move(extern_list), std::move(decl_list));
}

#pragma endregion
