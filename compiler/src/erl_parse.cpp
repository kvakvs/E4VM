#include "erl_parse.h"
#include "erl_lexer.h"

#include <llvm/ADT/STLExtras.h>

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ast::IExpr> Parser::error(const char* Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<ast::FunPrototype> Parser::error_p(const char* Str) {
  error(Str);
  return nullptr;
}

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int Parser::GetTokPrecedence() {
  if (!isascii(cur_tok_))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = binop_prio_[cur_tok_];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

/// numberexpr ::= number
std::unique_ptr<ast::IExpr> Parser::parse_number_expr() {
  auto Result = llvm::make_unique<ast::DoubleLit>(tokenizer_.get_double_val());
  get_next_token();  // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
std::unique_ptr<ast::IExpr> Parser::parse_parenthesized_expr() {
  get_next_token();  // eat (.
  auto V = parse_expr();
  if (!V)
    return nullptr;

  if (cur_tok_ != ')')
    return error("expected ')'");
  get_next_token();  // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
std::unique_ptr<ast::IExpr> Parser::parse_identifier_expr() {
  std::string IdName = tokenizer_.get_identifier_str();

  get_next_token();  // eat identifier.

  if (cur_tok_ != '(')  // Simple variable ref.
    return llvm::make_unique<ast::Variable>(IdName);

  // Call.
  get_next_token();  // eat (
  std::vector<std::unique_ptr<ast::IExpr>> args_v;
  if (cur_tok_ != ')') {
    while (true) {
      if (auto a = parse_expr())
        args_v.push_back(std::move(a));
      else
        return nullptr;

      if (cur_tok_ == ')')
        break;

      if (cur_tok_ != ',')
        return error("Expected ')' or ',' in argument list");
      get_next_token();
    }
  }

  // Eat the ')'.
  get_next_token();

  return llvm::make_unique<ast::Call>(IdName, std::move(args_v));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
std::unique_ptr<ast::IExpr> Parser::parse_if_expr() {
  get_next_token();  // eat the if.

  // condition.
  auto Cond = parse_expr();
  if (!Cond)
    return nullptr;

  if (cur_tok_ != Token::Then)
    return error("expected then");
  get_next_token();  // eat the then

  auto Then = parse_expr();
  if (!Then)
    return nullptr;

  if (cur_tok_ != Token::Else)
    return error("expected else");

  get_next_token();

  auto Else = parse_expr();
  if (!Else)
    return nullptr;

  return llvm::make_unique<ast::IfCondition>(std::move(Cond), std::move(Then),
                                             std::move(Else));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
std::unique_ptr<ast::IExpr> Parser::parse_for_expr() {
  get_next_token();  // eat the for.

  if (cur_tok_ != Token::Identifier)
    return error("expected identifier after for");

  std::string IdName = tokenizer_.get_identifier_str();
  get_next_token();  // eat identifier.

  if (cur_tok_ != '=')
    return error("expected '=' after for");
  get_next_token();  // eat '='.

  auto Start = parse_expr();
  if (!Start)
    return nullptr;
  if (cur_tok_ != ',')
    return error("expected ',' after for start value");
  get_next_token();

  auto End = parse_expr();
  if (!End)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ast::IExpr> Step;
  if (cur_tok_ == ',') {
    get_next_token();
    Step = parse_expr();
    if (!Step)
      return nullptr;
  }

  if (cur_tok_ != Token::In)
    return error("expected 'in' after for");
  get_next_token();  // eat 'in'.

  auto Body = parse_expr();
  if (!Body)
    return nullptr;

  return llvm::make_unique<ast::ForLoop>(IdName, std::move(Start),
                                         std::move(End), std::move(Step),
                                         std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
std::unique_ptr<ast::IExpr> Parser::parse_primary() {
  switch (cur_tok_) {
    default:
      return error("unknown token when expecting an expression");
    case Token::Identifier:
      return parse_identifier_expr();
    case Token::DoubleLit:
      return parse_number_expr();
    case '(':
      return parse_parenthesized_expr();
    case Token::If:
      return parse_if_expr();
    case Token::For:
      return parse_for_expr();
  }
}

/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ast::IExpr>
Parser::parse_binop_rhs(int ExprPrec,
                        std::unique_ptr<ast::IExpr> lhs) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current
    // binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec) { return lhs; }

    // Okay, we know this is a binop.
    int BinOp = cur_tok_;
    get_next_token();  // eat binop

    // Parse the primary expression after the binary operator.
    auto rhs = parse_primary();
    if (!rhs) { return nullptr; }

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      rhs = parse_binop_rhs(TokPrec + 1, std::move(rhs));
      if (!rhs)
        return nullptr;
    }

    // Merge LHS/RHS.
    lhs = llvm::make_unique<ast::BinaryExpr>(BinOp, std::move(lhs),
                                             std::move(rhs));
  }
}

/// expression
///   ::= primary binoprhs
///
std::unique_ptr<ast::IExpr> Parser::parse_expr() {
  auto LHS = parse_primary();
  if (!LHS)
    return nullptr;

  return parse_binop_rhs(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
std::unique_ptr<ast::FunPrototype> Parser::parse_fun_proto() {
  if (cur_tok_ != Token::Identifier)
    return error_p("Expected function name in prototype");

  std::string FnName = tokenizer_.get_identifier_str();
  get_next_token();

  if (cur_tok_ != '(')
    return error_p("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (get_next_token() == Token::Identifier) {
    ArgNames.push_back(tokenizer_.get_identifier_str());
  }
  if (cur_tok_ != ')') {
    return error_p("Expected ')' in prototype");
  }

  // success.
  get_next_token();  // eat ')'.

  return llvm::make_unique<ast::FunPrototype>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
std::unique_ptr<ast::FunctionAST> Parser::parse_definition() {
  get_next_token();  // eat def.
  auto fp = parse_fun_proto();
  if (!fp) { return nullptr; }

  if (auto expr = parse_expr()) {
    return llvm::make_unique<ast::FunctionAST>(std::move(fp),
                                               std::move(expr));
  }
  return nullptr;
}

/// toplevelexpr ::= expression
std::unique_ptr<ast::FunctionAST> Parser::parse_top_level_expr() {
  if (auto E = parse_expr()) {
    // Make an anonymous proto.
    auto fp = llvm::make_unique<ast::FunPrototype>("__anon_expr",
                                                   std::vector<std::string>());
    return llvm::make_unique<ast::FunctionAST>(std::move(fp), std::move(E));
  }
  return nullptr;
}

/// external ::= 'extern' prototype
std::unique_ptr<ast::FunPrototype> Parser::parse_extern() {
  get_next_token();  // eat extern.
  return parse_fun_proto();
}
