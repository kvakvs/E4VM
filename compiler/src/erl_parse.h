#pragma once

#include <map>

#include "erl_ast.h"
#include "erl_scanner.h"

class Parser {
  int cur_tok_ = ' ';
  Tokenizer& tokenizer_;

 public:
  std::map<char, int> binop_prio_;

  Parser(Tokenizer& tok) : tokenizer_(tok) {}

  int get_next_token() { return cur_tok_ = tokenizer_.get_tok(); }
  int get_cur_tok() const { return cur_tok_; }

 public:
  std::unique_ptr<ast::FunctionAST> parse_top_level_expr();
  std::unique_ptr<ast::FunctionAST> parse_definition();
  std::unique_ptr<ast::FunPrototype> parse_extern();

 private:
  int GetTokPrecedence();
  std::unique_ptr<ast::IExpr> parse_number_expr();
  std::unique_ptr<ast::IExpr> parse_parenthesized_expr();
  std::unique_ptr<ast::IExpr> parse_identifier_expr();
  std::unique_ptr<ast::IExpr> parse_if_expr();
  std::unique_ptr<ast::IExpr> parse_for_expr();
  std::unique_ptr<ast::IExpr> parse_primary();
  std::unique_ptr<ast::IExpr> parse_binop_rhs(int ExprPrec,
                                              std::unique_ptr<ast::IExpr> lhs);
  std::unique_ptr<ast::FunPrototype> parse_fun_proto();
  std::unique_ptr<ast::IExpr> parse_expr();

  std::unique_ptr<ast::IExpr> error(const char* Str);
  std::unique_ptr<ast::FunPrototype> error_p(const char* Str);
};
