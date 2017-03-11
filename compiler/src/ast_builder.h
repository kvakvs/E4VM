#pragma once

#include "ast.h"
#include "antlr4/ErlangBaseVisitor.h"

namespace erl {

class ASTBuilder: public ::ErlangBaseVisitor {
public:
  using Super = ::ErlangBaseVisitor;
  ast::INode::Ptr tree_;

  virtual antlrcpp::Any
  visitFunction(ErlangParser::FunctionContext *ctx) override;

private:
  void add_node(ast::INode::Ptr&& n);
};

// Helper function which given parse tree builds AST from it
ast::INode::Ptr&& build_ast(antlr4::tree::ParseTree* tree);

} // ns erl