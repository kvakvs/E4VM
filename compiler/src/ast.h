#pragma once

#include <tree/ParseTreeVisitor.h>
#include "antlr4/ErlangBaseVisitor.h"

namespace ast {

class INode {
  std::vector<INode*> children_;
public:
  void add_child(INode* n) {
    children_.push_back(n);
  }
};

class Atom: public INode {
};

///-----------------------------------------------------------------------------

class ASTBuilderVisitor: public ::ErlangBaseVisitor {
public:
  using Super = ::ErlangBaseVisitor;
  std::unique_ptr<INode> tree_;

  virtual antlrcpp::Any
  visitFunction(ErlangParser::FunctionContext *ctx) override;

private:
  void add_node(ast::INode* n);
};

}; // ns ast
