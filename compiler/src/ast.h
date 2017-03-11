#pragma once

#include <tree/ParseTreeVisitor.h>
#include "antlr4/ErlangParser.h"

namespace ast {

class INode {
public:
  using Ptr = std::shared_ptr<INode>;
private:
  std::vector<Ptr> children_;
public:
  virtual ~INode() {}

  void add_child(INode::Ptr&& n) {
    children_.push_back(std::move(n));
  }
};

class Atom : public INode {
public:
  virtual ~Atom() {}
};

class FunctionClause {
  std::vector<INode::Ptr> args_;
public:
  void add_arg(INode::Ptr&& ast) {
    args_.push_back(std::move(ast));
  }
};

class Function : public INode {
private:
  std::string name_;
  std::vector<std::unique_ptr<FunctionClause>> clauses_;
public:
  explicit Function(const std::string& name) : name_(name) {
  }

  virtual ~Function() {}

  void add_clause(std::unique_ptr<FunctionClause>&& clause) {
    clauses_.push_back(std::move(clause));
  }

  const std::string& get_name() const {
    return name_;
  }
};

}; // ns ast
