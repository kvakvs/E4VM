#pragma once

#include <tree/ParseTreeVisitor.h>
#include "antlr4/ErlangParser.h"

namespace erl {
  class Codegen;
} // ns erl

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

  virtual void codegen(erl::Codegen& cg) = 0;
};

//class Atom: public INode {
//public:
//  virtual ~Atom() {}
//};

class FunctionClause {
  std::vector<INode::Ptr> args_;

public:
  void add_arg(INode::Ptr&& ast) {
    args_.push_back(std::move(ast));
  }

  void codegen_clause(erl::Codegen& cg);
  int arity() const {
    return static_cast<int>(args_.size());
  }
};

class Function : public INode {
private:
  std::string name_;
  int arity_ = -1;
  std::vector<std::unique_ptr<FunctionClause>> clauses_;

public:
  explicit Function(const std::string& name) : name_(name) {
  }

  virtual ~Function() {}

  int arity() const { return arity_; }
  const std::string& name() const { return name_; }

  virtual void codegen(erl::Codegen& cg) override;

  void add_clause(std::unique_ptr<FunctionClause>&& clause);

  const std::string& get_name() const {
    return name_;
  }
};

}; // ns ast
