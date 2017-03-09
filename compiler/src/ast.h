#pragma once

#include <tree/ParseTreeVisitor.h>

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

}; // ns ast
