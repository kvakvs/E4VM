#include "ast.h"
#include "antlr4/ErlangParser.h"

namespace ast {

//antlrcpp::Any CSTVisitor::visit(antlr4::tree::ParseTree* tree) {
//  if (tree->children.size() == 0) {
//    auto term = dynamic_cast<antlr4::tree::TerminalNode*>(tree);
//    if (term) {
//      visitTerminal(term);
//    }
//    // NULL term means optional syntax element which matched nothing
//  }
//  return visitChildren(tree);
//}
//
//antlrcpp::Any CSTVisitor::visitChildren(antlr4::tree::ParseTree* node) {
//  for(auto c: node->children) {
//    visit(c);
//  }
//  return antlrcpp::Any();
//}
//
//antlrcpp::Any CSTVisitor::visitTerminal(antlr4::tree::TerminalNode* node) {
//  auto sym = node->getSymbol();
//  auto node_str = node->getText();
//  printf("TERM %s [%zu]\n", node_str.c_str(), sym->getType());
//  switch (sym->getType()) {
//    case ErlangParser::TokAtom: {
//      add_node(new Atom());
//    } break;
//    default:
//      ::printf("Unhandled token type %zu\n", sym->getType());
//      ::exit(1);
//  }
//  return antlrcpp::Any();
//}
//
//antlrcpp::Any CSTVisitor::visitErrorNode(antlr4::tree::ErrorNode* node) {
//  return antlrcpp::Any();
//}

void ASTBuilderVisitor::add_node(ast::INode* n) {
  if (not tree_) {
    tree_.reset(n);
  } else {
    tree_->add_child(n);
  }
}

antlrcpp::Any
ASTBuilderVisitor::visitFunction(ErlangParser::FunctionContext* ctx) {
  printf("FUN %s\n", ctx->getText().c_str());
  return Super::visitFunction(ctx);
}

} // ns ast
