#include "compiler.h"

namespace erl {

class ParseTreeVisitor: public antlr4::tree::ParseTreeVisitor {
public:
  std::unique_ptr<ast::INode> ast_tree_;
  ParseTreeVisitor() {}

  virtual antlrcpp::Any visit(antlr4::tree::ParseTree *tree) override {
    if (tree->children.size() == 0) {
      return visitTerminal((antlr4::tree::TerminalNode*)tree);
    }
    return visitChildren(tree);
  }

  virtual antlrcpp::Any visitChildren(antlr4::tree::ParseTree *node) override {
    for(auto c: node->children) {
      visit(c);
    }
    return antlrcpp::Any();
  }

  virtual antlrcpp::Any visitTerminal(antlr4::tree::TerminalNode *node) override {
    auto sym = node->getSymbol();
    auto node_str = node->getText();
    printf("TERM %s [%zu]\n", node_str.c_str(), sym->getType());
    switch (sym->getType()) {
      case ErlangParser::TokAtom: {
        ast_node(new ast::Atom());
      } break;
      default:
        printf("Unhandled token type %zu\n", sym->getType());
//        ::exit(1);
    }
    return antlrcpp::Any();
  }
  virtual antlrcpp::Any visitErrorNode(antlr4::tree::ErrorNode *node) override {
    return antlrcpp::Any();
  }

  void ast_node(ast::INode* n) {
    if (not ast_tree_) {
      ast_tree_.reset(n);
    } else {
      ast_tree_->add_child(n);
    }
  }
};

void Compiler::process(const char* filename) {
  antlr4::ANTLRFileStream f_stream(filename);
  lexer_ = std::make_unique<ErlangLexer1>(&f_stream);

  token_stream_ = std::make_unique<antlr4::CommonTokenStream>(lexer_.get());
  token_stream_->fill();

  //  for (auto token : tokens.getTokens()) {
  //    std::cout << token->toString() << std::endl;
  //  }

  parser_ = std::make_unique<ErlangParser>(token_stream_.get());
  parse_tree_ = parser_->forms();

//  std::cout << parse_tree_->toStringTree(parser_.get()) << std::endl;

  ParseTreeVisitor ptv;
  ptv.visit(parse_tree_);
  ast_tree_ = std::move(ptv.ast_tree_);
}

} // ns erl
