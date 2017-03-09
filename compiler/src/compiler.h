#pragma once

#include <ANTLRFileStream.h>
#include "antlr4/ErlangLexer.h"
#include "antlr4/ErlangParser.h"
#include "ast.h"

namespace erl {

class ErlangLexer1 : public ErlangLexer {
public:
  explicit ErlangLexer1(antlr4::CharStream* input) :
      ErlangLexer(input) {}

  virtual const std::vector<std::string>& getChannelNames() const override {
    return channels_;
  }

private:
  std::vector<std::string> channels_;
};

class Compiler {
private:
  std::unique_ptr<ErlangLexer1> lexer_;
  std::unique_ptr<antlr4::CommonTokenStream> token_stream_;
  std::unique_ptr<ErlangParser> parser_;
  antlr4::tree::ParseTree* parse_tree_;
  std::unique_ptr<ast::INode> ast_tree_;

public:
  void process(const char *filename);
};

} // ns erl
