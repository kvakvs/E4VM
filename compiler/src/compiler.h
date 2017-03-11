#pragma once

#include <ANTLRFileStream.h>
#include "antlr4/ErlangLexer.h"
#include "antlr4/ErlangParser.h"
#include "ast.h"
#include "antlr4/ErlangBaseListener.h"

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

class Compiler;

class ErlangErrorListener : public antlr4::BaseErrorListener {
private:
  Compiler* compiler_;
public:
  ErlangErrorListener(Compiler* comp) : compiler_(comp) {
  }

  virtual void syntaxError(antlr4::Recognizer* recognizer,
                           antlr4::Token* offendingSymbol,
                           size_t line, size_t charPositionInLine,
                           const std::string& msg,
                           std::exception_ptr e) override;
};

class Compiler {
private:
  std::unique_ptr<ErlangLexer1> lexer_;
  std::unique_ptr<antlr4::CommonTokenStream> token_stream_;
  std::unique_ptr<ErlangParser> parser_;
  std::unique_ptr<ErlangErrorListener> err_listener_;
  antlr4::tree::ParseTree* parse_tree_;
  ast::INode::Ptr ast_tree_;

  std::string filename_;
  bool is_error_ = false;

public:
  bool process(const char *filename);

  const std::string& get_filename() const { return filename_; }

  void set_error() { is_error_ = true; }
};

} // ns erl
