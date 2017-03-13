#include "compiler.h"
#include "ast_builder.h"
#include "codegen.h"

namespace erl {

bool Compiler::process(const char* filename) {
  filename_ = filename;
  antlr4::ANTLRFileStream f_stream(filename);
  lexer_ = std::make_unique<ErlangLexer1>(&f_stream);

  token_stream_ = std::make_unique<antlr4::CommonTokenStream>(lexer_.get());
  token_stream_->fill();

  //  for (auto token : tokens.getTokens()) {
  //    std::cout << token->toString() << std::endl;
  //  }

  parser_ = std::make_unique<ErlangParser>(token_stream_.get());
  err_listener_ = std::make_unique<ErlangErrorListener>(this);
  parser_->removeErrorListeners();
  parser_->addErrorListener(err_listener_.get());
  parse_tree_ = parser_->forms();

  if (is_error_) {
    return false;
  }

//  std::cout << parse_tree_->toStringTree(parser_.get()) << std::endl;

//  erl::ASTBuilder ast_b;
//  ast_b.visit(parse_tree_);
//  ast_tree_ = std::move(ast_b.tree_);
  ast_tree_ = erl::build_ast(parse_tree_);

  erl::Codegen cg;
  cg.init_module_and_pass_manager("test1");

  if (ast_tree_) {
    ast_tree_->codegen(cg);
  }
  return true;
}

void ErlangErrorListener::syntaxError(antlr4::Recognizer* /*recognizer*/ ,
                                      antlr4::Token* /*offendingSymbol*/ ,
                                      size_t line, size_t charPositionInLine,
                                      const std::string& msg,
                                      std::exception_ptr /*e*/ ) {
  std::cerr
      << "ERR " << compiler_->get_filename() << " "
      << line << ":" << charPositionInLine << " -> "
      << msg << std::endl;
  compiler_->set_error();
}

} // ns erl
