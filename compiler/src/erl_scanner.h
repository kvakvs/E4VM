#pragma once

#include <string>

#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "yy/erlang_parser.hpp"

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
namespace Token {
typedef enum {
  EndOfFile = 0,
  Dot,
  Comma,

  // commands
  FunDef,
  Extern,

  // primary
  Identifier,
  Atom,
  DoubleLit,
  IntegerLit,

  If,
  Then,
  Else,
  For,
  In,

  Plus,
  Minus,
  Mult,
  FDiv,
  IDiv4
} Type;
}  // ns

// LLVM Example
class Tokenizer {
  std::string ident_str_;   // Filled in if Identifier
  double d_literal_ = 0.0;  // Filled in if DoubleLit
  int last_char_ = ' ';     // Used in get_tok()

 public:
  Tokenizer() : ident_str_() {}

  int get_tok();

  double get_double_val() const { return d_literal_; }
  const std::string& get_identifier_str() const { return ident_str_; }
};

class ErlangScanner : public yyFlexLexer {
  using ErlangParser = yy::ErlangParser;

 public:
  explicit ErlangScanner(std::istream* in) : yyFlexLexer(in) {
    loc = new ErlangParser::location_type();
  };

  // get rid of override virtual function warning
  using FlexLexer::yylex;

  // YY_DECL for this is defined in erlang.ll
  // Method body is created by flex in erlang_lexer.cpp
  virtual ErlangParser::symbol_type yylex(
      ErlangParser::semantic_type* const lval,
      ErlangParser::location_type* location);

 private:
  ErlangParser::semantic_type* yylval = nullptr;
  ErlangParser::location_type* loc = nullptr;
};
