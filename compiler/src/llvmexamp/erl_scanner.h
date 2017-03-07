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
