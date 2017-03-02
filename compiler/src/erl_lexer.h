#pragma once

#include <string>

/*
  '(' ')' ',' '->' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
  'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
  'andalso' 'orelse'
  'bnot' 'not'
  '*' '/' 'div' 'rem' 'band' 'and'
  '+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
  '++' '--'
  '==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<=' '=>' ':='
  '<<' '>>'
  '!' '=' '::' '..' '...'
  'spec' 'callback' % helper
  dot.
 */

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
namespace Token {
  enum {
    EndOfFile = -1,

    // commands
    FunDef = -100,
    Extern = -101,

    // primary
    Identifier = -200,
    Atom = -201,
    DoubleLit = -202,
    IntegerLit = -203,

    // control
    If = -300,
    Then = -301,
    Else = -302,
    For = -303,
    In = -304
  };
} // ns

class Tokenizer {
  std::string ident_str_;   // Filled in if Identifier
  double d_literal_ = 0.0;  // Filled in if DoubleLit
  int last_char_ = ' ';     // Used in get_tok()

 public:
  Tokenizer(): ident_str_() {}

  int get_tok();

  double get_double_val() const { return d_literal_; }
  const std::string& get_identifier_str() const { return ident_str_; }
};
