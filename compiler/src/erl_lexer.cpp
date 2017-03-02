#include "erl_lexer.h"

//#include <cctype>
//#include <cstdio>

// std::string ident_str_;  // Filled in if Identifier
// double d_literal_;              // Filled in if DoubleLit

/// gettok - Return the next token from standard input.
int Tokenizer::get_tok() {
  // Skip any whitespace.
  while (isspace(last_char_))
    last_char_ = getchar();

  if (isalpha(last_char_)) {  // identifier: [a-zA-Z][a-zA-Z0-9]*
    ident_str_ = last_char_;
    while (isalnum((last_char_ = getchar())))
      ident_str_ += last_char_;

    if (ident_str_ == "def") {
      return Token::FunDef;
    } else if (ident_str_ == "extern") {
      return Token::Extern;
    } else if (ident_str_ == "if") {
      return Token::If;
    } else if (ident_str_ == "then") {
      return Token::Then;
    } else if (ident_str_ == "else") {
      return Token::Else;
    } else if (ident_str_ == "for") {
      return Token::For;
    } else if (ident_str_ == "in") {
      return Token::In;
    }

    if (isupper(ident_str_[0])) { return Token::Identifier; }
    return Token::Atom;
  }

  if (isdigit(last_char_) || last_char_ == '.') {  // Number: [0-9.]+
    std::string num_s;
    do {
      num_s += last_char_;
      last_char_ = getchar();
    } while (isdigit(last_char_) || last_char_ == '.');

    d_literal_ = strtod(num_s.c_str(), nullptr);
    return Token::DoubleLit;
  }

  if (last_char_ == '#') {
    // Comment until end of line.
    do
      last_char_ = getchar();
    while (last_char_ != EOF && last_char_ != '\n' && last_char_ != '\r');

    if (last_char_ != EOF)
      return get_tok();
  }

  // Check for end of file.  Don't eat the EOF.
  if (last_char_ == EOF)
    return Token::EndOfFile;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = last_char_;
  last_char_ = getchar();
  return ThisChar;
}
