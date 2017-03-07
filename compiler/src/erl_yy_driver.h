#pragma once

#include <map>
#include <string>
#include <memory>

#if !defined(yyFlexLexerOnce)
  #include <FlexLexer.h>
#endif

#include "yy/erlang_parser.hpp"
#include "yy/location.hh"

YY_DECL;

extern int yy_init; // whether we need to initialize
extern int yy_start;
extern std::istream& yyin;
extern std::ostream& yyout;

using ErlangParser = yy::BaseErlangParser;

class ErlangLexer : public yyFlexLexer {
 public:
  explicit ErlangLexer(std::istream* in) : yyFlexLexer(in) {
    loc = new ErlangParser::location_type();
  };

  // get rid of override virtual function warning
  using FlexLexer::yylex;

  // YY_DECL for this is defined in erlang.ll
  // Method body is created by flex in erlang_lexer.cpp
  virtual ErlangParser::symbol_type lex(
//      ErlangParser::semantic_type* const lval,
//      ErlangParser::location_type* location,
      ErlangDriver& driver);

 private:
  ErlangParser::semantic_type* yylval = nullptr;
  ErlangParser::location_type* loc = nullptr;
};

// Conducting the whole scanning and parsing of Calc++.
class ErlangDriver {
  std::map<std::string, int> variables_;

  int result_ = 0;
  bool trace_scanning_;

  // Whether parser traces should be generated.
  bool trace_parsing_;

 public:
  // The name of the file being parsed.
  // Used later to pass the file name to the location tracker.
  std::string file_;
  std::unique_ptr<ErlangLexer> lexer_;
  std::unique_ptr<ErlangParser> parser_;

 public:
  ErlangDriver();
  virtual ~ErlangDriver();

  void set_trace_parsing() { trace_parsing_ = true; }
  void set_trace_scanning() { trace_scanning_ = true; }
  int get_result() const { return result_; }

  // Handling the scanner.
  void scan_begin();
  void scan_end();

  // Run the parser on file F.
  // Return 0 on success.
  int parse(const std::string& f);

  // Error handling.
  void error(const yy::location& l, const std::string& m);
  void error(const std::string& m);
};
