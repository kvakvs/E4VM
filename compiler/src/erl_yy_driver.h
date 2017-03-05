#pragma once

#include <map>
#include <string>
#include <memory>

#include "yy/erlang_parser.hpp"
#include "yy/location.hh"
#include "erl_scanner.h"

// Tell Flex the lexer's prototype ...
#undef YY_DECL
#define YY_DECL yy::ErlangParser::symbol_type yylex(ErlangDriver& driver)
YY_DECL;

extern int yy_init; // whether we need to initialize
extern int yy_start;
//extern std::istream* yyin;

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
  std::unique_ptr<ErlangScanner> lexer_;

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
