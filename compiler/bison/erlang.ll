%{

#include "../erl_yy_driver.h"
#include "erlang_parser.hpp"

/* define yyterminate as this instead of NULL */
#define yyterminate() { return yy::ErlangParser::make_END_OF_FILE(*loc); }
#define YY_NO_UNISTD_H

// The location of the current token.
static yy::location loc;

//#undef YY_DECL
//#define YY_DECL yy::ErlangParser::symbol_type yylex(ErlangDriver& driver)
%}

%option c++
%option debug
%option nodefault
%option yyclass="ErlangScanner"
%option noyywrap nounput noinput batch
%option stack

%{
  // Code run each time a pattern is matched.
  # define YY_USER_ACTION  loc->columns (yyleng);
%}

t09     [0-9]
tAZ     [A-Z_]
taz     [a-z]
tAZaz   [a-zA-Z_]
tAtom   [a-zA-z0-9_@]
tBlank  [ \t]

%%

%{
  // Code run each time yylex is called.
  loc->step ();
%}

{tBlank}+   loc->step ();
[\n]+      loc->lines (yyleng); loc->step ();

"." { return yy::ErlangParser::make_DOT(*loc); }

"," { return yy::ErlangParser::make_COMMA(*loc); }

{tAZ}({tAZaz}|{t09})* {
    return yy::ErlangParser::make_VARIABLE(yytext, *loc);
}

{taz}{tAtom}*   {
    return yy::ErlangParser::make_ATOM(yytext, *loc);
}

{t09}({tAZaz}|{t09})*   {
    return yy::ErlangParser::make_INTEGER(yytext, *loc);
}

"+"   { return yy::ErlangParser::make_PLUS(*loc); }
"-"   { return yy::ErlangParser::make_MINUS(*loc); }
"*"   { return yy::ErlangParser::make_MULT(*loc); }
"/"   { return yy::ErlangParser::make_FDIV(*loc); }

.          driver.error (loc, "invalid character");
<<EOF>>    return yy::calcxx_parser::make_END(loc);
%%

void
calcxx_driver::scan_begin() {
  yy_flex_debug = trace_scanning;
  if (file.empty () || file == "-") {
    yyin = stdin;
  } else if (!(yyin = fopen (file.c_str (), "r"))) {
      error ("cannot open " + file + ": " + strerror(errno));
      exit (EXIT_FAILURE);
    }
}

void
calcxx_driver::scan_end () {
  fclose (yyin);
}
