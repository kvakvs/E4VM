%{
    #include "../erl_yy_driver.h"
    #include "erlang_parser.hpp"

    // define yyterminate as this instead of NULL
    #define yyterminate() { return ErlangParser::make_END_OF_FILE(*loc); }

    // The location of the current token.
    static yy::location loc;
%}

%option yyclass="BaseErlangScanner"
%option C++ noyywrap nounput noinput batch debug
%option stack

%{
  // Code run each time a pattern is matched.
  #define YY_USER_ACTION  loc.columns (yyleng);
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
  loc.step ();
%}

{tBlank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();

"." { return ErlangParser::make_DOT(loc); }

"," { return ErlangParser::make_COMMA(loc); }

{tAZ}({tAZaz}|{t09})* {
    return ErlangParser::make_VARIABLE(yytext, loc);
}

{taz}{tAtom}*   {
    return ErlangParser::make_ATOM(yytext, loc);
}

{t09}({tAZaz}|{t09})*   {
    return ErlangParser::make_INTEGER(yytext, loc);
}

"+"   { return ErlangParser::make_PLUS(loc); }
"-"   { return ErlangParser::make_MINUS(loc); }
"*"   { return ErlangParser::make_MULT(loc); }
"/"   { return ErlangParser::make_FDIV(loc); }

.          driver.error (loc, "invalid character");
<<EOF>>    return ErlangParser::make_END_OF_FILE(loc);
%%

void
ErlangDriver::scan_begin() {
  yy_flex_debug = trace_scanning;
  if (file.empty () || file == "-") {
    yyin = stdin;
  } else if (!(yyin = fopen (file.c_str (), "r"))) {
      error ("cannot open " + file + ": " + strerror(errno));
      exit (EXIT_FAILURE);
    }
}

void
ErlangDriver::scan_end () {
  fclose (yyin);
}
