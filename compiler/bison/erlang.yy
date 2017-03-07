// %glr-parser
// %skeleton "glr.cc"

%skeleton "lalr1.cc"
%require "3.0.4"

%defines
%define parser_class_name {BaseErlangParser}

%define api.token.constructor
%define api.value.type variant
%define parse.assert

// %define api.value.type {int}

%code requires {
    #include <string>
    class ErlangDriver;

    // Tell Flex the lexer's prototype ...
    #undef YY_DECL
    #define YY_DECL yy::BaseErlangParser::symbol_type yylex(ErlangDriver& driver)
}

// The parsing context.
%param { ErlangDriver& driver }

%locations
%initial-action {
    // Initialize the initial location.
    @$.begin.filename = @$.end.filename = &driver.file_;
};

%define parse.trace
%define parse.error verbose

%code {
    #include "../erl_yy_driver.h"

    #undef yylex
    #define yylex driver.lexer_->lex
}

%define api.token.prefix {TOK_}
%printer { yyoutput << $$; } <*>;

%token DOT COMMA ;

%token <std::string>
    ATOM VARIABLE STRING
    ;

%token <std::string>
    INTEGER FLOAT
    ;

%token
    END_OF_FILE 0   "end of file"
    END_OF_LINE     "end of line"

    LT '<'
    GT '>'
    MATCHEQ '='
    GTE ">="
    LTE "=<"
    EQUAL "=="
    EQUAL_S "=:="
    NEQUAL "=/"
    NEQUAL_S "=/="

    PLUS  '+'
    MINUS '-'
    MULT  '*'
    FDIV  '/'
    REM   "rem"
    IDIV  "div"

    BAND  "band"
    BOR   "bor"
    BXOR  "bxor"
    BSL   "bsl"
    BSR   "bsr"

    AND     "and"
    ANDALSO "andalso"
    OR      "or"
    ORELSE  "orelse"
    ;

%left '<' '>' '=' ">=" "=<"
%left "==" "=:=" "=/" "=/="
%left '+' '-' '*' '/' "rem" "div"
%left "band" "bor" "bxor" "bsl" "bsr"
%left "and" "andalso" "or" "orelse" "not"

%%

primary_expr:
    VARIABLE
    | INTEGER
    | FLOAT
    | ATOM
    ;
/*
    | '(' expression ')'
    ;

expression:
    match_expression
    | expression ',' match_expression
    ;

match_expression:
    VARIABLE '=' expression
    ;
*/

%%
void
yy::BaseErlangParser::error(const location_type& l, const std::string& m) {
  driver.error (l, m);
}
