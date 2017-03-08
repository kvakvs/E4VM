/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
*/

// An ANTLR4 Grammar of Erlang R16B01 made by Pierre Fenoll from
// https://github.com/erlang/otp/blob/maint/lib/stdlib/src/erl_parse.yrl
// Modified by Dmytro Lytovchenko to Erlang 19 and 20
// <dmytro.lytovchenko@gmail.com>

grammar Erlang;

forms : form+ EOF ;

form : (attribute | function | ruleClauses) '.' ;

/// Tokens

tokAtom : TokAtom ;
TokAtom : [a-z@][0-9a-zA-Z_@]*
        | '\'' ( '\\' (~'\\'|'\\') | ~[\\'] )* '\'' ;

tokVar : TokVar ;
TokVar : [A-Z_][0-9a-zA-Z_]* ;

tokFloat : TokFloat ;
TokFloat : '-'? [0-9]+ '.' [0-9]+  ([Ee] [+-]? [0-9]+)? ;

tokInteger : TokInteger ;
TokInteger : '-'? [0-9]+ ('#' [0-9a-zA-Z]+)? ;

tokChar : TokChar ;
TokChar : '$' ('\\'? ~[\r\n] | '\\' [0-9] [0-9] [0-9]) ;

tokString : TokString ;
TokString : '"' ( '\\' (~'\\'|'\\') | ~[\\"] )* '"' ;

// antlr4 would not accept spec as an Atom otherwise.
AttrName : '-' ('spec' | 'callback') ;

Comment : '%' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;



attribute : '-' tokAtom                           attrVal
          | '-' tokAtom                      typedAttrVal
          | '-' tokAtom                  '(' typedAttrVal ')'
          | AttrName                           typeSpec
          ;


/// Typing

typeSpec :     specFun typeSigs
         | '(' specFun typeSigs ')'
         ;

specFun :             tokAtom
        | tokAtom ':' tokAtom
// The following two are retained only for backwards compatibility;
// they are not part of the EEP syntax and should be removed.
        |             tokAtom '/' tokInteger '::'
        | tokAtom ':' tokAtom '/' tokInteger '::'
        ;

typedAttrVal : catchExpr ','  typedRecordFields
             | catchExpr '::' topType
             ;

typedRecordFields : '{' typedExprs '}' ;

typedExprs : typedExpr
           | typedExpr  ',' typedExprs
           | catchExpr       ',' typedExprs
           | typedExpr  ','      exprs ;

typedExpr : catchExpr '::' topType ;

typeSigs : typeSig (';' typeSig)* ;

typeSig : funType ('when' typeGuards)? ;

typeGuards : typeGuard (',' typeGuard)* ;

typeGuard : tokAtom '(' topTypes ')'
          | tokVar '::' topType ;

topTypes : topType (',' topType)* ;

topType : (tokVar '::')? topType100 ;

topType100 : type200 ('|' topType100)? ;

type200 : type300 ('..' type300)? ;

type300 : type300 addOp type400
        |               type400 ;

type400 : type400 multOp type500
        |                type500 ;

type500 : unaryOp? type ;

type : '(' topType ')'
     | tokVar
     | tokAtom
     | tokAtom             '('          ')'
     | tokAtom             '(' topTypes ')'
     | tokAtom ':' tokAtom '('          ')'
     | tokAtom ':' tokAtom '(' topTypes ')'
     | '['                   ']'
     | '[' topType           ']'
     | '[' topType ',' '...' ']'
     | '{'          '}'
     | '{' topTypes '}'
     | '#' tokAtom '{'            '}'
     | '#' tokAtom '{' fieldTypes '}'
     | binaryType
     | tokInteger
     | 'fun' '('            ')'
     | 'fun' '(' funType100 ')' ;

funType100 : '(' '...' ')' '->' topType
           | funType ;

funType : '(' (topTypes)? ')' '->' topType ;

fieldTypes : fieldType (',' fieldType)* ;

fieldType : tokAtom '::' topType ;

binaryType : '<<'                             '>>'
           | '<<' binBaseType                 '>>'
           | '<<'                 binUnitType '>>'
           | '<<' binBaseType ',' binUnitType '>>'
           ;

binBaseType : tokVar ':'            type ;

binUnitType : tokVar ':' tokVar '*' type ;



/// Exprs

attrVal :     catchExpr
        | '(' catchExpr           ')'
        |     catchExpr ',' exprs
        | '(' catchExpr ',' exprs ')' ;

function : functionClause (';' functionClause)* ;

functionClause : tokAtom clauseArgs clauseGuard clauseBody ;


clauseArgs : argumentList ;

clauseGuard : ('when' guard)? ;

clauseBody
    : '->' exprs ;

catchExpr
    : 'catch' catchExpr
    | matchbangExpr
    ;

matchbangExpr
    : orelseExpr
    | ('=' | '!') orelseExpr
    ;

orelseExpr
    : andalsoExpr
    | 'orelse' andalsoExpr
    ;

andalsoExpr
    : compareExpr
    | 'andalso' compareExpr
    ;

compareExpr
    : listExpr
    | compareExpr compareOp listExpr
    ;

listExpr
    : addExpr
    | listExpr listOp addExpr
    ;

addExpr
    : multExpr
    | addExpr addOp multExpr
    ;

multExpr
    : unaryExpr
    | multExpr multOp unaryExpr ;

unaryExpr : unaryOp? expr700 ;

expr700 : functionCall
        | recordExpr
        | semicolonExpr ;

semicolonExpr : exprMax (':' exprMax)? ;

exprMax : tokVar
        | atomic
        | list
        | binary
        | listComprehension
        | binaryComprehension
        | tuple
    //  | struct
        | '(' catchExpr ')'
        | 'begin' exprs 'end'
        | ifExpr
        | caseExpr
        | receiveExpr
        | funExpr
        | tryExpr
        ;

list : '['      ']'
     | '[' catchExpr tail
     ;
tail :          ']'
     | '|' catchExpr ']'
     | ',' catchExpr tail
     ;

binary : '<<'             '>>'
       | '<<' binElements '>>' ;

binElements : binElement (',' binElement)* ;

binElement : bitExpr optBitSizeExpr optBitTypeList ;

bitExpr : unaryOp? exprMax ;

optBitSizeExpr : (':' bitSizeExpr)? ;

optBitTypeList : ('/' bitTypeList)? ;

bitTypeList : bitType ('-' bitType)* ;

bitType : tokAtom (':' tokInteger)? ;

bitSizeExpr : exprMax ;


listComprehension :   '['  catchExpr   '||' lcExprs ']' ;

binaryComprehension : '<<' binary '||' lcExprs '>>' ;

lcExprs : lcExpr (',' lcExpr)* ;

lcExpr : catchExpr
       | catchExpr   '<-' catchExpr
       | binary '<=' catchExpr
       ;

tuple : '{' exprs? '}' ;


/* struct : tokAtom tuple ; */


/* N.B. This is called from expr700.
   N.B. Field names are returned as the complete object, even if they are
   always atoms for the moment, this might change in the future.           */

recordExpr : exprMax?   '#' tokAtom ('.' tokAtom | recordTuple)
           | recordExpr '#' tokAtom ('.' tokAtom | recordTuple)
           ;

recordTuple : '{' recordFields? '}' ;

recordFields : recordField (',' recordField)* ;

recordField : (tokVar | tokAtom) '=' catchExpr ;


/* N.B. This is called from expr700. */

functionCall : semicolonExpr argumentList ;


ifExpr : 'if' ifClauses 'end' ;

ifClauses : ifClause (';' ifClause)* ;

ifClause : guard clauseBody ;


caseExpr : 'case' catchExpr 'of' crClauses 'end' ;

crClauses : crClause (';' crClause)* ;

crClause : catchExpr clauseGuard clauseBody ;


receiveExpr : 'receive' crClauses                         'end'
            | 'receive'           'after' catchExpr clauseBody 'end'
            | 'receive' crClauses 'after' catchExpr clauseBody 'end'
            ;


funExpr : 'fun' tokAtom '/' tokInteger
        | 'fun' atomOrVar ':' atomOrVar '/' integerOrVar
        | 'fun' funClauses 'end'
        ;

atomOrVar : tokAtom | tokVar ;

integerOrVar : tokInteger | tokVar ;


funClauses : funClause (';' funClause)* ;

funClause : argumentList clauseGuard clauseBody ;


tryExpr : 'try' exprs ('of' crClauses)? tryCatch ;

tryCatch : 'catch' tryClauses               'end'
         | 'catch' tryClauses 'after' exprs 'end'
         |                    'after' exprs 'end' ;

tryClauses : tryClause (';' tryClause)* ;

tryClause : (atomOrVar ':')? catchExpr clauseGuard clauseBody ;



argumentList : '(' exprs? ')' ;

exprs : catchExpr (',' catchExpr)* ;

guard : exprs (';' exprs)* ;

atomic : tokChar
       | tokInteger
       | tokFloat
       | tokAtom
       | (tokString)+
       ;

unaryOp : '+'
         | '-'
         | 'bnot'
         | 'not'
         ;

multOp : '/'
       | '*'
       | 'div'
       | 'rem'
       | 'band'
       | 'and'
       ;

addOp : '+'
      | '-'
      | 'bor'
      | 'bxor'
      | 'bsl'
      | 'bsr'
      | 'or'
      | 'xor'
      ;

listOp : '++'
       | '--'
       ;

compareOp : '=='
       | '/='
       | '=<'
       | '<'
       | '>='
       | '>'
       | '=:='
       | '=/='
       ;

ruleClauses : ruleClause (';' ruleClause)* ;

ruleClause : tokAtom clauseArgs clauseGuard ruleBody ;

ruleBody : ':-' lcExprs ;
