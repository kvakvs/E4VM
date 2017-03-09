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

TokDot: '.' ;
forms : form+ EOF ;

// form : (attribute | function | ruleClauses) TokDot ;
form : ( attribute | function ) TokDot ;

/// Tokens

TokAfter:       'after' ;
TokAnd:         'and' ;
TokAndalso:     'andalso' ;
TokBand:        'band' ;
TokBang:        '!' ;
TokBar:         '|' ;
TokBarBar:      '||' ;
TokBegin:       'begin' ;
TokBinaryClose: '>>' ;
TokBinaryOpen:  '<<' ;
TokBnot:        'bnot' ;
TokBor:         'bor' ;
TokBsl:         'bsl' ;
TokBsr:         'bsr' ;
TokBxor:        'bxor' ;
TokCase:        'case' ;
TokCatch:       'catch' ;
TokColon:       ':' ;
TokComma:       ',' ;
TokCurlyClose:  '}' ;
TokCurlyOpen:   '{' ;
TokDiv:         'div' ;
TokDoubleColon: '::' ;
TokDoubleDot:   '..' ;
TokDoubleEq:    '==' ;
TokDoubleMinus: '--' ;
TokDoublePlus:  '++' ;
TokEllipsis:    '...' ;
TokEnd:         'end' ;
TokEq:          '=' ;
TokFun:         'fun' ;
TokGreater:     '>' ;
TokGreaterEq:   '>=' ;
TokHash:        '#' ;
TokIf:          'if' ;
TokLArrow:      '<-' ;
TokLDoubleArrow:'<=' ;
TokLess:        '<' ;
TokLessEq:      '=<' ;
TokMinus:       '-' ;
TokNot:         'not' ;
TokNotEq:       '/=' ;
TokOf:          'of' ;
TokOr:          'or' ;
TokOrelse:      'orelse' ;
TokParenClose:  ')' ;
TokParenOpen:   '(' ;
TokPlus:        '+' ;
TokRArrow:      '->' ;
TokReceive:     'receive' ;
TokRem:         'rem' ;
TokSemicolon:   ';' ;
TokSlash:       '/' ;
TokSquareClose: ']' ;
TokSquareOpen:  '[' ;
TokStar:        '*' ;
TokStrictEq:    '=:=' ;
TokStrictNeq:   '=/=' ;
TokTry:         'try' ;
TokWhen:        'when' ;
TokXor:         'xor' ;

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
TokAttrName : '-' ('spec' | 'callback') ;

TokComment : '%' ~[\r\n]* '\r'? '\n' -> skip ;

TokWhitespace : [ \t\r\n]+ -> skip ;



attribute : TokMinus tokAtom attrVal
          | TokMinus tokAtom typedAttrVal
          | TokMinus tokAtom TokParenOpen typedAttrVal TokParenClose
          | TokAttrName typeSpec
          ;


/// Typing

typeSpec :              specFun typeSigs
         | TokParenOpen specFun typeSigs TokParenClose
         ;

specFun :                  tokAtom
        | tokAtom TokColon tokAtom
// The following two are retained only for backwards compatibility;
// they are not part of the EEP syntax and should be removed.
        |                  tokAtom TokSlash tokInteger TokDoubleColon
        | tokAtom TokColon tokAtom TokSlash tokInteger TokDoubleColon
        ;

typedAttrVal : expression TokComma  typedRecordFields
             | expression TokDoubleColon topType
             ;

typedRecordFields : TokCurlyOpen typedExprs TokCurlyClose ;

typedExprs : typedExpr
           | typedExpr  TokComma typedExprs
           | expression  TokComma typedExprs
           | typedExpr  TokComma      commaSeparatedExprs ;

typedExpr : expression TokDoubleColon topType ;

typeSigs : typeSig (TokSemicolon typeSig)* ;

typeSig : funType (TokWhen typeGuards)? ;

typeGuards : typeGuard (TokComma typeGuard)* ;

typeGuard : tokAtom TokParenOpen topTypes TokParenClose
          | tokVar TokDoubleColon topType ;

topTypes : topType (TokComma topType)* ;

topType : (tokVar TokDoubleColon)? topType100 ;

topType100 : type200 (TokBar topType100)? ;

type200 : type300 (TokDoubleDot type300)? ;

type300 : type300 addOp type400
        |               type400 ;

type400 : type400 multOp type500
        |                type500 ;

type500 : unaryOp? type ;

type : TokParenOpen topType TokParenClose
     | tokVar
     | tokAtom
     | tokAtom                  TokParenOpen          TokParenClose
     | tokAtom                  TokParenOpen topTypes TokParenClose
     | tokAtom TokColon tokAtom TokParenOpen          TokParenClose
     | tokAtom TokColon tokAtom TokParenOpen topTypes TokParenClose
     | TokSquareOpen                   TokSquareClose
     | TokSquareOpen topType           TokSquareClose
     | TokSquareOpen topType TokComma TokEllipsis TokSquareClose
     | TokCurlyOpen          TokCurlyClose
     | TokCurlyOpen topTypes TokCurlyClose
     | TokHash tokAtom TokCurlyOpen            TokCurlyClose
     | TokHash tokAtom TokCurlyOpen fieldTypes TokCurlyClose
     | binaryType
     | tokInteger
     | TokFun TokParenOpen            TokParenClose
     | TokFun TokParenOpen funType100 TokParenClose ;

funType100 : TokParenOpen TokEllipsis TokParenClose TokRArrow topType
           | funType ;

funType : TokParenOpen (topTypes)? TokParenClose TokRArrow topType ;

fieldTypes : fieldType (TokComma fieldType)* ;

fieldType : tokAtom TokDoubleColon topType ;

binaryType : TokBinaryOpen                                  TokBinaryClose
           | TokBinaryOpen binBaseType                      TokBinaryClose
           | TokBinaryOpen                      binUnitType TokBinaryClose
           | TokBinaryOpen binBaseType TokComma binUnitType TokBinaryClose
           ;

binBaseType : tokVar TokColon            type ;

binUnitType : tokVar TokColon tokVar TokStar type ;



/// Exprs

attrVal :              expression
        | TokParenOpen expression                              TokParenClose
        |              expression TokComma commaSeparatedExprs
        | TokParenOpen expression TokComma commaSeparatedExprs TokParenClose ;

function : functionClause (TokSemicolon functionClause)* ;

functionClause : tokAtom clauseArgs clauseGuard clauseBody ;


clauseArgs : argumentList ;

clauseGuard : (TokWhen guard)? ;

clauseBody : TokRArrow commaSeparatedExprs ;

expression  : TokCatch expression | matchExpr ;

matchExpr   : orelseExpr    ((TokEq | TokBang) orelseExpr)* ;
orelseExpr  : andalsoExpr   (TokOrelse andalsoExpr)* ;
andalsoExpr : compareExpr   (TokAndalso compareExpr)* ;
compareExpr : listExpr      (compareOp listExpr)? ;
listExpr    : addExpr       (listOp addExpr)* ;
addExpr     : multExpr      (addOp multExpr)* ;
multExpr    : unaryExpr     (multOp unaryExpr)* ;
unaryExpr   : unaryOp? expr700 ;
expr700     : functionCall
            | recordExpr
            | colonExpr ;
colonExpr   : exprMax (TokColon exprMax)? ;

exprMax : tokVar
        | literal
        | list
        | binary
        | listComprehension
        | binaryComprehension
        | tuple
    //  | struct
        | TokParenOpen expression TokParenClose
        | TokBegin commaSeparatedExprs TokEnd
        | ifExpr
        | caseExpr
        | receiveExpr
        | funExpr
        | tryExpr
        ;

list : TokSquareOpen                TokSquareClose
     | TokSquareOpen expression tail
     ;
tail :                          TokSquareClose
     | TokBar expression         TokSquareClose
     | TokComma expression tail
     ;

binary : TokBinaryOpen             TokBinaryClose
       | TokBinaryOpen binElements TokBinaryClose ;

binElements     : binElement (TokComma binElement)* ;

binElement      : bitExpr optBitSizeExpr optBitTypeList ;

bitExpr         : unaryOp? exprMax ;

optBitSizeExpr  : (TokColon bitSizeExpr)? ;

optBitTypeList  : (TokSlash bitTypeList)? ;

bitTypeList     : bitType (TokMinus bitType)* ;

bitType         : tokAtom (TokColon tokInteger)? ;

bitSizeExpr     : exprMax ;


listComprehension : TokSquareOpen expression TokBarBar lcExprs TokSquareClose ;

binaryComprehension : TokBinaryOpen binary TokBarBar lcExprs TokBinaryClose ;

lcExprs : lcExpr (TokComma lcExpr)* ;

lcExpr : expression
       | expression  TokLArrow       expression
       | binary      TokLDoubleArrow expression
       ;

tuple : TokCurlyOpen commaSeparatedExprs? TokCurlyClose ;


/* struct : tokAtom tuple ; */


/* N.B. This is called from expr700.
   N.B. Field names are returned as the complete object, even if they are
   always atoms for the moment, this might change in the future.           */

recordExpr : exprMax?   TokHash tokAtom (TokDot tokAtom | recordTuple)
           | recordExpr TokHash tokAtom (TokDot tokAtom | recordTuple)
           ;

recordTuple : TokCurlyOpen recordFields? TokCurlyClose ;

recordFields : recordField (TokComma recordField)* ;

recordField : (tokVar | tokAtom) TokEq expression ;


/* N.B. This is called from expr700. */

functionCall : colonExpr argumentList ;


ifExpr : TokIf ifClauses TokEnd ;

ifClauses : ifClause (TokSemicolon ifClause)* ;

ifClause : guard clauseBody ;


caseExpr : TokCase expression TokOf crClauses TokEnd ;

crClauses : crClause (TokSemicolon crClause)* ;

crClause : expression clauseGuard clauseBody ;


receiveExpr : TokReceive crClauses                                TokEnd
            | TokReceive           TokAfter expression clauseBody TokEnd
            | TokReceive crClauses TokAfter expression clauseBody TokEnd
            ;


funExpr : TokFun tokAtom TokSlash tokInteger
        | TokFun atomOrVar TokColon atomOrVar TokSlash integerOrVar
        | TokFun funClauses TokEnd
        ;

atomOrVar : tokAtom | tokVar ;

integerOrVar : tokInteger | tokVar ;


funClauses : funClause (TokSemicolon funClause)* ;

funClause : argumentList clauseGuard clauseBody ;


tryExpr : TokTry commaSeparatedExprs (TokOf crClauses)? tryCatch ;

tryCatch : TokCatch tryClauses                              TokEnd
         | TokCatch tryClauses TokAfter commaSeparatedExprs TokEnd
         |                     TokAfter commaSeparatedExprs TokEnd ;

tryClauses : tryClause (TokSemicolon tryClause)* ;

tryClause : (atomOrVar TokColon)? expression clauseGuard clauseBody ;



argumentList : TokParenOpen commaSeparatedExprs? TokParenClose ;

commaSeparatedExprs : expression (TokComma expression)* ;

guard : commaSeparatedExprs (TokSemicolon commaSeparatedExprs)* ;

literal : tokChar
        | tokInteger
        | tokFloat
        | tokAtom
        | (tokString)+
        ;

unaryOp : TokPlus
        | TokMinus
        | TokBnot
        | TokNot
        ;

multOp : TokSlash
       | TokStar
       | TokDiv
       | TokRem
       | TokBand
       | TokAnd
       ;

addOp : TokPlus
      | TokMinus
      | TokBor
      | TokBxor
      | TokBsl
      | TokBsr
      | TokOr
      | TokXor
      ;

listOp : TokDoublePlus
       | TokDoubleMinus
       ;

compareOp : TokDoubleEq
          | TokNotEq
          | TokLessEq
          | TokLess
          | TokGreaterEq
          | TokGreater
          | TokStrictEq
          | TokStrictNeq
          ;

//ruleClauses : ruleClause (TokSemicolon ruleClause)* ;
//ruleClause : tokAtom clauseArgs clauseGuard ruleBody ;
//ruleBody : ':-' lcExprs ;
