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

TokMinus:       '-' ;
TokPlus:        '+' ;
TokSlash:       '/' ;
TokStar:        '*' ;

TokBang:        '!' ;
TokBar:         '|' ;
TokBarBar:      '||' ;
TokBinaryClose: '>>' ;
TokBinaryOpen:  '<<' ;
TokColon:       ':' ;
TokComma:       ',' ;
TokCurlyClose:  '}' ;
TokCurlyOpen:   '{' ;
TokDoubleColon: '::' ;
TokDoubleDot:   '..' ;
TokEllipsis:    '...' ;
TokEq:          '=' ;
TokHash:        '#' ;
TokLArrow:      '<-' ;
TokLDoubleArrow: '<=' ;
TokParenClose:  ')' ;
TokParenOpen:   '(' ;
TokRArrow:      '->' ;
TokSemicolon:   ';' ;
TokSquareClose: ']' ;
TokSquareOpen:  '[' ;

TokAfter:       'after' ;
TokAndalso:     'andalso' ;
TokBegin:       'begin' ;
TokCase:        'case' ;
TokCatch:       'catch' ;
TokEnd:         'end' ;
TokFun:         'fun' ;
TokIf:          'if' ;
TokOf:          'of' ;
TokOrelse:      'orelse' ;
TokReceive:     'receive' ;
TokTry:         'try' ;
TokWhen:        'when' ;

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
TokAttrName : TokMinus ('spec' | 'callback') ;

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

typedAttrVal : catchExpr TokComma  typedRecordFields
             | catchExpr TokDoubleColon topType
             ;

typedRecordFields : TokCurlyOpen typedExprs TokCurlyClose ;

typedExprs : typedExpr
           | typedExpr  TokComma typedExprs
           | catchExpr  TokComma typedExprs
           | typedExpr  TokComma      exprs ;

typedExpr : catchExpr TokDoubleColon topType ;

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

attrVal :              catchExpr
        | TokParenOpen catchExpr                TokParenClose
        |              catchExpr TokComma exprs
        | TokParenOpen catchExpr TokComma exprs TokParenClose ;

function : functionClause (TokSemicolon functionClause)* ;

functionClause : tokAtom clauseArgs clauseGuard clauseBody ;


clauseArgs : argumentList ;

clauseGuard : (TokWhen guard)? ;

clauseBody
    : TokRArrow exprs ;

catchExpr
    : TokCatch catchExpr
    | matchbangExpr
    ;

matchbangExpr
    : orelseExpr
    | (TokEq | TokBang) orelseExpr
    ;

orelseExpr
    : andalsoExpr
    | TokOrelse andalsoExpr
    ;

andalsoExpr
    : compareExpr
    | TokAndalso compareExpr
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

semicolonExpr : exprMax (TokColon exprMax)? ;

exprMax : tokVar
        | atomic
        | list
        | binary
        | listComprehension
        | binaryComprehension
        | tuple
    //  | struct
        | TokParenOpen catchExpr TokParenClose
        | TokBegin exprs TokEnd
        | ifExpr
        | caseExpr
        | receiveExpr
        | funExpr
        | tryExpr
        ;

list : TokSquareOpen                TokSquareClose
     | TokSquareOpen catchExpr tail
     ;
tail :                          TokSquareClose
     | TokBar catchExpr         TokSquareClose
     | TokComma catchExpr tail
     ;

binary : TokBinaryOpen             TokBinaryClose
       | TokBinaryOpen binElements TokBinaryClose ;

binElements : binElement (TokComma binElement)* ;

binElement : bitExpr optBitSizeExpr optBitTypeList ;

bitExpr : unaryOp? exprMax ;

optBitSizeExpr : (TokColon bitSizeExpr)? ;

optBitTypeList : (TokSlash bitTypeList)? ;

bitTypeList : bitType (TokMinus bitType)* ;

bitType : tokAtom (TokColon tokInteger)? ;

bitSizeExpr : exprMax ;


listComprehension : TokSquareOpen catchExpr TokBarBar lcExprs TokSquareClose ;

binaryComprehension : TokBinaryOpen binary TokBarBar lcExprs TokBinaryClose ;

lcExprs : lcExpr (TokComma lcExpr)* ;

lcExpr : catchExpr
       | catchExpr  TokLArrow       catchExpr
       | binary     TokLDoubleArrow catchExpr
       ;

tuple : TokCurlyOpen exprs? TokCurlyClose ;


/* struct : tokAtom tuple ; */


/* N.B. This is called from expr700.
   N.B. Field names are returned as the complete object, even if they are
   always atoms for the moment, this might change in the future.           */

recordExpr : exprMax?   TokHash tokAtom (TokDot tokAtom | recordTuple)
           | recordExpr TokHash tokAtom (TokDot tokAtom | recordTuple)
           ;

recordTuple : TokCurlyOpen recordFields? TokCurlyClose ;

recordFields : recordField (TokComma recordField)* ;

recordField : (tokVar | tokAtom) TokEq catchExpr ;


/* N.B. This is called from expr700. */

functionCall : semicolonExpr argumentList ;


ifExpr : TokIf ifClauses TokEnd ;

ifClauses : ifClause (TokSemicolon ifClause)* ;

ifClause : guard clauseBody ;


caseExpr : TokCase catchExpr TokOf crClauses TokEnd ;

crClauses : crClause (TokSemicolon crClause)* ;

crClause : catchExpr clauseGuard clauseBody ;


receiveExpr : TokReceive crClauses                              TokEnd
            | TokReceive           TokAfter catchExpr clauseBody TokEnd
            | TokReceive crClauses TokAfter catchExpr clauseBody TokEnd
            ;


funExpr : TokFun tokAtom TokSlash tokInteger
        | TokFun atomOrVar TokColon atomOrVar TokSlash integerOrVar
        | TokFun funClauses TokEnd
        ;

atomOrVar : tokAtom | tokVar ;

integerOrVar : tokInteger | tokVar ;


funClauses : funClause (TokSemicolon funClause)* ;

funClause : argumentList clauseGuard clauseBody ;


tryExpr : TokTry exprs (TokOf crClauses)? tryCatch ;

tryCatch : TokCatch tryClauses                TokEnd
         | TokCatch tryClauses TokAfter exprs TokEnd
         |                     TokAfter exprs TokEnd ;

tryClauses : tryClause (TokSemicolon tryClause)* ;

tryClause : (atomOrVar TokColon)? catchExpr clauseGuard clauseBody ;



argumentList : TokParenOpen exprs? TokParenClose ;

exprs : catchExpr (TokComma catchExpr)* ;

guard : exprs (TokSemicolon exprs)* ;

atomic : tokChar
       | tokInteger
       | tokFloat
       | tokAtom
       | (tokString)+
       ;

TokBnot:    'bnot' ;
TokNot:     'not' ;

unaryOp : TokPlus
         | TokMinus
         | TokBnot
         | TokNot
         ;

TokDiv:     'div' ;
TokRem:     'rem' ;
TokBand:    'band' ;
TokAnd:     'and' ;

multOp : TokSlash
       | TokStar
       | TokDiv
       | TokRem
       | TokBand
       | TokAnd
       ;

TokBor:     'bor' ;
TokBxor:    'bxor' ;
TokBsl:     'bsl' ;
TokBsr:     'bsr' ;
TokOr:      'or' ;
TokXor:     'xor' ;

addOp : TokPlus
      | TokMinus
      | TokBor
      | TokBxor
      | TokBsl
      | TokBsr
      | TokOr
      | TokXor
      ;

TokDoublePlus:   '++' ;
TokDoubleMinus:  '--' ;

listOp : TokDoublePlus
       | TokDoubleMinus
       ;

TokDoubleEq:    '==' ;
TokNotEq:       '/=' ;
TokLessEq:      '=<' ;
TokLess:        '<' ;
TokGreaterEq:   '>=' ;
TokGreater:     '>' ;
TokStrictEq:    '=:=' ;
TokStrictNeq:   '=/=' ;

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
