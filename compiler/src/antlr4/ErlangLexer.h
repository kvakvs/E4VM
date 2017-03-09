
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"




class  ErlangLexer : public antlr4::Lexer {
public:
  enum {
    TokDot = 1, TokMinus = 2, TokPlus = 3, TokSlash = 4, TokStar = 5, TokBang = 6, 
    TokBar = 7, TokBarBar = 8, TokBinaryClose = 9, TokBinaryOpen = 10, TokColon = 11, 
    TokComma = 12, TokCurlyClose = 13, TokCurlyOpen = 14, TokDoubleColon = 15, 
    TokDoubleDot = 16, TokEllipsis = 17, TokEq = 18, TokHash = 19, TokLArrow = 20, 
    TokLDoubleArrow = 21, TokParenClose = 22, TokParenOpen = 23, TokRArrow = 24, 
    TokSemicolon = 25, TokSquareClose = 26, TokSquareOpen = 27, TokAfter = 28, 
    TokAndalso = 29, TokBegin = 30, TokCase = 31, TokCatch = 32, TokEnd = 33, 
    TokFun = 34, TokIf = 35, TokOf = 36, TokOrelse = 37, TokReceive = 38, 
    TokTry = 39, TokWhen = 40, TokAtom = 41, TokVar = 42, TokFloat = 43, 
    TokInteger = 44, TokChar = 45, TokString = 46, TokAttrName = 47, TokComment = 48, 
    TokWhitespace = 49, TokBnot = 50, TokNot = 51, TokDiv = 52, TokRem = 53, 
    TokBand = 54, TokAnd = 55, TokBor = 56, TokBxor = 57, TokBsl = 58, TokBsr = 59, 
    TokOr = 60, TokXor = 61, TokDoublePlus = 62, TokDoubleMinus = 63, TokDoubleEq = 64, 
    TokNotEq = 65, TokLessEq = 66, TokLess = 67, TokGreaterEq = 68, TokGreater = 69, 
    TokStrictEq = 70, TokStrictNeq = 71
  };

  ErlangLexer(antlr4::CharStream *input);
  ~ErlangLexer();

  virtual std::string getGrammarFileName() const override;
  virtual const std::vector<std::string>& getRuleNames() const override;

  virtual const std::vector<std::string>& getModeNames() const override;
  virtual const std::vector<std::string>& getTokenNames() const override; // deprecated, use vocabulary instead
  virtual antlr4::dfa::Vocabulary& getVocabulary() const override;

  virtual const std::vector<uint16_t> getSerializedATN() const override;
  virtual const antlr4::atn::ATN& getATN() const override;

private:
  static std::vector<antlr4::dfa::DFA> _decisionToDFA;
  static antlr4::atn::PredictionContextCache _sharedContextCache;
  static std::vector<std::string> _ruleNames;
  static std::vector<std::string> _tokenNames;
  static std::vector<std::string> _modeNames;

  static std::vector<std::string> _literalNames;
  static std::vector<std::string> _symbolicNames;
  static antlr4::dfa::Vocabulary _vocabulary;
  static antlr4::atn::ATN _atn;
  static std::vector<uint16_t> _serializedATN;


  // Individual action functions triggered by action() above.

  // Individual semantic predicate functions triggered by sempred() above.

  struct Initializer {
    Initializer();
  };
  static Initializer _init;
};

