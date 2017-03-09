
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"




class  ErlangLexer : public antlr4::Lexer {
public:
  enum {
    TokDot = 1, TokAfter = 2, TokAnd = 3, TokAndalso = 4, TokBand = 5, TokBang = 6, 
    TokBar = 7, TokBarBar = 8, TokBegin = 9, TokBinaryClose = 10, TokBinaryOpen = 11, 
    TokBnot = 12, TokBor = 13, TokBsl = 14, TokBsr = 15, TokBxor = 16, TokCase = 17, 
    TokCatch = 18, TokColon = 19, TokComma = 20, TokCurlyClose = 21, TokCurlyOpen = 22, 
    TokDiv = 23, TokDoubleColon = 24, TokDoubleDot = 25, TokDoubleEq = 26, 
    TokDoubleMinus = 27, TokDoublePlus = 28, TokEllipsis = 29, TokEnd = 30, 
    TokEq = 31, TokFun = 32, TokGreater = 33, TokGreaterEq = 34, TokHash = 35, 
    TokIf = 36, TokLArrow = 37, TokLDoubleArrow = 38, TokLess = 39, TokLessEq = 40, 
    TokMinus = 41, TokNot = 42, TokNotEq = 43, TokOf = 44, TokOr = 45, TokOrelse = 46, 
    TokParenClose = 47, TokParenOpen = 48, TokPlus = 49, TokRArrow = 50, 
    TokReceive = 51, TokRem = 52, TokSemicolon = 53, TokSlash = 54, TokSquareClose = 55, 
    TokSquareOpen = 56, TokStar = 57, TokStrictEq = 58, TokStrictNeq = 59, 
    TokTry = 60, TokWhen = 61, TokXor = 62, TokAtom = 63, TokVar = 64, TokFloat = 65, 
    TokInteger = 66, TokChar = 67, TokString = 68, TokAttrName = 69, TokComment = 70, 
    TokWhitespace = 71
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

