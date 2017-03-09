
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"




class  ErlangParser : public antlr4::Parser {
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

  enum {
    RuleForms = 0, RuleForm = 1, RuleTokAtom = 2, RuleTokVar = 3, RuleTokFloat = 4, 
    RuleTokInteger = 5, RuleTokChar = 6, RuleTokString = 7, RuleAttribute = 8, 
    RuleTypeSpec = 9, RuleSpecFun = 10, RuleTypedAttrVal = 11, RuleTypedRecordFields = 12, 
    RuleTypedExprs = 13, RuleTypedExpr = 14, RuleTypeSigs = 15, RuleTypeSig = 16, 
    RuleTypeGuards = 17, RuleTypeGuard = 18, RuleTopTypes = 19, RuleTopType = 20, 
    RuleTopType100 = 21, RuleType200 = 22, RuleType300 = 23, RuleType400 = 24, 
    RuleType500 = 25, RuleType = 26, RuleFunType100 = 27, RuleFunType = 28, 
    RuleFieldTypes = 29, RuleFieldType = 30, RuleBinaryType = 31, RuleBinBaseType = 32, 
    RuleBinUnitType = 33, RuleAttrVal = 34, RuleFunction = 35, RuleFunctionClause = 36, 
    RuleClauseArgs = 37, RuleClauseGuard = 38, RuleClauseBody = 39, RuleExpression = 40, 
    RuleMatchExpr = 41, RuleOrelseExpr = 42, RuleAndalsoExpr = 43, RuleCompareExpr = 44, 
    RuleListExpr = 45, RuleAddExpr = 46, RuleMultExpr = 47, RuleUnaryExpr = 48, 
    RuleExpr700 = 49, RuleColonExpr = 50, RuleExprMax = 51, RuleList = 52, 
    RuleTail = 53, RuleBinary = 54, RuleBinElements = 55, RuleBinElement = 56, 
    RuleBitExpr = 57, RuleOptBitSizeExpr = 58, RuleOptBitTypeList = 59, 
    RuleBitTypeList = 60, RuleBitType = 61, RuleBitSizeExpr = 62, RuleListComprehension = 63, 
    RuleBinaryComprehension = 64, RuleLcExprs = 65, RuleLcExpr = 66, RuleTuple = 67, 
    RuleRecordExpr = 68, RuleRecordTuple = 69, RuleRecordFields = 70, RuleRecordField = 71, 
    RuleFunctionCall = 72, RuleIfExpr = 73, RuleIfClauses = 74, RuleIfClause = 75, 
    RuleCaseExpr = 76, RuleCrClauses = 77, RuleCrClause = 78, RuleReceiveExpr = 79, 
    RuleFunExpr = 80, RuleAtomOrVar = 81, RuleIntegerOrVar = 82, RuleFunClauses = 83, 
    RuleFunClause = 84, RuleTryExpr = 85, RuleTryCatch = 86, RuleTryClauses = 87, 
    RuleTryClause = 88, RuleArgumentList = 89, RuleCommaSeparatedExprs = 90, 
    RuleGuard = 91, RuleLiteral = 92, RuleUnaryOp = 93, RuleMultOp = 94, 
    RuleAddOp = 95, RuleListOp = 96, RuleCompareOp = 97
  };

  ErlangParser(antlr4::TokenStream *input);
  ~ErlangParser();

  virtual std::string getGrammarFileName() const override;
  virtual const antlr4::atn::ATN& getATN() const override { return _atn; };
  virtual const std::vector<std::string>& getTokenNames() const override { return _tokenNames; }; // deprecated: use vocabulary instead.
  virtual const std::vector<std::string>& getRuleNames() const override;
  virtual antlr4::dfa::Vocabulary& getVocabulary() const override;


  class FormsContext;
  class FormContext;
  class TokAtomContext;
  class TokVarContext;
  class TokFloatContext;
  class TokIntegerContext;
  class TokCharContext;
  class TokStringContext;
  class AttributeContext;
  class TypeSpecContext;
  class SpecFunContext;
  class TypedAttrValContext;
  class TypedRecordFieldsContext;
  class TypedExprsContext;
  class TypedExprContext;
  class TypeSigsContext;
  class TypeSigContext;
  class TypeGuardsContext;
  class TypeGuardContext;
  class TopTypesContext;
  class TopTypeContext;
  class TopType100Context;
  class Type200Context;
  class Type300Context;
  class Type400Context;
  class Type500Context;
  class TypeContext;
  class FunType100Context;
  class FunTypeContext;
  class FieldTypesContext;
  class FieldTypeContext;
  class BinaryTypeContext;
  class BinBaseTypeContext;
  class BinUnitTypeContext;
  class AttrValContext;
  class FunctionContext;
  class FunctionClauseContext;
  class ClauseArgsContext;
  class ClauseGuardContext;
  class ClauseBodyContext;
  class ExpressionContext;
  class MatchExprContext;
  class OrelseExprContext;
  class AndalsoExprContext;
  class CompareExprContext;
  class ListExprContext;
  class AddExprContext;
  class MultExprContext;
  class UnaryExprContext;
  class Expr700Context;
  class ColonExprContext;
  class ExprMaxContext;
  class ListContext;
  class TailContext;
  class BinaryContext;
  class BinElementsContext;
  class BinElementContext;
  class BitExprContext;
  class OptBitSizeExprContext;
  class OptBitTypeListContext;
  class BitTypeListContext;
  class BitTypeContext;
  class BitSizeExprContext;
  class ListComprehensionContext;
  class BinaryComprehensionContext;
  class LcExprsContext;
  class LcExprContext;
  class TupleContext;
  class RecordExprContext;
  class RecordTupleContext;
  class RecordFieldsContext;
  class RecordFieldContext;
  class FunctionCallContext;
  class IfExprContext;
  class IfClausesContext;
  class IfClauseContext;
  class CaseExprContext;
  class CrClausesContext;
  class CrClauseContext;
  class ReceiveExprContext;
  class FunExprContext;
  class AtomOrVarContext;
  class IntegerOrVarContext;
  class FunClausesContext;
  class FunClauseContext;
  class TryExprContext;
  class TryCatchContext;
  class TryClausesContext;
  class TryClauseContext;
  class ArgumentListContext;
  class CommaSeparatedExprsContext;
  class GuardContext;
  class LiteralContext;
  class UnaryOpContext;
  class MultOpContext;
  class AddOpContext;
  class ListOpContext;
  class CompareOpContext; 

  class  FormsContext : public antlr4::ParserRuleContext {
  public:
    FormsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *EOF();
    std::vector<FormContext *> form();
    FormContext* form(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FormsContext* forms();

  class  FormContext : public antlr4::ParserRuleContext {
  public:
    FormContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokDot();
    AttributeContext *attribute();
    FunctionContext *function();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FormContext* form();

  class  TokAtomContext : public antlr4::ParserRuleContext {
  public:
    TokAtomContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokAtom();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TokAtomContext* tokAtom();

  class  TokVarContext : public antlr4::ParserRuleContext {
  public:
    TokVarContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokVar();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TokVarContext* tokVar();

  class  TokFloatContext : public antlr4::ParserRuleContext {
  public:
    TokFloatContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokFloat();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TokFloatContext* tokFloat();

  class  TokIntegerContext : public antlr4::ParserRuleContext {
  public:
    TokIntegerContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokInteger();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TokIntegerContext* tokInteger();

  class  TokCharContext : public antlr4::ParserRuleContext {
  public:
    TokCharContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokChar();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TokCharContext* tokChar();

  class  TokStringContext : public antlr4::ParserRuleContext {
  public:
    TokStringContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokString();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TokStringContext* tokString();

  class  AttributeContext : public antlr4::ParserRuleContext {
  public:
    AttributeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokMinus();
    TokAtomContext *tokAtom();
    AttrValContext *attrVal();
    TypedAttrValContext *typedAttrVal();
    antlr4::tree::TerminalNode *TokParenOpen();
    antlr4::tree::TerminalNode *TokParenClose();
    antlr4::tree::TerminalNode *TokAttrName();
    TypeSpecContext *typeSpec();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AttributeContext* attribute();

  class  TypeSpecContext : public antlr4::ParserRuleContext {
  public:
    TypeSpecContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    SpecFunContext *specFun();
    TypeSigsContext *typeSigs();
    antlr4::tree::TerminalNode *TokParenOpen();
    antlr4::tree::TerminalNode *TokParenClose();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeSpecContext* typeSpec();

  class  SpecFunContext : public antlr4::ParserRuleContext {
  public:
    SpecFunContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TokAtomContext *> tokAtom();
    TokAtomContext* tokAtom(size_t i);
    antlr4::tree::TerminalNode *TokColon();
    antlr4::tree::TerminalNode *TokSlash();
    TokIntegerContext *tokInteger();
    antlr4::tree::TerminalNode *TokDoubleColon();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  SpecFunContext* specFun();

  class  TypedAttrValContext : public antlr4::ParserRuleContext {
  public:
    TypedAttrValContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokComma();
    TypedRecordFieldsContext *typedRecordFields();
    antlr4::tree::TerminalNode *TokDoubleColon();
    TopTypeContext *topType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedAttrValContext* typedAttrVal();

  class  TypedRecordFieldsContext : public antlr4::ParserRuleContext {
  public:
    TypedRecordFieldsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokCurlyOpen();
    TypedExprsContext *typedExprs();
    antlr4::tree::TerminalNode *TokCurlyClose();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedRecordFieldsContext* typedRecordFields();

  class  TypedExprsContext : public antlr4::ParserRuleContext {
  public:
    TypedExprsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TypedExprContext *typedExpr();
    antlr4::tree::TerminalNode *TokComma();
    TypedExprsContext *typedExprs();
    ExpressionContext *expression();
    CommaSeparatedExprsContext *commaSeparatedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedExprsContext* typedExprs();

  class  TypedExprContext : public antlr4::ParserRuleContext {
  public:
    TypedExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokDoubleColon();
    TopTypeContext *topType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedExprContext* typedExpr();

  class  TypeSigsContext : public antlr4::ParserRuleContext {
  public:
    TypeSigsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TypeSigContext *> typeSig();
    TypeSigContext* typeSig(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeSigsContext* typeSigs();

  class  TypeSigContext : public antlr4::ParserRuleContext {
  public:
    TypeSigContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    FunTypeContext *funType();
    antlr4::tree::TerminalNode *TokWhen();
    TypeGuardsContext *typeGuards();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeSigContext* typeSig();

  class  TypeGuardsContext : public antlr4::ParserRuleContext {
  public:
    TypeGuardsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TypeGuardContext *> typeGuard();
    TypeGuardContext* typeGuard(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeGuardsContext* typeGuards();

  class  TypeGuardContext : public antlr4::ParserRuleContext {
  public:
    TypeGuardContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    antlr4::tree::TerminalNode *TokParenOpen();
    TopTypesContext *topTypes();
    antlr4::tree::TerminalNode *TokParenClose();
    TokVarContext *tokVar();
    antlr4::tree::TerminalNode *TokDoubleColon();
    TopTypeContext *topType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeGuardContext* typeGuard();

  class  TopTypesContext : public antlr4::ParserRuleContext {
  public:
    TopTypesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TopTypeContext *> topType();
    TopTypeContext* topType(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TopTypesContext* topTypes();

  class  TopTypeContext : public antlr4::ParserRuleContext {
  public:
    TopTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TopType100Context *topType100();
    TokVarContext *tokVar();
    antlr4::tree::TerminalNode *TokDoubleColon();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TopTypeContext* topType();

  class  TopType100Context : public antlr4::ParserRuleContext {
  public:
    TopType100Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    Type200Context *type200();
    antlr4::tree::TerminalNode *TokBar();
    TopType100Context *topType100();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TopType100Context* topType100();

  class  Type200Context : public antlr4::ParserRuleContext {
  public:
    Type200Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<Type300Context *> type300();
    Type300Context* type300(size_t i);
    antlr4::tree::TerminalNode *TokDoubleDot();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  Type200Context* type200();

  class  Type300Context : public antlr4::ParserRuleContext {
  public:
    Type300Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    Type400Context *type400();
    Type300Context *type300();
    AddOpContext *addOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  Type300Context* type300();
  Type300Context* type300(int precedence);
  class  Type400Context : public antlr4::ParserRuleContext {
  public:
    Type400Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    Type500Context *type500();
    Type400Context *type400();
    MultOpContext *multOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  Type400Context* type400();
  Type400Context* type400(int precedence);
  class  Type500Context : public antlr4::ParserRuleContext {
  public:
    Type500Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TypeContext *type();
    UnaryOpContext *unaryOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  Type500Context* type500();

  class  TypeContext : public antlr4::ParserRuleContext {
  public:
    TypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokParenOpen();
    TopTypeContext *topType();
    antlr4::tree::TerminalNode *TokParenClose();
    TokVarContext *tokVar();
    std::vector<TokAtomContext *> tokAtom();
    TokAtomContext* tokAtom(size_t i);
    TopTypesContext *topTypes();
    antlr4::tree::TerminalNode *TokColon();
    antlr4::tree::TerminalNode *TokSquareOpen();
    antlr4::tree::TerminalNode *TokSquareClose();
    antlr4::tree::TerminalNode *TokComma();
    antlr4::tree::TerminalNode *TokEllipsis();
    antlr4::tree::TerminalNode *TokCurlyOpen();
    antlr4::tree::TerminalNode *TokCurlyClose();
    antlr4::tree::TerminalNode *TokHash();
    FieldTypesContext *fieldTypes();
    BinaryTypeContext *binaryType();
    TokIntegerContext *tokInteger();
    antlr4::tree::TerminalNode *TokFun();
    FunType100Context *funType100();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeContext* type();

  class  FunType100Context : public antlr4::ParserRuleContext {
  public:
    FunType100Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokParenOpen();
    antlr4::tree::TerminalNode *TokEllipsis();
    antlr4::tree::TerminalNode *TokParenClose();
    antlr4::tree::TerminalNode *TokRArrow();
    TopTypeContext *topType();
    FunTypeContext *funType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunType100Context* funType100();

  class  FunTypeContext : public antlr4::ParserRuleContext {
  public:
    FunTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokParenOpen();
    antlr4::tree::TerminalNode *TokParenClose();
    antlr4::tree::TerminalNode *TokRArrow();
    TopTypeContext *topType();
    TopTypesContext *topTypes();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunTypeContext* funType();

  class  FieldTypesContext : public antlr4::ParserRuleContext {
  public:
    FieldTypesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<FieldTypeContext *> fieldType();
    FieldTypeContext* fieldType(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FieldTypesContext* fieldTypes();

  class  FieldTypeContext : public antlr4::ParserRuleContext {
  public:
    FieldTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    antlr4::tree::TerminalNode *TokDoubleColon();
    TopTypeContext *topType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FieldTypeContext* fieldType();

  class  BinaryTypeContext : public antlr4::ParserRuleContext {
  public:
    BinaryTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokBinaryOpen();
    antlr4::tree::TerminalNode *TokBinaryClose();
    BinBaseTypeContext *binBaseType();
    BinUnitTypeContext *binUnitType();
    antlr4::tree::TerminalNode *TokComma();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinaryTypeContext* binaryType();

  class  BinBaseTypeContext : public antlr4::ParserRuleContext {
  public:
    BinBaseTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokVarContext *tokVar();
    antlr4::tree::TerminalNode *TokColon();
    TypeContext *type();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinBaseTypeContext* binBaseType();

  class  BinUnitTypeContext : public antlr4::ParserRuleContext {
  public:
    BinUnitTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TokVarContext *> tokVar();
    TokVarContext* tokVar(size_t i);
    antlr4::tree::TerminalNode *TokColon();
    antlr4::tree::TerminalNode *TokStar();
    TypeContext *type();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinUnitTypeContext* binUnitType();

  class  AttrValContext : public antlr4::ParserRuleContext {
  public:
    AttrValContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokParenOpen();
    antlr4::tree::TerminalNode *TokParenClose();
    antlr4::tree::TerminalNode *TokComma();
    CommaSeparatedExprsContext *commaSeparatedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AttrValContext* attrVal();

  class  FunctionContext : public antlr4::ParserRuleContext {
  public:
    FunctionContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<FunctionClauseContext *> functionClause();
    FunctionClauseContext* functionClause(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunctionContext* function();

  class  FunctionClauseContext : public antlr4::ParserRuleContext {
  public:
    FunctionClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    ClauseArgsContext *clauseArgs();
    ClauseGuardContext *clauseGuard();
    ClauseBodyContext *clauseBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunctionClauseContext* functionClause();

  class  ClauseArgsContext : public antlr4::ParserRuleContext {
  public:
    ClauseArgsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ArgumentListContext *argumentList();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ClauseArgsContext* clauseArgs();

  class  ClauseGuardContext : public antlr4::ParserRuleContext {
  public:
    ClauseGuardContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokWhen();
    GuardContext *guard();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ClauseGuardContext* clauseGuard();

  class  ClauseBodyContext : public antlr4::ParserRuleContext {
  public:
    ClauseBodyContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokRArrow();
    CommaSeparatedExprsContext *commaSeparatedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ClauseBodyContext* clauseBody();

  class  ExpressionContext : public antlr4::ParserRuleContext {
  public:
    ExpressionContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokCatch();
    ExpressionContext *expression();
    MatchExprContext *matchExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ExpressionContext* expression();

  class  MatchExprContext : public antlr4::ParserRuleContext {
  public:
    MatchExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<OrelseExprContext *> orelseExpr();
    OrelseExprContext* orelseExpr(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokEq();
    antlr4::tree::TerminalNode* TokEq(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokBang();
    antlr4::tree::TerminalNode* TokBang(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  MatchExprContext* matchExpr();

  class  OrelseExprContext : public antlr4::ParserRuleContext {
  public:
    OrelseExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<AndalsoExprContext *> andalsoExpr();
    AndalsoExprContext* andalsoExpr(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokOrelse();
    antlr4::tree::TerminalNode* TokOrelse(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  OrelseExprContext* orelseExpr();

  class  AndalsoExprContext : public antlr4::ParserRuleContext {
  public:
    AndalsoExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<CompareExprContext *> compareExpr();
    CompareExprContext* compareExpr(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokAndalso();
    antlr4::tree::TerminalNode* TokAndalso(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AndalsoExprContext* andalsoExpr();

  class  CompareExprContext : public antlr4::ParserRuleContext {
  public:
    CompareExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ListExprContext *> listExpr();
    ListExprContext* listExpr(size_t i);
    CompareOpContext *compareOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CompareExprContext* compareExpr();

  class  ListExprContext : public antlr4::ParserRuleContext {
  public:
    ListExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<AddExprContext *> addExpr();
    AddExprContext* addExpr(size_t i);
    std::vector<ListOpContext *> listOp();
    ListOpContext* listOp(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListExprContext* listExpr();

  class  AddExprContext : public antlr4::ParserRuleContext {
  public:
    AddExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<MultExprContext *> multExpr();
    MultExprContext* multExpr(size_t i);
    std::vector<AddOpContext *> addOp();
    AddOpContext* addOp(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AddExprContext* addExpr();

  class  MultExprContext : public antlr4::ParserRuleContext {
  public:
    MultExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<UnaryExprContext *> unaryExpr();
    UnaryExprContext* unaryExpr(size_t i);
    std::vector<MultOpContext *> multOp();
    MultOpContext* multOp(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  MultExprContext* multExpr();

  class  UnaryExprContext : public antlr4::ParserRuleContext {
  public:
    UnaryExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    Expr700Context *expr700();
    UnaryOpContext *unaryOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  UnaryExprContext* unaryExpr();

  class  Expr700Context : public antlr4::ParserRuleContext {
  public:
    Expr700Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    FunctionCallContext *functionCall();
    RecordExprContext *recordExpr();
    ColonExprContext *colonExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  Expr700Context* expr700();

  class  ColonExprContext : public antlr4::ParserRuleContext {
  public:
    ColonExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ExprMaxContext *> exprMax();
    ExprMaxContext* exprMax(size_t i);
    antlr4::tree::TerminalNode *TokColon();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ColonExprContext* colonExpr();

  class  ExprMaxContext : public antlr4::ParserRuleContext {
  public:
    ExprMaxContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokVarContext *tokVar();
    LiteralContext *literal();
    ListContext *list();
    BinaryContext *binary();
    ListComprehensionContext *listComprehension();
    BinaryComprehensionContext *binaryComprehension();
    TupleContext *tuple();
    antlr4::tree::TerminalNode *TokParenOpen();
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokParenClose();
    antlr4::tree::TerminalNode *TokBegin();
    CommaSeparatedExprsContext *commaSeparatedExprs();
    antlr4::tree::TerminalNode *TokEnd();
    IfExprContext *ifExpr();
    CaseExprContext *caseExpr();
    ReceiveExprContext *receiveExpr();
    FunExprContext *funExpr();
    TryExprContext *tryExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ExprMaxContext* exprMax();

  class  ListContext : public antlr4::ParserRuleContext {
  public:
    ListContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokSquareOpen();
    antlr4::tree::TerminalNode *TokSquareClose();
    ExpressionContext *expression();
    TailContext *tail();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListContext* list();

  class  TailContext : public antlr4::ParserRuleContext {
  public:
    TailContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokSquareClose();
    antlr4::tree::TerminalNode *TokBar();
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokComma();
    TailContext *tail();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TailContext* tail();

  class  BinaryContext : public antlr4::ParserRuleContext {
  public:
    BinaryContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokBinaryOpen();
    antlr4::tree::TerminalNode *TokBinaryClose();
    BinElementsContext *binElements();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinaryContext* binary();

  class  BinElementsContext : public antlr4::ParserRuleContext {
  public:
    BinElementsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<BinElementContext *> binElement();
    BinElementContext* binElement(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinElementsContext* binElements();

  class  BinElementContext : public antlr4::ParserRuleContext {
  public:
    BinElementContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    BitExprContext *bitExpr();
    OptBitSizeExprContext *optBitSizeExpr();
    OptBitTypeListContext *optBitTypeList();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinElementContext* binElement();

  class  BitExprContext : public antlr4::ParserRuleContext {
  public:
    BitExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExprMaxContext *exprMax();
    UnaryOpContext *unaryOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BitExprContext* bitExpr();

  class  OptBitSizeExprContext : public antlr4::ParserRuleContext {
  public:
    OptBitSizeExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokColon();
    BitSizeExprContext *bitSizeExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  OptBitSizeExprContext* optBitSizeExpr();

  class  OptBitTypeListContext : public antlr4::ParserRuleContext {
  public:
    OptBitTypeListContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokSlash();
    BitTypeListContext *bitTypeList();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  OptBitTypeListContext* optBitTypeList();

  class  BitTypeListContext : public antlr4::ParserRuleContext {
  public:
    BitTypeListContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<BitTypeContext *> bitType();
    BitTypeContext* bitType(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokMinus();
    antlr4::tree::TerminalNode* TokMinus(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BitTypeListContext* bitTypeList();

  class  BitTypeContext : public antlr4::ParserRuleContext {
  public:
    BitTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    antlr4::tree::TerminalNode *TokColon();
    TokIntegerContext *tokInteger();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BitTypeContext* bitType();

  class  BitSizeExprContext : public antlr4::ParserRuleContext {
  public:
    BitSizeExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExprMaxContext *exprMax();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BitSizeExprContext* bitSizeExpr();

  class  ListComprehensionContext : public antlr4::ParserRuleContext {
  public:
    ListComprehensionContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokSquareOpen();
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokBarBar();
    LcExprsContext *lcExprs();
    antlr4::tree::TerminalNode *TokSquareClose();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListComprehensionContext* listComprehension();

  class  BinaryComprehensionContext : public antlr4::ParserRuleContext {
  public:
    BinaryComprehensionContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokBinaryOpen();
    BinaryContext *binary();
    antlr4::tree::TerminalNode *TokBarBar();
    LcExprsContext *lcExprs();
    antlr4::tree::TerminalNode *TokBinaryClose();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinaryComprehensionContext* binaryComprehension();

  class  LcExprsContext : public antlr4::ParserRuleContext {
  public:
    LcExprsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<LcExprContext *> lcExpr();
    LcExprContext* lcExpr(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  LcExprsContext* lcExprs();

  class  LcExprContext : public antlr4::ParserRuleContext {
  public:
    LcExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ExpressionContext *> expression();
    ExpressionContext* expression(size_t i);
    antlr4::tree::TerminalNode *TokLArrow();
    BinaryContext *binary();
    antlr4::tree::TerminalNode *TokLDoubleArrow();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  LcExprContext* lcExpr();

  class  TupleContext : public antlr4::ParserRuleContext {
  public:
    TupleContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokCurlyOpen();
    antlr4::tree::TerminalNode *TokCurlyClose();
    CommaSeparatedExprsContext *commaSeparatedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TupleContext* tuple();

  class  RecordExprContext : public antlr4::ParserRuleContext {
  public:
    RecordExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokHash();
    std::vector<TokAtomContext *> tokAtom();
    TokAtomContext* tokAtom(size_t i);
    antlr4::tree::TerminalNode *TokDot();
    RecordTupleContext *recordTuple();
    ExprMaxContext *exprMax();
    RecordExprContext *recordExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RecordExprContext* recordExpr();
  RecordExprContext* recordExpr(int precedence);
  class  RecordTupleContext : public antlr4::ParserRuleContext {
  public:
    RecordTupleContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokCurlyOpen();
    antlr4::tree::TerminalNode *TokCurlyClose();
    RecordFieldsContext *recordFields();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RecordTupleContext* recordTuple();

  class  RecordFieldsContext : public antlr4::ParserRuleContext {
  public:
    RecordFieldsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<RecordFieldContext *> recordField();
    RecordFieldContext* recordField(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RecordFieldsContext* recordFields();

  class  RecordFieldContext : public antlr4::ParserRuleContext {
  public:
    RecordFieldContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokEq();
    ExpressionContext *expression();
    TokVarContext *tokVar();
    TokAtomContext *tokAtom();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RecordFieldContext* recordField();

  class  FunctionCallContext : public antlr4::ParserRuleContext {
  public:
    FunctionCallContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ColonExprContext *colonExpr();
    ArgumentListContext *argumentList();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunctionCallContext* functionCall();

  class  IfExprContext : public antlr4::ParserRuleContext {
  public:
    IfExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokIf();
    IfClausesContext *ifClauses();
    antlr4::tree::TerminalNode *TokEnd();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  IfExprContext* ifExpr();

  class  IfClausesContext : public antlr4::ParserRuleContext {
  public:
    IfClausesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<IfClauseContext *> ifClause();
    IfClauseContext* ifClause(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  IfClausesContext* ifClauses();

  class  IfClauseContext : public antlr4::ParserRuleContext {
  public:
    IfClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    GuardContext *guard();
    ClauseBodyContext *clauseBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  IfClauseContext* ifClause();

  class  CaseExprContext : public antlr4::ParserRuleContext {
  public:
    CaseExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokCase();
    ExpressionContext *expression();
    antlr4::tree::TerminalNode *TokOf();
    CrClausesContext *crClauses();
    antlr4::tree::TerminalNode *TokEnd();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CaseExprContext* caseExpr();

  class  CrClausesContext : public antlr4::ParserRuleContext {
  public:
    CrClausesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<CrClauseContext *> crClause();
    CrClauseContext* crClause(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CrClausesContext* crClauses();

  class  CrClauseContext : public antlr4::ParserRuleContext {
  public:
    CrClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExpressionContext *expression();
    ClauseGuardContext *clauseGuard();
    ClauseBodyContext *clauseBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CrClauseContext* crClause();

  class  ReceiveExprContext : public antlr4::ParserRuleContext {
  public:
    ReceiveExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokReceive();
    CrClausesContext *crClauses();
    antlr4::tree::TerminalNode *TokEnd();
    antlr4::tree::TerminalNode *TokAfter();
    ExpressionContext *expression();
    ClauseBodyContext *clauseBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ReceiveExprContext* receiveExpr();

  class  FunExprContext : public antlr4::ParserRuleContext {
  public:
    FunExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokFun();
    TokAtomContext *tokAtom();
    antlr4::tree::TerminalNode *TokSlash();
    TokIntegerContext *tokInteger();
    std::vector<AtomOrVarContext *> atomOrVar();
    AtomOrVarContext* atomOrVar(size_t i);
    antlr4::tree::TerminalNode *TokColon();
    IntegerOrVarContext *integerOrVar();
    FunClausesContext *funClauses();
    antlr4::tree::TerminalNode *TokEnd();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunExprContext* funExpr();

  class  AtomOrVarContext : public antlr4::ParserRuleContext {
  public:
    AtomOrVarContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    TokVarContext *tokVar();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AtomOrVarContext* atomOrVar();

  class  IntegerOrVarContext : public antlr4::ParserRuleContext {
  public:
    IntegerOrVarContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokIntegerContext *tokInteger();
    TokVarContext *tokVar();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  IntegerOrVarContext* integerOrVar();

  class  FunClausesContext : public antlr4::ParserRuleContext {
  public:
    FunClausesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<FunClauseContext *> funClause();
    FunClauseContext* funClause(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunClausesContext* funClauses();

  class  FunClauseContext : public antlr4::ParserRuleContext {
  public:
    FunClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ArgumentListContext *argumentList();
    ClauseGuardContext *clauseGuard();
    ClauseBodyContext *clauseBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunClauseContext* funClause();

  class  TryExprContext : public antlr4::ParserRuleContext {
  public:
    TryExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokTry();
    CommaSeparatedExprsContext *commaSeparatedExprs();
    TryCatchContext *tryCatch();
    antlr4::tree::TerminalNode *TokOf();
    CrClausesContext *crClauses();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryExprContext* tryExpr();

  class  TryCatchContext : public antlr4::ParserRuleContext {
  public:
    TryCatchContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokCatch();
    TryClausesContext *tryClauses();
    antlr4::tree::TerminalNode *TokEnd();
    antlr4::tree::TerminalNode *TokAfter();
    CommaSeparatedExprsContext *commaSeparatedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryCatchContext* tryCatch();

  class  TryClausesContext : public antlr4::ParserRuleContext {
  public:
    TryClausesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TryClauseContext *> tryClause();
    TryClauseContext* tryClause(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryClausesContext* tryClauses();

  class  TryClauseContext : public antlr4::ParserRuleContext {
  public:
    TryClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExpressionContext *expression();
    ClauseGuardContext *clauseGuard();
    ClauseBodyContext *clauseBody();
    AtomOrVarContext *atomOrVar();
    antlr4::tree::TerminalNode *TokColon();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryClauseContext* tryClause();

  class  ArgumentListContext : public antlr4::ParserRuleContext {
  public:
    ArgumentListContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokParenOpen();
    antlr4::tree::TerminalNode *TokParenClose();
    CommaSeparatedExprsContext *commaSeparatedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ArgumentListContext* argumentList();

  class  CommaSeparatedExprsContext : public antlr4::ParserRuleContext {
  public:
    CommaSeparatedExprsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ExpressionContext *> expression();
    ExpressionContext* expression(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokComma();
    antlr4::tree::TerminalNode* TokComma(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CommaSeparatedExprsContext* commaSeparatedExprs();

  class  GuardContext : public antlr4::ParserRuleContext {
  public:
    GuardContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<CommaSeparatedExprsContext *> commaSeparatedExprs();
    CommaSeparatedExprsContext* commaSeparatedExprs(size_t i);
    std::vector<antlr4::tree::TerminalNode *> TokSemicolon();
    antlr4::tree::TerminalNode* TokSemicolon(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  GuardContext* guard();

  class  LiteralContext : public antlr4::ParserRuleContext {
  public:
    LiteralContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokCharContext *tokChar();
    TokIntegerContext *tokInteger();
    TokFloatContext *tokFloat();
    TokAtomContext *tokAtom();
    std::vector<TokStringContext *> tokString();
    TokStringContext* tokString(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  LiteralContext* literal();

  class  UnaryOpContext : public antlr4::ParserRuleContext {
  public:
    UnaryOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokPlus();
    antlr4::tree::TerminalNode *TokMinus();
    antlr4::tree::TerminalNode *TokBnot();
    antlr4::tree::TerminalNode *TokNot();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  UnaryOpContext* unaryOp();

  class  MultOpContext : public antlr4::ParserRuleContext {
  public:
    MultOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokSlash();
    antlr4::tree::TerminalNode *TokStar();
    antlr4::tree::TerminalNode *TokDiv();
    antlr4::tree::TerminalNode *TokRem();
    antlr4::tree::TerminalNode *TokBand();
    antlr4::tree::TerminalNode *TokAnd();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  MultOpContext* multOp();

  class  AddOpContext : public antlr4::ParserRuleContext {
  public:
    AddOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokPlus();
    antlr4::tree::TerminalNode *TokMinus();
    antlr4::tree::TerminalNode *TokBor();
    antlr4::tree::TerminalNode *TokBxor();
    antlr4::tree::TerminalNode *TokBsl();
    antlr4::tree::TerminalNode *TokBsr();
    antlr4::tree::TerminalNode *TokOr();
    antlr4::tree::TerminalNode *TokXor();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AddOpContext* addOp();

  class  ListOpContext : public antlr4::ParserRuleContext {
  public:
    ListOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokDoublePlus();
    antlr4::tree::TerminalNode *TokDoubleMinus();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListOpContext* listOp();

  class  CompareOpContext : public antlr4::ParserRuleContext {
  public:
    CompareOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    antlr4::tree::TerminalNode *TokDoubleEq();
    antlr4::tree::TerminalNode *TokNotEq();
    antlr4::tree::TerminalNode *TokLessEq();
    antlr4::tree::TerminalNode *TokLess();
    antlr4::tree::TerminalNode *TokGreaterEq();
    antlr4::tree::TerminalNode *TokGreater();
    antlr4::tree::TerminalNode *TokStrictEq();
    antlr4::tree::TerminalNode *TokStrictNeq();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CompareOpContext* compareOp();


  virtual bool sempred(antlr4::RuleContext *_localctx, size_t ruleIndex, size_t predicateIndex) override;
  bool type300Sempred(Type300Context *_localctx, size_t predicateIndex);
  bool type400Sempred(Type400Context *_localctx, size_t predicateIndex);
  bool recordExprSempred(RecordExprContext *_localctx, size_t predicateIndex);

private:
  static std::vector<antlr4::dfa::DFA> _decisionToDFA;
  static antlr4::atn::PredictionContextCache _sharedContextCache;
  static std::vector<std::string> _ruleNames;
  static std::vector<std::string> _tokenNames;

  static std::vector<std::string> _literalNames;
  static std::vector<std::string> _symbolicNames;
  static antlr4::dfa::Vocabulary _vocabulary;
  static antlr4::atn::ATN _atn;
  static std::vector<uint16_t> _serializedATN;


  struct Initializer {
    Initializer();
  };
  static Initializer _init;
};

