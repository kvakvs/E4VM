
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"




class  ErlangParser : public antlr4::Parser {
public:
  enum {
    T__0 = 1, T__1 = 2, T__2 = 3, T__3 = 4, T__4 = 5, T__5 = 6, T__6 = 7, 
    T__7 = 8, T__8 = 9, T__9 = 10, T__10 = 11, T__11 = 12, T__12 = 13, T__13 = 14, 
    T__14 = 15, T__15 = 16, T__16 = 17, T__17 = 18, T__18 = 19, T__19 = 20, 
    T__20 = 21, T__21 = 22, T__22 = 23, T__23 = 24, T__24 = 25, T__25 = 26, 
    T__26 = 27, T__27 = 28, T__28 = 29, T__29 = 30, T__30 = 31, T__31 = 32, 
    T__32 = 33, T__33 = 34, T__34 = 35, T__35 = 36, T__36 = 37, T__37 = 38, 
    T__38 = 39, T__39 = 40, T__40 = 41, T__41 = 42, T__42 = 43, T__43 = 44, 
    T__44 = 45, T__45 = 46, T__46 = 47, T__47 = 48, T__48 = 49, T__49 = 50, 
    T__50 = 51, T__51 = 52, T__52 = 53, T__53 = 54, T__54 = 55, T__55 = 56, 
    T__56 = 57, T__57 = 58, T__58 = 59, T__59 = 60, T__60 = 61, T__61 = 62, 
    T__62 = 63, TokAtom = 64, TokVar = 65, TokFloat = 66, TokInteger = 67, 
    TokChar = 68, TokString = 69, AttrName = 70, Comment = 71, WS = 72
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
    RuleClauseArgs = 37, RuleClauseGuard = 38, RuleClauseBody = 39, RuleCatchExpr = 40, 
    RuleMatchbangExpr = 41, RuleOrelseExpr = 42, RuleAndalsoExpr = 43, RuleCompareExpr = 44, 
    RuleListExpr = 45, RuleAddExpr = 46, RuleMultExpr = 47, RuleUnaryExpr = 48, 
    RuleExpr700 = 49, RuleSemicolonExpr = 50, RuleExprMax = 51, RuleList = 52, 
    RuleTail = 53, RuleBinary = 54, RuleBinElements = 55, RuleBinElement = 56, 
    RuleBitExpr = 57, RuleOptBitSizeExpr = 58, RuleOptBitTypeList = 59, 
    RuleBitTypeList = 60, RuleBitType = 61, RuleBitSizeExpr = 62, RuleListComprehension = 63, 
    RuleBinaryComprehension = 64, RuleLcExprs = 65, RuleLcExpr = 66, RuleTuple = 67, 
    RuleRecordExpr = 68, RuleRecordTuple = 69, RuleRecordFields = 70, RuleRecordField = 71, 
    RuleFunctionCall = 72, RuleIfExpr = 73, RuleIfClauses = 74, RuleIfClause = 75, 
    RuleCaseExpr = 76, RuleCrClauses = 77, RuleCrClause = 78, RuleReceiveExpr = 79, 
    RuleFunExpr = 80, RuleAtomOrVar = 81, RuleIntegerOrVar = 82, RuleFunClauses = 83, 
    RuleFunClause = 84, RuleTryExpr = 85, RuleTryCatch = 86, RuleTryClauses = 87, 
    RuleTryClause = 88, RuleArgumentList = 89, RuleExprs = 90, RuleGuard = 91, 
    RuleAtomic = 92, RuleUnaryOp = 93, RuleMultOp = 94, RuleAddOp = 95, 
    RuleListOp = 96, RuleCompareOp = 97, RuleRuleClauses = 98, RuleRuleClause = 99, 
    RuleRuleBody = 100
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
  class CatchExprContext;
  class MatchbangExprContext;
  class OrelseExprContext;
  class AndalsoExprContext;
  class CompareExprContext;
  class ListExprContext;
  class AddExprContext;
  class MultExprContext;
  class UnaryExprContext;
  class Expr700Context;
  class SemicolonExprContext;
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
  class ExprsContext;
  class GuardContext;
  class AtomicContext;
  class UnaryOpContext;
  class MultOpContext;
  class AddOpContext;
  class ListOpContext;
  class CompareOpContext;
  class RuleClausesContext;
  class RuleClauseContext;
  class RuleBodyContext; 

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
    AttributeContext *attribute();
    FunctionContext *function();
    RuleClausesContext *ruleClauses();

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
    TokAtomContext *tokAtom();
    AttrValContext *attrVal();
    TypedAttrValContext *typedAttrVal();
    antlr4::tree::TerminalNode *AttrName();
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
    TokIntegerContext *tokInteger();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  SpecFunContext* specFun();

  class  TypedAttrValContext : public antlr4::ParserRuleContext {
  public:
    TypedAttrValContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
    TypedRecordFieldsContext *typedRecordFields();
    TopTypeContext *topType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedAttrValContext* typedAttrVal();

  class  TypedRecordFieldsContext : public antlr4::ParserRuleContext {
  public:
    TypedRecordFieldsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TypedExprsContext *typedExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedRecordFieldsContext* typedRecordFields();

  class  TypedExprsContext : public antlr4::ParserRuleContext {
  public:
    TypedExprsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TypedExprContext *typedExpr();
    TypedExprsContext *typedExprs();
    CatchExprContext *catchExpr();
    ExprsContext *exprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypedExprsContext* typedExprs();

  class  TypedExprContext : public antlr4::ParserRuleContext {
  public:
    TypedExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeSigsContext* typeSigs();

  class  TypeSigContext : public antlr4::ParserRuleContext {
  public:
    TypeSigContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    FunTypeContext *funType();
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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeGuardsContext* typeGuards();

  class  TypeGuardContext : public antlr4::ParserRuleContext {
  public:
    TypeGuardContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    TopTypesContext *topTypes();
    TokVarContext *tokVar();
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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TopTypeContext* topType();

  class  TopType100Context : public antlr4::ParserRuleContext {
  public:
    TopType100Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    Type200Context *type200();
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
    TopTypeContext *topType();
    TokVarContext *tokVar();
    std::vector<TokAtomContext *> tokAtom();
    TokAtomContext* tokAtom(size_t i);
    TopTypesContext *topTypes();
    FieldTypesContext *fieldTypes();
    BinaryTypeContext *binaryType();
    TokIntegerContext *tokInteger();
    FunType100Context *funType100();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TypeContext* type();

  class  FunType100Context : public antlr4::ParserRuleContext {
  public:
    FunType100Context(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FieldTypesContext* fieldTypes();

  class  FieldTypeContext : public antlr4::ParserRuleContext {
  public:
    FieldTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    TopTypeContext *topType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FieldTypeContext* fieldType();

  class  BinaryTypeContext : public antlr4::ParserRuleContext {
  public:
    BinaryTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    BinBaseTypeContext *binBaseType();
    BinUnitTypeContext *binUnitType();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinaryTypeContext* binaryType();

  class  BinBaseTypeContext : public antlr4::ParserRuleContext {
  public:
    BinBaseTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokVarContext *tokVar();
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
    TypeContext *type();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BinUnitTypeContext* binUnitType();

  class  AttrValContext : public antlr4::ParserRuleContext {
  public:
    AttrValContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
    ExprsContext *exprs();

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
    GuardContext *guard();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ClauseGuardContext* clauseGuard();

  class  ClauseBodyContext : public antlr4::ParserRuleContext {
  public:
    ClauseBodyContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExprsContext *exprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ClauseBodyContext* clauseBody();

  class  CatchExprContext : public antlr4::ParserRuleContext {
  public:
    CatchExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
    MatchbangExprContext *matchbangExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CatchExprContext* catchExpr();

  class  MatchbangExprContext : public antlr4::ParserRuleContext {
  public:
    MatchbangExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    OrelseExprContext *orelseExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  MatchbangExprContext* matchbangExpr();

  class  OrelseExprContext : public antlr4::ParserRuleContext {
  public:
    OrelseExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    AndalsoExprContext *andalsoExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  OrelseExprContext* orelseExpr();

  class  AndalsoExprContext : public antlr4::ParserRuleContext {
  public:
    AndalsoExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CompareExprContext *compareExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AndalsoExprContext* andalsoExpr();

  class  CompareExprContext : public antlr4::ParserRuleContext {
  public:
    CompareExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ListExprContext *listExpr();
    CompareExprContext *compareExpr();
    CompareOpContext *compareOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CompareExprContext* compareExpr();
  CompareExprContext* compareExpr(int precedence);
  class  ListExprContext : public antlr4::ParserRuleContext {
  public:
    ListExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    AddExprContext *addExpr();
    ListExprContext *listExpr();
    ListOpContext *listOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListExprContext* listExpr();
  ListExprContext* listExpr(int precedence);
  class  AddExprContext : public antlr4::ParserRuleContext {
  public:
    AddExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    MultExprContext *multExpr();
    AddExprContext *addExpr();
    AddOpContext *addOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AddExprContext* addExpr();
  AddExprContext* addExpr(int precedence);
  class  MultExprContext : public antlr4::ParserRuleContext {
  public:
    MultExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    UnaryExprContext *unaryExpr();
    MultExprContext *multExpr();
    MultOpContext *multOp();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  MultExprContext* multExpr();
  MultExprContext* multExpr(int precedence);
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
    SemicolonExprContext *semicolonExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  Expr700Context* expr700();

  class  SemicolonExprContext : public antlr4::ParserRuleContext {
  public:
    SemicolonExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ExprMaxContext *> exprMax();
    ExprMaxContext* exprMax(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  SemicolonExprContext* semicolonExpr();

  class  ExprMaxContext : public antlr4::ParserRuleContext {
  public:
    ExprMaxContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokVarContext *tokVar();
    AtomicContext *atomic();
    ListContext *list();
    BinaryContext *binary();
    ListComprehensionContext *listComprehension();
    BinaryComprehensionContext *binaryComprehension();
    TupleContext *tuple();
    CatchExprContext *catchExpr();
    ExprsContext *exprs();
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
    CatchExprContext *catchExpr();
    TailContext *tail();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListContext* list();

  class  TailContext : public antlr4::ParserRuleContext {
  public:
    TailContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
    TailContext *tail();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TailContext* tail();

  class  BinaryContext : public antlr4::ParserRuleContext {
  public:
    BinaryContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
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
    BitSizeExprContext *bitSizeExpr();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  OptBitSizeExprContext* optBitSizeExpr();

  class  OptBitTypeListContext : public antlr4::ParserRuleContext {
  public:
    OptBitTypeListContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  BitTypeListContext* bitTypeList();

  class  BitTypeContext : public antlr4::ParserRuleContext {
  public:
    BitTypeContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
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
    CatchExprContext *catchExpr();
    LcExprsContext *lcExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListComprehensionContext* listComprehension();

  class  BinaryComprehensionContext : public antlr4::ParserRuleContext {
  public:
    BinaryComprehensionContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    BinaryContext *binary();
    LcExprsContext *lcExprs();

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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  LcExprsContext* lcExprs();

  class  LcExprContext : public antlr4::ParserRuleContext {
  public:
    LcExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<CatchExprContext *> catchExpr();
    CatchExprContext* catchExpr(size_t i);
    BinaryContext *binary();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  LcExprContext* lcExpr();

  class  TupleContext : public antlr4::ParserRuleContext {
  public:
    TupleContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExprsContext *exprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TupleContext* tuple();

  class  RecordExprContext : public antlr4::ParserRuleContext {
  public:
    RecordExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<TokAtomContext *> tokAtom();
    TokAtomContext* tokAtom(size_t i);
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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RecordFieldsContext* recordFields();

  class  RecordFieldContext : public antlr4::ParserRuleContext {
  public:
    RecordFieldContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
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
    SemicolonExprContext *semicolonExpr();
    ArgumentListContext *argumentList();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  FunctionCallContext* functionCall();

  class  IfExprContext : public antlr4::ParserRuleContext {
  public:
    IfExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    IfClausesContext *ifClauses();

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
    CatchExprContext *catchExpr();
    CrClausesContext *crClauses();

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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CrClausesContext* crClauses();

  class  CrClauseContext : public antlr4::ParserRuleContext {
  public:
    CrClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
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
    CrClausesContext *crClauses();
    CatchExprContext *catchExpr();
    ClauseBodyContext *clauseBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ReceiveExprContext* receiveExpr();

  class  FunExprContext : public antlr4::ParserRuleContext {
  public:
    FunExprContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    TokIntegerContext *tokInteger();
    std::vector<AtomOrVarContext *> atomOrVar();
    AtomOrVarContext* atomOrVar(size_t i);
    IntegerOrVarContext *integerOrVar();
    FunClausesContext *funClauses();

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
    ExprsContext *exprs();
    TryCatchContext *tryCatch();
    CrClausesContext *crClauses();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryExprContext* tryExpr();

  class  TryCatchContext : public antlr4::ParserRuleContext {
  public:
    TryCatchContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TryClausesContext *tryClauses();
    ExprsContext *exprs();

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

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryClausesContext* tryClauses();

  class  TryClauseContext : public antlr4::ParserRuleContext {
  public:
    TryClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    CatchExprContext *catchExpr();
    ClauseGuardContext *clauseGuard();
    ClauseBodyContext *clauseBody();
    AtomOrVarContext *atomOrVar();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  TryClauseContext* tryClause();

  class  ArgumentListContext : public antlr4::ParserRuleContext {
  public:
    ArgumentListContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    ExprsContext *exprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ArgumentListContext* argumentList();

  class  ExprsContext : public antlr4::ParserRuleContext {
  public:
    ExprsContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<CatchExprContext *> catchExpr();
    CatchExprContext* catchExpr(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ExprsContext* exprs();

  class  GuardContext : public antlr4::ParserRuleContext {
  public:
    GuardContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<ExprsContext *> exprs();
    ExprsContext* exprs(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  GuardContext* guard();

  class  AtomicContext : public antlr4::ParserRuleContext {
  public:
    AtomicContext(antlr4::ParserRuleContext *parent, size_t invokingState);
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

  AtomicContext* atomic();

  class  UnaryOpContext : public antlr4::ParserRuleContext {
  public:
    UnaryOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  UnaryOpContext* unaryOp();

  class  MultOpContext : public antlr4::ParserRuleContext {
  public:
    MultOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  MultOpContext* multOp();

  class  AddOpContext : public antlr4::ParserRuleContext {
  public:
    AddOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  AddOpContext* addOp();

  class  ListOpContext : public antlr4::ParserRuleContext {
  public:
    ListOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  ListOpContext* listOp();

  class  CompareOpContext : public antlr4::ParserRuleContext {
  public:
    CompareOpContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  CompareOpContext* compareOp();

  class  RuleClausesContext : public antlr4::ParserRuleContext {
  public:
    RuleClausesContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<RuleClauseContext *> ruleClause();
    RuleClauseContext* ruleClause(size_t i);

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RuleClausesContext* ruleClauses();

  class  RuleClauseContext : public antlr4::ParserRuleContext {
  public:
    RuleClauseContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    TokAtomContext *tokAtom();
    ClauseArgsContext *clauseArgs();
    ClauseGuardContext *clauseGuard();
    RuleBodyContext *ruleBody();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RuleClauseContext* ruleClause();

  class  RuleBodyContext : public antlr4::ParserRuleContext {
  public:
    RuleBodyContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    LcExprsContext *lcExprs();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;
   
  };

  RuleBodyContext* ruleBody();


  virtual bool sempred(antlr4::RuleContext *_localctx, size_t ruleIndex, size_t predicateIndex) override;
  bool type300Sempred(Type300Context *_localctx, size_t predicateIndex);
  bool type400Sempred(Type400Context *_localctx, size_t predicateIndex);
  bool compareExprSempred(CompareExprContext *_localctx, size_t predicateIndex);
  bool listExprSempred(ListExprContext *_localctx, size_t predicateIndex);
  bool addExprSempred(AddExprContext *_localctx, size_t predicateIndex);
  bool multExprSempred(MultExprContext *_localctx, size_t predicateIndex);
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

