
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"
#include "ErlangListener.h"


/**
 * This class provides an empty implementation of ErlangListener,
 * which can be extended to create a listener which only needs to handle a subset
 * of the available methods.
 */
class  ErlangBaseListener : public ErlangListener {
public:

  virtual void enterForms(ErlangParser::FormsContext * /*ctx*/) override { }
  virtual void exitForms(ErlangParser::FormsContext * /*ctx*/) override { }

  virtual void enterForm(ErlangParser::FormContext * /*ctx*/) override { }
  virtual void exitForm(ErlangParser::FormContext * /*ctx*/) override { }

  virtual void enterTokAtom(ErlangParser::TokAtomContext * /*ctx*/) override { }
  virtual void exitTokAtom(ErlangParser::TokAtomContext * /*ctx*/) override { }

  virtual void enterTokVar(ErlangParser::TokVarContext * /*ctx*/) override { }
  virtual void exitTokVar(ErlangParser::TokVarContext * /*ctx*/) override { }

  virtual void enterTokFloat(ErlangParser::TokFloatContext * /*ctx*/) override { }
  virtual void exitTokFloat(ErlangParser::TokFloatContext * /*ctx*/) override { }

  virtual void enterTokInteger(ErlangParser::TokIntegerContext * /*ctx*/) override { }
  virtual void exitTokInteger(ErlangParser::TokIntegerContext * /*ctx*/) override { }

  virtual void enterTokChar(ErlangParser::TokCharContext * /*ctx*/) override { }
  virtual void exitTokChar(ErlangParser::TokCharContext * /*ctx*/) override { }

  virtual void enterTokString(ErlangParser::TokStringContext * /*ctx*/) override { }
  virtual void exitTokString(ErlangParser::TokStringContext * /*ctx*/) override { }

  virtual void enterAttribute(ErlangParser::AttributeContext * /*ctx*/) override { }
  virtual void exitAttribute(ErlangParser::AttributeContext * /*ctx*/) override { }

  virtual void enterTypeSpec(ErlangParser::TypeSpecContext * /*ctx*/) override { }
  virtual void exitTypeSpec(ErlangParser::TypeSpecContext * /*ctx*/) override { }

  virtual void enterSpecFun(ErlangParser::SpecFunContext * /*ctx*/) override { }
  virtual void exitSpecFun(ErlangParser::SpecFunContext * /*ctx*/) override { }

  virtual void enterTypedAttrVal(ErlangParser::TypedAttrValContext * /*ctx*/) override { }
  virtual void exitTypedAttrVal(ErlangParser::TypedAttrValContext * /*ctx*/) override { }

  virtual void enterTypedRecordFields(ErlangParser::TypedRecordFieldsContext * /*ctx*/) override { }
  virtual void exitTypedRecordFields(ErlangParser::TypedRecordFieldsContext * /*ctx*/) override { }

  virtual void enterTypedExprs(ErlangParser::TypedExprsContext * /*ctx*/) override { }
  virtual void exitTypedExprs(ErlangParser::TypedExprsContext * /*ctx*/) override { }

  virtual void enterTypedExpr(ErlangParser::TypedExprContext * /*ctx*/) override { }
  virtual void exitTypedExpr(ErlangParser::TypedExprContext * /*ctx*/) override { }

  virtual void enterTypeSigs(ErlangParser::TypeSigsContext * /*ctx*/) override { }
  virtual void exitTypeSigs(ErlangParser::TypeSigsContext * /*ctx*/) override { }

  virtual void enterTypeSig(ErlangParser::TypeSigContext * /*ctx*/) override { }
  virtual void exitTypeSig(ErlangParser::TypeSigContext * /*ctx*/) override { }

  virtual void enterTypeGuards(ErlangParser::TypeGuardsContext * /*ctx*/) override { }
  virtual void exitTypeGuards(ErlangParser::TypeGuardsContext * /*ctx*/) override { }

  virtual void enterTypeGuard(ErlangParser::TypeGuardContext * /*ctx*/) override { }
  virtual void exitTypeGuard(ErlangParser::TypeGuardContext * /*ctx*/) override { }

  virtual void enterTopTypes(ErlangParser::TopTypesContext * /*ctx*/) override { }
  virtual void exitTopTypes(ErlangParser::TopTypesContext * /*ctx*/) override { }

  virtual void enterTopType(ErlangParser::TopTypeContext * /*ctx*/) override { }
  virtual void exitTopType(ErlangParser::TopTypeContext * /*ctx*/) override { }

  virtual void enterTopType100(ErlangParser::TopType100Context * /*ctx*/) override { }
  virtual void exitTopType100(ErlangParser::TopType100Context * /*ctx*/) override { }

  virtual void enterType200(ErlangParser::Type200Context * /*ctx*/) override { }
  virtual void exitType200(ErlangParser::Type200Context * /*ctx*/) override { }

  virtual void enterType300(ErlangParser::Type300Context * /*ctx*/) override { }
  virtual void exitType300(ErlangParser::Type300Context * /*ctx*/) override { }

  virtual void enterType400(ErlangParser::Type400Context * /*ctx*/) override { }
  virtual void exitType400(ErlangParser::Type400Context * /*ctx*/) override { }

  virtual void enterType500(ErlangParser::Type500Context * /*ctx*/) override { }
  virtual void exitType500(ErlangParser::Type500Context * /*ctx*/) override { }

  virtual void enterType(ErlangParser::TypeContext * /*ctx*/) override { }
  virtual void exitType(ErlangParser::TypeContext * /*ctx*/) override { }

  virtual void enterFunType100(ErlangParser::FunType100Context * /*ctx*/) override { }
  virtual void exitFunType100(ErlangParser::FunType100Context * /*ctx*/) override { }

  virtual void enterFunType(ErlangParser::FunTypeContext * /*ctx*/) override { }
  virtual void exitFunType(ErlangParser::FunTypeContext * /*ctx*/) override { }

  virtual void enterFieldTypes(ErlangParser::FieldTypesContext * /*ctx*/) override { }
  virtual void exitFieldTypes(ErlangParser::FieldTypesContext * /*ctx*/) override { }

  virtual void enterFieldType(ErlangParser::FieldTypeContext * /*ctx*/) override { }
  virtual void exitFieldType(ErlangParser::FieldTypeContext * /*ctx*/) override { }

  virtual void enterBinaryType(ErlangParser::BinaryTypeContext * /*ctx*/) override { }
  virtual void exitBinaryType(ErlangParser::BinaryTypeContext * /*ctx*/) override { }

  virtual void enterBinBaseType(ErlangParser::BinBaseTypeContext * /*ctx*/) override { }
  virtual void exitBinBaseType(ErlangParser::BinBaseTypeContext * /*ctx*/) override { }

  virtual void enterBinUnitType(ErlangParser::BinUnitTypeContext * /*ctx*/) override { }
  virtual void exitBinUnitType(ErlangParser::BinUnitTypeContext * /*ctx*/) override { }

  virtual void enterAttrVal(ErlangParser::AttrValContext * /*ctx*/) override { }
  virtual void exitAttrVal(ErlangParser::AttrValContext * /*ctx*/) override { }

  virtual void enterFunction(ErlangParser::FunctionContext * /*ctx*/) override { }
  virtual void exitFunction(ErlangParser::FunctionContext * /*ctx*/) override { }

  virtual void enterFunctionClause(ErlangParser::FunctionClauseContext * /*ctx*/) override { }
  virtual void exitFunctionClause(ErlangParser::FunctionClauseContext * /*ctx*/) override { }

  virtual void enterClauseArgs(ErlangParser::ClauseArgsContext * /*ctx*/) override { }
  virtual void exitClauseArgs(ErlangParser::ClauseArgsContext * /*ctx*/) override { }

  virtual void enterClauseGuard(ErlangParser::ClauseGuardContext * /*ctx*/) override { }
  virtual void exitClauseGuard(ErlangParser::ClauseGuardContext * /*ctx*/) override { }

  virtual void enterClauseBody(ErlangParser::ClauseBodyContext * /*ctx*/) override { }
  virtual void exitClauseBody(ErlangParser::ClauseBodyContext * /*ctx*/) override { }

  virtual void enterCatchExpr(ErlangParser::CatchExprContext * /*ctx*/) override { }
  virtual void exitCatchExpr(ErlangParser::CatchExprContext * /*ctx*/) override { }

  virtual void enterMatchbangExpr(ErlangParser::MatchbangExprContext * /*ctx*/) override { }
  virtual void exitMatchbangExpr(ErlangParser::MatchbangExprContext * /*ctx*/) override { }

  virtual void enterOrelseExpr(ErlangParser::OrelseExprContext * /*ctx*/) override { }
  virtual void exitOrelseExpr(ErlangParser::OrelseExprContext * /*ctx*/) override { }

  virtual void enterAndalsoExpr(ErlangParser::AndalsoExprContext * /*ctx*/) override { }
  virtual void exitAndalsoExpr(ErlangParser::AndalsoExprContext * /*ctx*/) override { }

  virtual void enterCompareExpr(ErlangParser::CompareExprContext * /*ctx*/) override { }
  virtual void exitCompareExpr(ErlangParser::CompareExprContext * /*ctx*/) override { }

  virtual void enterListExpr(ErlangParser::ListExprContext * /*ctx*/) override { }
  virtual void exitListExpr(ErlangParser::ListExprContext * /*ctx*/) override { }

  virtual void enterAddExpr(ErlangParser::AddExprContext * /*ctx*/) override { }
  virtual void exitAddExpr(ErlangParser::AddExprContext * /*ctx*/) override { }

  virtual void enterMultExpr(ErlangParser::MultExprContext * /*ctx*/) override { }
  virtual void exitMultExpr(ErlangParser::MultExprContext * /*ctx*/) override { }

  virtual void enterUnaryExpr(ErlangParser::UnaryExprContext * /*ctx*/) override { }
  virtual void exitUnaryExpr(ErlangParser::UnaryExprContext * /*ctx*/) override { }

  virtual void enterExpr700(ErlangParser::Expr700Context * /*ctx*/) override { }
  virtual void exitExpr700(ErlangParser::Expr700Context * /*ctx*/) override { }

  virtual void enterSemicolonExpr(ErlangParser::SemicolonExprContext * /*ctx*/) override { }
  virtual void exitSemicolonExpr(ErlangParser::SemicolonExprContext * /*ctx*/) override { }

  virtual void enterExprMax(ErlangParser::ExprMaxContext * /*ctx*/) override { }
  virtual void exitExprMax(ErlangParser::ExprMaxContext * /*ctx*/) override { }

  virtual void enterList(ErlangParser::ListContext * /*ctx*/) override { }
  virtual void exitList(ErlangParser::ListContext * /*ctx*/) override { }

  virtual void enterTail(ErlangParser::TailContext * /*ctx*/) override { }
  virtual void exitTail(ErlangParser::TailContext * /*ctx*/) override { }

  virtual void enterBinary(ErlangParser::BinaryContext * /*ctx*/) override { }
  virtual void exitBinary(ErlangParser::BinaryContext * /*ctx*/) override { }

  virtual void enterBinElements(ErlangParser::BinElementsContext * /*ctx*/) override { }
  virtual void exitBinElements(ErlangParser::BinElementsContext * /*ctx*/) override { }

  virtual void enterBinElement(ErlangParser::BinElementContext * /*ctx*/) override { }
  virtual void exitBinElement(ErlangParser::BinElementContext * /*ctx*/) override { }

  virtual void enterBitExpr(ErlangParser::BitExprContext * /*ctx*/) override { }
  virtual void exitBitExpr(ErlangParser::BitExprContext * /*ctx*/) override { }

  virtual void enterOptBitSizeExpr(ErlangParser::OptBitSizeExprContext * /*ctx*/) override { }
  virtual void exitOptBitSizeExpr(ErlangParser::OptBitSizeExprContext * /*ctx*/) override { }

  virtual void enterOptBitTypeList(ErlangParser::OptBitTypeListContext * /*ctx*/) override { }
  virtual void exitOptBitTypeList(ErlangParser::OptBitTypeListContext * /*ctx*/) override { }

  virtual void enterBitTypeList(ErlangParser::BitTypeListContext * /*ctx*/) override { }
  virtual void exitBitTypeList(ErlangParser::BitTypeListContext * /*ctx*/) override { }

  virtual void enterBitType(ErlangParser::BitTypeContext * /*ctx*/) override { }
  virtual void exitBitType(ErlangParser::BitTypeContext * /*ctx*/) override { }

  virtual void enterBitSizeExpr(ErlangParser::BitSizeExprContext * /*ctx*/) override { }
  virtual void exitBitSizeExpr(ErlangParser::BitSizeExprContext * /*ctx*/) override { }

  virtual void enterListComprehension(ErlangParser::ListComprehensionContext * /*ctx*/) override { }
  virtual void exitListComprehension(ErlangParser::ListComprehensionContext * /*ctx*/) override { }

  virtual void enterBinaryComprehension(ErlangParser::BinaryComprehensionContext * /*ctx*/) override { }
  virtual void exitBinaryComprehension(ErlangParser::BinaryComprehensionContext * /*ctx*/) override { }

  virtual void enterLcExprs(ErlangParser::LcExprsContext * /*ctx*/) override { }
  virtual void exitLcExprs(ErlangParser::LcExprsContext * /*ctx*/) override { }

  virtual void enterLcExpr(ErlangParser::LcExprContext * /*ctx*/) override { }
  virtual void exitLcExpr(ErlangParser::LcExprContext * /*ctx*/) override { }

  virtual void enterTuple(ErlangParser::TupleContext * /*ctx*/) override { }
  virtual void exitTuple(ErlangParser::TupleContext * /*ctx*/) override { }

  virtual void enterRecordExpr(ErlangParser::RecordExprContext * /*ctx*/) override { }
  virtual void exitRecordExpr(ErlangParser::RecordExprContext * /*ctx*/) override { }

  virtual void enterRecordTuple(ErlangParser::RecordTupleContext * /*ctx*/) override { }
  virtual void exitRecordTuple(ErlangParser::RecordTupleContext * /*ctx*/) override { }

  virtual void enterRecordFields(ErlangParser::RecordFieldsContext * /*ctx*/) override { }
  virtual void exitRecordFields(ErlangParser::RecordFieldsContext * /*ctx*/) override { }

  virtual void enterRecordField(ErlangParser::RecordFieldContext * /*ctx*/) override { }
  virtual void exitRecordField(ErlangParser::RecordFieldContext * /*ctx*/) override { }

  virtual void enterFunctionCall(ErlangParser::FunctionCallContext * /*ctx*/) override { }
  virtual void exitFunctionCall(ErlangParser::FunctionCallContext * /*ctx*/) override { }

  virtual void enterIfExpr(ErlangParser::IfExprContext * /*ctx*/) override { }
  virtual void exitIfExpr(ErlangParser::IfExprContext * /*ctx*/) override { }

  virtual void enterIfClauses(ErlangParser::IfClausesContext * /*ctx*/) override { }
  virtual void exitIfClauses(ErlangParser::IfClausesContext * /*ctx*/) override { }

  virtual void enterIfClause(ErlangParser::IfClauseContext * /*ctx*/) override { }
  virtual void exitIfClause(ErlangParser::IfClauseContext * /*ctx*/) override { }

  virtual void enterCaseExpr(ErlangParser::CaseExprContext * /*ctx*/) override { }
  virtual void exitCaseExpr(ErlangParser::CaseExprContext * /*ctx*/) override { }

  virtual void enterCrClauses(ErlangParser::CrClausesContext * /*ctx*/) override { }
  virtual void exitCrClauses(ErlangParser::CrClausesContext * /*ctx*/) override { }

  virtual void enterCrClause(ErlangParser::CrClauseContext * /*ctx*/) override { }
  virtual void exitCrClause(ErlangParser::CrClauseContext * /*ctx*/) override { }

  virtual void enterReceiveExpr(ErlangParser::ReceiveExprContext * /*ctx*/) override { }
  virtual void exitReceiveExpr(ErlangParser::ReceiveExprContext * /*ctx*/) override { }

  virtual void enterFunExpr(ErlangParser::FunExprContext * /*ctx*/) override { }
  virtual void exitFunExpr(ErlangParser::FunExprContext * /*ctx*/) override { }

  virtual void enterAtomOrVar(ErlangParser::AtomOrVarContext * /*ctx*/) override { }
  virtual void exitAtomOrVar(ErlangParser::AtomOrVarContext * /*ctx*/) override { }

  virtual void enterIntegerOrVar(ErlangParser::IntegerOrVarContext * /*ctx*/) override { }
  virtual void exitIntegerOrVar(ErlangParser::IntegerOrVarContext * /*ctx*/) override { }

  virtual void enterFunClauses(ErlangParser::FunClausesContext * /*ctx*/) override { }
  virtual void exitFunClauses(ErlangParser::FunClausesContext * /*ctx*/) override { }

  virtual void enterFunClause(ErlangParser::FunClauseContext * /*ctx*/) override { }
  virtual void exitFunClause(ErlangParser::FunClauseContext * /*ctx*/) override { }

  virtual void enterTryExpr(ErlangParser::TryExprContext * /*ctx*/) override { }
  virtual void exitTryExpr(ErlangParser::TryExprContext * /*ctx*/) override { }

  virtual void enterTryCatch(ErlangParser::TryCatchContext * /*ctx*/) override { }
  virtual void exitTryCatch(ErlangParser::TryCatchContext * /*ctx*/) override { }

  virtual void enterTryClauses(ErlangParser::TryClausesContext * /*ctx*/) override { }
  virtual void exitTryClauses(ErlangParser::TryClausesContext * /*ctx*/) override { }

  virtual void enterTryClause(ErlangParser::TryClauseContext * /*ctx*/) override { }
  virtual void exitTryClause(ErlangParser::TryClauseContext * /*ctx*/) override { }

  virtual void enterArgumentList(ErlangParser::ArgumentListContext * /*ctx*/) override { }
  virtual void exitArgumentList(ErlangParser::ArgumentListContext * /*ctx*/) override { }

  virtual void enterExprs(ErlangParser::ExprsContext * /*ctx*/) override { }
  virtual void exitExprs(ErlangParser::ExprsContext * /*ctx*/) override { }

  virtual void enterGuard(ErlangParser::GuardContext * /*ctx*/) override { }
  virtual void exitGuard(ErlangParser::GuardContext * /*ctx*/) override { }

  virtual void enterAtomic(ErlangParser::AtomicContext * /*ctx*/) override { }
  virtual void exitAtomic(ErlangParser::AtomicContext * /*ctx*/) override { }

  virtual void enterUnaryOp(ErlangParser::UnaryOpContext * /*ctx*/) override { }
  virtual void exitUnaryOp(ErlangParser::UnaryOpContext * /*ctx*/) override { }

  virtual void enterMultOp(ErlangParser::MultOpContext * /*ctx*/) override { }
  virtual void exitMultOp(ErlangParser::MultOpContext * /*ctx*/) override { }

  virtual void enterAddOp(ErlangParser::AddOpContext * /*ctx*/) override { }
  virtual void exitAddOp(ErlangParser::AddOpContext * /*ctx*/) override { }

  virtual void enterListOp(ErlangParser::ListOpContext * /*ctx*/) override { }
  virtual void exitListOp(ErlangParser::ListOpContext * /*ctx*/) override { }

  virtual void enterCompareOp(ErlangParser::CompareOpContext * /*ctx*/) override { }
  virtual void exitCompareOp(ErlangParser::CompareOpContext * /*ctx*/) override { }

  virtual void enterRuleClauses(ErlangParser::RuleClausesContext * /*ctx*/) override { }
  virtual void exitRuleClauses(ErlangParser::RuleClausesContext * /*ctx*/) override { }

  virtual void enterRuleClause(ErlangParser::RuleClauseContext * /*ctx*/) override { }
  virtual void exitRuleClause(ErlangParser::RuleClauseContext * /*ctx*/) override { }

  virtual void enterRuleBody(ErlangParser::RuleBodyContext * /*ctx*/) override { }
  virtual void exitRuleBody(ErlangParser::RuleBodyContext * /*ctx*/) override { }


  virtual void enterEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void exitEveryRule(antlr4::ParserRuleContext * /*ctx*/) override { }
  virtual void visitTerminal(antlr4::tree::TerminalNode * /*node*/) override { }
  virtual void visitErrorNode(antlr4::tree::ErrorNode * /*node*/) override { }

};

