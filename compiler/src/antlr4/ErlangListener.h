
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"
#include "ErlangParser.h"


/**
 * This interface defines an abstract listener for a parse tree produced by ErlangParser.
 */
class  ErlangListener : public antlr4::tree::ParseTreeListener {
public:

  virtual void enterForms(ErlangParser::FormsContext *ctx) = 0;
  virtual void exitForms(ErlangParser::FormsContext *ctx) = 0;

  virtual void enterForm(ErlangParser::FormContext *ctx) = 0;
  virtual void exitForm(ErlangParser::FormContext *ctx) = 0;

  virtual void enterTokAtom(ErlangParser::TokAtomContext *ctx) = 0;
  virtual void exitTokAtom(ErlangParser::TokAtomContext *ctx) = 0;

  virtual void enterTokVar(ErlangParser::TokVarContext *ctx) = 0;
  virtual void exitTokVar(ErlangParser::TokVarContext *ctx) = 0;

  virtual void enterTokFloat(ErlangParser::TokFloatContext *ctx) = 0;
  virtual void exitTokFloat(ErlangParser::TokFloatContext *ctx) = 0;

  virtual void enterTokInteger(ErlangParser::TokIntegerContext *ctx) = 0;
  virtual void exitTokInteger(ErlangParser::TokIntegerContext *ctx) = 0;

  virtual void enterTokChar(ErlangParser::TokCharContext *ctx) = 0;
  virtual void exitTokChar(ErlangParser::TokCharContext *ctx) = 0;

  virtual void enterTokString(ErlangParser::TokStringContext *ctx) = 0;
  virtual void exitTokString(ErlangParser::TokStringContext *ctx) = 0;

  virtual void enterAttribute(ErlangParser::AttributeContext *ctx) = 0;
  virtual void exitAttribute(ErlangParser::AttributeContext *ctx) = 0;

  virtual void enterTypeSpec(ErlangParser::TypeSpecContext *ctx) = 0;
  virtual void exitTypeSpec(ErlangParser::TypeSpecContext *ctx) = 0;

  virtual void enterSpecFun(ErlangParser::SpecFunContext *ctx) = 0;
  virtual void exitSpecFun(ErlangParser::SpecFunContext *ctx) = 0;

  virtual void enterTypedAttrVal(ErlangParser::TypedAttrValContext *ctx) = 0;
  virtual void exitTypedAttrVal(ErlangParser::TypedAttrValContext *ctx) = 0;

  virtual void enterTypedRecordFields(ErlangParser::TypedRecordFieldsContext *ctx) = 0;
  virtual void exitTypedRecordFields(ErlangParser::TypedRecordFieldsContext *ctx) = 0;

  virtual void enterTypedExprs(ErlangParser::TypedExprsContext *ctx) = 0;
  virtual void exitTypedExprs(ErlangParser::TypedExprsContext *ctx) = 0;

  virtual void enterTypedExpr(ErlangParser::TypedExprContext *ctx) = 0;
  virtual void exitTypedExpr(ErlangParser::TypedExprContext *ctx) = 0;

  virtual void enterTypeSigs(ErlangParser::TypeSigsContext *ctx) = 0;
  virtual void exitTypeSigs(ErlangParser::TypeSigsContext *ctx) = 0;

  virtual void enterTypeSig(ErlangParser::TypeSigContext *ctx) = 0;
  virtual void exitTypeSig(ErlangParser::TypeSigContext *ctx) = 0;

  virtual void enterTypeGuards(ErlangParser::TypeGuardsContext *ctx) = 0;
  virtual void exitTypeGuards(ErlangParser::TypeGuardsContext *ctx) = 0;

  virtual void enterTypeGuard(ErlangParser::TypeGuardContext *ctx) = 0;
  virtual void exitTypeGuard(ErlangParser::TypeGuardContext *ctx) = 0;

  virtual void enterTopTypes(ErlangParser::TopTypesContext *ctx) = 0;
  virtual void exitTopTypes(ErlangParser::TopTypesContext *ctx) = 0;

  virtual void enterTopType(ErlangParser::TopTypeContext *ctx) = 0;
  virtual void exitTopType(ErlangParser::TopTypeContext *ctx) = 0;

  virtual void enterTopType100(ErlangParser::TopType100Context *ctx) = 0;
  virtual void exitTopType100(ErlangParser::TopType100Context *ctx) = 0;

  virtual void enterType200(ErlangParser::Type200Context *ctx) = 0;
  virtual void exitType200(ErlangParser::Type200Context *ctx) = 0;

  virtual void enterType300(ErlangParser::Type300Context *ctx) = 0;
  virtual void exitType300(ErlangParser::Type300Context *ctx) = 0;

  virtual void enterType400(ErlangParser::Type400Context *ctx) = 0;
  virtual void exitType400(ErlangParser::Type400Context *ctx) = 0;

  virtual void enterType500(ErlangParser::Type500Context *ctx) = 0;
  virtual void exitType500(ErlangParser::Type500Context *ctx) = 0;

  virtual void enterType(ErlangParser::TypeContext *ctx) = 0;
  virtual void exitType(ErlangParser::TypeContext *ctx) = 0;

  virtual void enterFunType100(ErlangParser::FunType100Context *ctx) = 0;
  virtual void exitFunType100(ErlangParser::FunType100Context *ctx) = 0;

  virtual void enterFunType(ErlangParser::FunTypeContext *ctx) = 0;
  virtual void exitFunType(ErlangParser::FunTypeContext *ctx) = 0;

  virtual void enterFieldTypes(ErlangParser::FieldTypesContext *ctx) = 0;
  virtual void exitFieldTypes(ErlangParser::FieldTypesContext *ctx) = 0;

  virtual void enterFieldType(ErlangParser::FieldTypeContext *ctx) = 0;
  virtual void exitFieldType(ErlangParser::FieldTypeContext *ctx) = 0;

  virtual void enterBinaryType(ErlangParser::BinaryTypeContext *ctx) = 0;
  virtual void exitBinaryType(ErlangParser::BinaryTypeContext *ctx) = 0;

  virtual void enterBinBaseType(ErlangParser::BinBaseTypeContext *ctx) = 0;
  virtual void exitBinBaseType(ErlangParser::BinBaseTypeContext *ctx) = 0;

  virtual void enterBinUnitType(ErlangParser::BinUnitTypeContext *ctx) = 0;
  virtual void exitBinUnitType(ErlangParser::BinUnitTypeContext *ctx) = 0;

  virtual void enterAttrVal(ErlangParser::AttrValContext *ctx) = 0;
  virtual void exitAttrVal(ErlangParser::AttrValContext *ctx) = 0;

  virtual void enterFunction(ErlangParser::FunctionContext *ctx) = 0;
  virtual void exitFunction(ErlangParser::FunctionContext *ctx) = 0;

  virtual void enterFunctionClause(ErlangParser::FunctionClauseContext *ctx) = 0;
  virtual void exitFunctionClause(ErlangParser::FunctionClauseContext *ctx) = 0;

  virtual void enterClauseArgs(ErlangParser::ClauseArgsContext *ctx) = 0;
  virtual void exitClauseArgs(ErlangParser::ClauseArgsContext *ctx) = 0;

  virtual void enterClauseGuard(ErlangParser::ClauseGuardContext *ctx) = 0;
  virtual void exitClauseGuard(ErlangParser::ClauseGuardContext *ctx) = 0;

  virtual void enterClauseBody(ErlangParser::ClauseBodyContext *ctx) = 0;
  virtual void exitClauseBody(ErlangParser::ClauseBodyContext *ctx) = 0;

  virtual void enterCatchExpr(ErlangParser::CatchExprContext *ctx) = 0;
  virtual void exitCatchExpr(ErlangParser::CatchExprContext *ctx) = 0;

  virtual void enterMatchbangExpr(ErlangParser::MatchbangExprContext *ctx) = 0;
  virtual void exitMatchbangExpr(ErlangParser::MatchbangExprContext *ctx) = 0;

  virtual void enterOrelseExpr(ErlangParser::OrelseExprContext *ctx) = 0;
  virtual void exitOrelseExpr(ErlangParser::OrelseExprContext *ctx) = 0;

  virtual void enterAndalsoExpr(ErlangParser::AndalsoExprContext *ctx) = 0;
  virtual void exitAndalsoExpr(ErlangParser::AndalsoExprContext *ctx) = 0;

  virtual void enterCompareExpr(ErlangParser::CompareExprContext *ctx) = 0;
  virtual void exitCompareExpr(ErlangParser::CompareExprContext *ctx) = 0;

  virtual void enterListExpr(ErlangParser::ListExprContext *ctx) = 0;
  virtual void exitListExpr(ErlangParser::ListExprContext *ctx) = 0;

  virtual void enterAddExpr(ErlangParser::AddExprContext *ctx) = 0;
  virtual void exitAddExpr(ErlangParser::AddExprContext *ctx) = 0;

  virtual void enterMultExpr(ErlangParser::MultExprContext *ctx) = 0;
  virtual void exitMultExpr(ErlangParser::MultExprContext *ctx) = 0;

  virtual void enterUnaryExpr(ErlangParser::UnaryExprContext *ctx) = 0;
  virtual void exitUnaryExpr(ErlangParser::UnaryExprContext *ctx) = 0;

  virtual void enterExpr700(ErlangParser::Expr700Context *ctx) = 0;
  virtual void exitExpr700(ErlangParser::Expr700Context *ctx) = 0;

  virtual void enterSemicolonExpr(ErlangParser::SemicolonExprContext *ctx) = 0;
  virtual void exitSemicolonExpr(ErlangParser::SemicolonExprContext *ctx) = 0;

  virtual void enterExprMax(ErlangParser::ExprMaxContext *ctx) = 0;
  virtual void exitExprMax(ErlangParser::ExprMaxContext *ctx) = 0;

  virtual void enterList(ErlangParser::ListContext *ctx) = 0;
  virtual void exitList(ErlangParser::ListContext *ctx) = 0;

  virtual void enterTail(ErlangParser::TailContext *ctx) = 0;
  virtual void exitTail(ErlangParser::TailContext *ctx) = 0;

  virtual void enterBinary(ErlangParser::BinaryContext *ctx) = 0;
  virtual void exitBinary(ErlangParser::BinaryContext *ctx) = 0;

  virtual void enterBinElements(ErlangParser::BinElementsContext *ctx) = 0;
  virtual void exitBinElements(ErlangParser::BinElementsContext *ctx) = 0;

  virtual void enterBinElement(ErlangParser::BinElementContext *ctx) = 0;
  virtual void exitBinElement(ErlangParser::BinElementContext *ctx) = 0;

  virtual void enterBitExpr(ErlangParser::BitExprContext *ctx) = 0;
  virtual void exitBitExpr(ErlangParser::BitExprContext *ctx) = 0;

  virtual void enterOptBitSizeExpr(ErlangParser::OptBitSizeExprContext *ctx) = 0;
  virtual void exitOptBitSizeExpr(ErlangParser::OptBitSizeExprContext *ctx) = 0;

  virtual void enterOptBitTypeList(ErlangParser::OptBitTypeListContext *ctx) = 0;
  virtual void exitOptBitTypeList(ErlangParser::OptBitTypeListContext *ctx) = 0;

  virtual void enterBitTypeList(ErlangParser::BitTypeListContext *ctx) = 0;
  virtual void exitBitTypeList(ErlangParser::BitTypeListContext *ctx) = 0;

  virtual void enterBitType(ErlangParser::BitTypeContext *ctx) = 0;
  virtual void exitBitType(ErlangParser::BitTypeContext *ctx) = 0;

  virtual void enterBitSizeExpr(ErlangParser::BitSizeExprContext *ctx) = 0;
  virtual void exitBitSizeExpr(ErlangParser::BitSizeExprContext *ctx) = 0;

  virtual void enterListComprehension(ErlangParser::ListComprehensionContext *ctx) = 0;
  virtual void exitListComprehension(ErlangParser::ListComprehensionContext *ctx) = 0;

  virtual void enterBinaryComprehension(ErlangParser::BinaryComprehensionContext *ctx) = 0;
  virtual void exitBinaryComprehension(ErlangParser::BinaryComprehensionContext *ctx) = 0;

  virtual void enterLcExprs(ErlangParser::LcExprsContext *ctx) = 0;
  virtual void exitLcExprs(ErlangParser::LcExprsContext *ctx) = 0;

  virtual void enterLcExpr(ErlangParser::LcExprContext *ctx) = 0;
  virtual void exitLcExpr(ErlangParser::LcExprContext *ctx) = 0;

  virtual void enterTuple(ErlangParser::TupleContext *ctx) = 0;
  virtual void exitTuple(ErlangParser::TupleContext *ctx) = 0;

  virtual void enterRecordExpr(ErlangParser::RecordExprContext *ctx) = 0;
  virtual void exitRecordExpr(ErlangParser::RecordExprContext *ctx) = 0;

  virtual void enterRecordTuple(ErlangParser::RecordTupleContext *ctx) = 0;
  virtual void exitRecordTuple(ErlangParser::RecordTupleContext *ctx) = 0;

  virtual void enterRecordFields(ErlangParser::RecordFieldsContext *ctx) = 0;
  virtual void exitRecordFields(ErlangParser::RecordFieldsContext *ctx) = 0;

  virtual void enterRecordField(ErlangParser::RecordFieldContext *ctx) = 0;
  virtual void exitRecordField(ErlangParser::RecordFieldContext *ctx) = 0;

  virtual void enterFunctionCall(ErlangParser::FunctionCallContext *ctx) = 0;
  virtual void exitFunctionCall(ErlangParser::FunctionCallContext *ctx) = 0;

  virtual void enterIfExpr(ErlangParser::IfExprContext *ctx) = 0;
  virtual void exitIfExpr(ErlangParser::IfExprContext *ctx) = 0;

  virtual void enterIfClauses(ErlangParser::IfClausesContext *ctx) = 0;
  virtual void exitIfClauses(ErlangParser::IfClausesContext *ctx) = 0;

  virtual void enterIfClause(ErlangParser::IfClauseContext *ctx) = 0;
  virtual void exitIfClause(ErlangParser::IfClauseContext *ctx) = 0;

  virtual void enterCaseExpr(ErlangParser::CaseExprContext *ctx) = 0;
  virtual void exitCaseExpr(ErlangParser::CaseExprContext *ctx) = 0;

  virtual void enterCrClauses(ErlangParser::CrClausesContext *ctx) = 0;
  virtual void exitCrClauses(ErlangParser::CrClausesContext *ctx) = 0;

  virtual void enterCrClause(ErlangParser::CrClauseContext *ctx) = 0;
  virtual void exitCrClause(ErlangParser::CrClauseContext *ctx) = 0;

  virtual void enterReceiveExpr(ErlangParser::ReceiveExprContext *ctx) = 0;
  virtual void exitReceiveExpr(ErlangParser::ReceiveExprContext *ctx) = 0;

  virtual void enterFunExpr(ErlangParser::FunExprContext *ctx) = 0;
  virtual void exitFunExpr(ErlangParser::FunExprContext *ctx) = 0;

  virtual void enterAtomOrVar(ErlangParser::AtomOrVarContext *ctx) = 0;
  virtual void exitAtomOrVar(ErlangParser::AtomOrVarContext *ctx) = 0;

  virtual void enterIntegerOrVar(ErlangParser::IntegerOrVarContext *ctx) = 0;
  virtual void exitIntegerOrVar(ErlangParser::IntegerOrVarContext *ctx) = 0;

  virtual void enterFunClauses(ErlangParser::FunClausesContext *ctx) = 0;
  virtual void exitFunClauses(ErlangParser::FunClausesContext *ctx) = 0;

  virtual void enterFunClause(ErlangParser::FunClauseContext *ctx) = 0;
  virtual void exitFunClause(ErlangParser::FunClauseContext *ctx) = 0;

  virtual void enterTryExpr(ErlangParser::TryExprContext *ctx) = 0;
  virtual void exitTryExpr(ErlangParser::TryExprContext *ctx) = 0;

  virtual void enterTryCatch(ErlangParser::TryCatchContext *ctx) = 0;
  virtual void exitTryCatch(ErlangParser::TryCatchContext *ctx) = 0;

  virtual void enterTryClauses(ErlangParser::TryClausesContext *ctx) = 0;
  virtual void exitTryClauses(ErlangParser::TryClausesContext *ctx) = 0;

  virtual void enterTryClause(ErlangParser::TryClauseContext *ctx) = 0;
  virtual void exitTryClause(ErlangParser::TryClauseContext *ctx) = 0;

  virtual void enterArgumentList(ErlangParser::ArgumentListContext *ctx) = 0;
  virtual void exitArgumentList(ErlangParser::ArgumentListContext *ctx) = 0;

  virtual void enterExprs(ErlangParser::ExprsContext *ctx) = 0;
  virtual void exitExprs(ErlangParser::ExprsContext *ctx) = 0;

  virtual void enterGuard(ErlangParser::GuardContext *ctx) = 0;
  virtual void exitGuard(ErlangParser::GuardContext *ctx) = 0;

  virtual void enterAtomic(ErlangParser::AtomicContext *ctx) = 0;
  virtual void exitAtomic(ErlangParser::AtomicContext *ctx) = 0;

  virtual void enterUnaryOp(ErlangParser::UnaryOpContext *ctx) = 0;
  virtual void exitUnaryOp(ErlangParser::UnaryOpContext *ctx) = 0;

  virtual void enterMultOp(ErlangParser::MultOpContext *ctx) = 0;
  virtual void exitMultOp(ErlangParser::MultOpContext *ctx) = 0;

  virtual void enterAddOp(ErlangParser::AddOpContext *ctx) = 0;
  virtual void exitAddOp(ErlangParser::AddOpContext *ctx) = 0;

  virtual void enterListOp(ErlangParser::ListOpContext *ctx) = 0;
  virtual void exitListOp(ErlangParser::ListOpContext *ctx) = 0;

  virtual void enterCompareOp(ErlangParser::CompareOpContext *ctx) = 0;
  virtual void exitCompareOp(ErlangParser::CompareOpContext *ctx) = 0;


};

