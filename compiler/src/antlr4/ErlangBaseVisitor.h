
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"
#include "ErlangVisitor.h"


/**
 * This class provides an empty implementation of ErlangVisitor, which can be
 * extended to create a visitor which only needs to handle a subset of the available methods.
 */
class  ErlangBaseVisitor : public ErlangVisitor {
public:

  virtual antlrcpp::Any visitForms(ErlangParser::FormsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitForm(ErlangParser::FormContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTokAtom(ErlangParser::TokAtomContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTokVar(ErlangParser::TokVarContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTokFloat(ErlangParser::TokFloatContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTokInteger(ErlangParser::TokIntegerContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTokChar(ErlangParser::TokCharContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTokString(ErlangParser::TokStringContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAttribute(ErlangParser::AttributeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypeSpec(ErlangParser::TypeSpecContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitSpecFun(ErlangParser::SpecFunContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypedAttrVal(ErlangParser::TypedAttrValContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypedRecordFields(ErlangParser::TypedRecordFieldsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypedExprs(ErlangParser::TypedExprsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypedExpr(ErlangParser::TypedExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypeSigs(ErlangParser::TypeSigsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypeSig(ErlangParser::TypeSigContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypeGuards(ErlangParser::TypeGuardsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTypeGuard(ErlangParser::TypeGuardContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTopTypes(ErlangParser::TopTypesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTopType(ErlangParser::TopTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTopType100(ErlangParser::TopType100Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitType200(ErlangParser::Type200Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitType300(ErlangParser::Type300Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitType400(ErlangParser::Type400Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitType500(ErlangParser::Type500Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitType(ErlangParser::TypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunType100(ErlangParser::FunType100Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunType(ErlangParser::FunTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFieldTypes(ErlangParser::FieldTypesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFieldType(ErlangParser::FieldTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinaryType(ErlangParser::BinaryTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinBaseType(ErlangParser::BinBaseTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinUnitType(ErlangParser::BinUnitTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAttrVal(ErlangParser::AttrValContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunction(ErlangParser::FunctionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunctionClause(ErlangParser::FunctionClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitClauseArgs(ErlangParser::ClauseArgsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitClauseGuard(ErlangParser::ClauseGuardContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitClauseBody(ErlangParser::ClauseBodyContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitExpression(ErlangParser::ExpressionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitMatchExpr(ErlangParser::MatchExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOrelseExpr(ErlangParser::OrelseExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAndalsoExpr(ErlangParser::AndalsoExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCompareExpr(ErlangParser::CompareExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitListExpr(ErlangParser::ListExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAddExpr(ErlangParser::AddExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitMultExpr(ErlangParser::MultExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitUnaryExpr(ErlangParser::UnaryExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitExpr700(ErlangParser::Expr700Context *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitColonExpr(ErlangParser::ColonExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitExprMax(ErlangParser::ExprMaxContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitList(ErlangParser::ListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTail(ErlangParser::TailContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinary(ErlangParser::BinaryContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinElements(ErlangParser::BinElementsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinElement(ErlangParser::BinElementContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBitExpr(ErlangParser::BitExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOptBitSizeExpr(ErlangParser::OptBitSizeExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitOptBitTypeList(ErlangParser::OptBitTypeListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBitTypeList(ErlangParser::BitTypeListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBitType(ErlangParser::BitTypeContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBitSizeExpr(ErlangParser::BitSizeExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitListComprehension(ErlangParser::ListComprehensionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitBinaryComprehension(ErlangParser::BinaryComprehensionContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLcExprs(ErlangParser::LcExprsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLcExpr(ErlangParser::LcExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTuple(ErlangParser::TupleContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRecordExpr(ErlangParser::RecordExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRecordTuple(ErlangParser::RecordTupleContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRecordFields(ErlangParser::RecordFieldsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitRecordField(ErlangParser::RecordFieldContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunctionCall(ErlangParser::FunctionCallContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIfExpr(ErlangParser::IfExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIfClauses(ErlangParser::IfClausesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIfClause(ErlangParser::IfClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCaseExpr(ErlangParser::CaseExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCrClauses(ErlangParser::CrClausesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCrClause(ErlangParser::CrClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitReceiveExpr(ErlangParser::ReceiveExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunExpr(ErlangParser::FunExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAtomOrVar(ErlangParser::AtomOrVarContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitIntegerOrVar(ErlangParser::IntegerOrVarContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunClauses(ErlangParser::FunClausesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitFunClause(ErlangParser::FunClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTryExpr(ErlangParser::TryExprContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTryCatch(ErlangParser::TryCatchContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTryClauses(ErlangParser::TryClausesContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitTryClause(ErlangParser::TryClauseContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitArgumentList(ErlangParser::ArgumentListContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCommaSeparatedExprs(ErlangParser::CommaSeparatedExprsContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitGuard(ErlangParser::GuardContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitLiteral(ErlangParser::LiteralContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitUnaryOp(ErlangParser::UnaryOpContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitMultOp(ErlangParser::MultOpContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitAddOp(ErlangParser::AddOpContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitListOp(ErlangParser::ListOpContext *ctx) override {
    return visitChildren(ctx);
  }

  virtual antlrcpp::Any visitCompareOp(ErlangParser::CompareOpContext *ctx) override {
    return visitChildren(ctx);
  }


};

