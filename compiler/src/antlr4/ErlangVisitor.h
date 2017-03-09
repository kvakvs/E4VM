
// Generated from Erlang.g4 by ANTLR 4.6

#pragma once


#include "antlr4-runtime.h"
#include "ErlangParser.h"



/**
 * This class defines an abstract visitor for a parse tree
 * produced by ErlangParser.
 */
class  ErlangVisitor : public antlr4::tree::AbstractParseTreeVisitor {
public:

  /**
   * Visit parse trees produced by ErlangParser.
   */
    virtual antlrcpp::Any visitForms(ErlangParser::FormsContext *context) = 0;

    virtual antlrcpp::Any visitForm(ErlangParser::FormContext *context) = 0;

    virtual antlrcpp::Any visitTokAtom(ErlangParser::TokAtomContext *context) = 0;

    virtual antlrcpp::Any visitTokVar(ErlangParser::TokVarContext *context) = 0;

    virtual antlrcpp::Any visitTokFloat(ErlangParser::TokFloatContext *context) = 0;

    virtual antlrcpp::Any visitTokInteger(ErlangParser::TokIntegerContext *context) = 0;

    virtual antlrcpp::Any visitTokChar(ErlangParser::TokCharContext *context) = 0;

    virtual antlrcpp::Any visitTokString(ErlangParser::TokStringContext *context) = 0;

    virtual antlrcpp::Any visitAttribute(ErlangParser::AttributeContext *context) = 0;

    virtual antlrcpp::Any visitTypeSpec(ErlangParser::TypeSpecContext *context) = 0;

    virtual antlrcpp::Any visitSpecFun(ErlangParser::SpecFunContext *context) = 0;

    virtual antlrcpp::Any visitTypedAttrVal(ErlangParser::TypedAttrValContext *context) = 0;

    virtual antlrcpp::Any visitTypedRecordFields(ErlangParser::TypedRecordFieldsContext *context) = 0;

    virtual antlrcpp::Any visitTypedExprs(ErlangParser::TypedExprsContext *context) = 0;

    virtual antlrcpp::Any visitTypedExpr(ErlangParser::TypedExprContext *context) = 0;

    virtual antlrcpp::Any visitTypeSigs(ErlangParser::TypeSigsContext *context) = 0;

    virtual antlrcpp::Any visitTypeSig(ErlangParser::TypeSigContext *context) = 0;

    virtual antlrcpp::Any visitTypeGuards(ErlangParser::TypeGuardsContext *context) = 0;

    virtual antlrcpp::Any visitTypeGuard(ErlangParser::TypeGuardContext *context) = 0;

    virtual antlrcpp::Any visitTopTypes(ErlangParser::TopTypesContext *context) = 0;

    virtual antlrcpp::Any visitTopType(ErlangParser::TopTypeContext *context) = 0;

    virtual antlrcpp::Any visitTopType100(ErlangParser::TopType100Context *context) = 0;

    virtual antlrcpp::Any visitType200(ErlangParser::Type200Context *context) = 0;

    virtual antlrcpp::Any visitType300(ErlangParser::Type300Context *context) = 0;

    virtual antlrcpp::Any visitType400(ErlangParser::Type400Context *context) = 0;

    virtual antlrcpp::Any visitType500(ErlangParser::Type500Context *context) = 0;

    virtual antlrcpp::Any visitType(ErlangParser::TypeContext *context) = 0;

    virtual antlrcpp::Any visitFunType100(ErlangParser::FunType100Context *context) = 0;

    virtual antlrcpp::Any visitFunType(ErlangParser::FunTypeContext *context) = 0;

    virtual antlrcpp::Any visitFieldTypes(ErlangParser::FieldTypesContext *context) = 0;

    virtual antlrcpp::Any visitFieldType(ErlangParser::FieldTypeContext *context) = 0;

    virtual antlrcpp::Any visitBinaryType(ErlangParser::BinaryTypeContext *context) = 0;

    virtual antlrcpp::Any visitBinBaseType(ErlangParser::BinBaseTypeContext *context) = 0;

    virtual antlrcpp::Any visitBinUnitType(ErlangParser::BinUnitTypeContext *context) = 0;

    virtual antlrcpp::Any visitAttrVal(ErlangParser::AttrValContext *context) = 0;

    virtual antlrcpp::Any visitFunction(ErlangParser::FunctionContext *context) = 0;

    virtual antlrcpp::Any visitFunctionClause(ErlangParser::FunctionClauseContext *context) = 0;

    virtual antlrcpp::Any visitClauseArgs(ErlangParser::ClauseArgsContext *context) = 0;

    virtual antlrcpp::Any visitClauseGuard(ErlangParser::ClauseGuardContext *context) = 0;

    virtual antlrcpp::Any visitClauseBody(ErlangParser::ClauseBodyContext *context) = 0;

    virtual antlrcpp::Any visitExpression(ErlangParser::ExpressionContext *context) = 0;

    virtual antlrcpp::Any visitMatchExpr(ErlangParser::MatchExprContext *context) = 0;

    virtual antlrcpp::Any visitOrelseExpr(ErlangParser::OrelseExprContext *context) = 0;

    virtual antlrcpp::Any visitAndalsoExpr(ErlangParser::AndalsoExprContext *context) = 0;

    virtual antlrcpp::Any visitCompareExpr(ErlangParser::CompareExprContext *context) = 0;

    virtual antlrcpp::Any visitListExpr(ErlangParser::ListExprContext *context) = 0;

    virtual antlrcpp::Any visitAddExpr(ErlangParser::AddExprContext *context) = 0;

    virtual antlrcpp::Any visitMultExpr(ErlangParser::MultExprContext *context) = 0;

    virtual antlrcpp::Any visitUnaryExpr(ErlangParser::UnaryExprContext *context) = 0;

    virtual antlrcpp::Any visitExpr700(ErlangParser::Expr700Context *context) = 0;

    virtual antlrcpp::Any visitColonExpr(ErlangParser::ColonExprContext *context) = 0;

    virtual antlrcpp::Any visitExprMax(ErlangParser::ExprMaxContext *context) = 0;

    virtual antlrcpp::Any visitList(ErlangParser::ListContext *context) = 0;

    virtual antlrcpp::Any visitTail(ErlangParser::TailContext *context) = 0;

    virtual antlrcpp::Any visitBinary(ErlangParser::BinaryContext *context) = 0;

    virtual antlrcpp::Any visitBinElements(ErlangParser::BinElementsContext *context) = 0;

    virtual antlrcpp::Any visitBinElement(ErlangParser::BinElementContext *context) = 0;

    virtual antlrcpp::Any visitBitExpr(ErlangParser::BitExprContext *context) = 0;

    virtual antlrcpp::Any visitOptBitSizeExpr(ErlangParser::OptBitSizeExprContext *context) = 0;

    virtual antlrcpp::Any visitOptBitTypeList(ErlangParser::OptBitTypeListContext *context) = 0;

    virtual antlrcpp::Any visitBitTypeList(ErlangParser::BitTypeListContext *context) = 0;

    virtual antlrcpp::Any visitBitType(ErlangParser::BitTypeContext *context) = 0;

    virtual antlrcpp::Any visitBitSizeExpr(ErlangParser::BitSizeExprContext *context) = 0;

    virtual antlrcpp::Any visitListComprehension(ErlangParser::ListComprehensionContext *context) = 0;

    virtual antlrcpp::Any visitBinaryComprehension(ErlangParser::BinaryComprehensionContext *context) = 0;

    virtual antlrcpp::Any visitLcExprs(ErlangParser::LcExprsContext *context) = 0;

    virtual antlrcpp::Any visitLcExpr(ErlangParser::LcExprContext *context) = 0;

    virtual antlrcpp::Any visitTuple(ErlangParser::TupleContext *context) = 0;

    virtual antlrcpp::Any visitRecordExpr(ErlangParser::RecordExprContext *context) = 0;

    virtual antlrcpp::Any visitRecordTuple(ErlangParser::RecordTupleContext *context) = 0;

    virtual antlrcpp::Any visitRecordFields(ErlangParser::RecordFieldsContext *context) = 0;

    virtual antlrcpp::Any visitRecordField(ErlangParser::RecordFieldContext *context) = 0;

    virtual antlrcpp::Any visitFunctionCall(ErlangParser::FunctionCallContext *context) = 0;

    virtual antlrcpp::Any visitIfExpr(ErlangParser::IfExprContext *context) = 0;

    virtual antlrcpp::Any visitIfClauses(ErlangParser::IfClausesContext *context) = 0;

    virtual antlrcpp::Any visitIfClause(ErlangParser::IfClauseContext *context) = 0;

    virtual antlrcpp::Any visitCaseExpr(ErlangParser::CaseExprContext *context) = 0;

    virtual antlrcpp::Any visitCrClauses(ErlangParser::CrClausesContext *context) = 0;

    virtual antlrcpp::Any visitCrClause(ErlangParser::CrClauseContext *context) = 0;

    virtual antlrcpp::Any visitReceiveExpr(ErlangParser::ReceiveExprContext *context) = 0;

    virtual antlrcpp::Any visitFunExpr(ErlangParser::FunExprContext *context) = 0;

    virtual antlrcpp::Any visitAtomOrVar(ErlangParser::AtomOrVarContext *context) = 0;

    virtual antlrcpp::Any visitIntegerOrVar(ErlangParser::IntegerOrVarContext *context) = 0;

    virtual antlrcpp::Any visitFunClauses(ErlangParser::FunClausesContext *context) = 0;

    virtual antlrcpp::Any visitFunClause(ErlangParser::FunClauseContext *context) = 0;

    virtual antlrcpp::Any visitTryExpr(ErlangParser::TryExprContext *context) = 0;

    virtual antlrcpp::Any visitTryCatch(ErlangParser::TryCatchContext *context) = 0;

    virtual antlrcpp::Any visitTryClauses(ErlangParser::TryClausesContext *context) = 0;

    virtual antlrcpp::Any visitTryClause(ErlangParser::TryClauseContext *context) = 0;

    virtual antlrcpp::Any visitArgumentList(ErlangParser::ArgumentListContext *context) = 0;

    virtual antlrcpp::Any visitCommaSeparatedExprs(ErlangParser::CommaSeparatedExprsContext *context) = 0;

    virtual antlrcpp::Any visitGuard(ErlangParser::GuardContext *context) = 0;

    virtual antlrcpp::Any visitLiteral(ErlangParser::LiteralContext *context) = 0;

    virtual antlrcpp::Any visitUnaryOp(ErlangParser::UnaryOpContext *context) = 0;

    virtual antlrcpp::Any visitMultOp(ErlangParser::MultOpContext *context) = 0;

    virtual antlrcpp::Any visitAddOp(ErlangParser::AddOpContext *context) = 0;

    virtual antlrcpp::Any visitListOp(ErlangParser::ListOpContext *context) = 0;

    virtual antlrcpp::Any visitCompareOp(ErlangParser::CompareOpContext *context) = 0;


};

