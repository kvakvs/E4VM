#include "ast.h"
#include "codegen.h"
#include "compile_error.h"
//#include "antlr4/ErlangParser.h"

namespace ast {

void Function::codegen(erl::Codegen& cg) {
  // invert dependency
  cg.gen_function(*this);
}

void Function::add_clause(std::unique_ptr<FunctionClause>&& clause) {
  if (arity_ == -1) {
    arity_ = clause->arity();
  } else if (arity_ != clause->arity()) {
    throw err::CompileError("Function clause arity mismatch");
  }
  clauses_.push_back(std::move(clause));
}

void FunctionClause::codegen_clause(erl::Codegen& cg) {

}

} // ns ast
