#include "ast.h"
//#include "antlr4/ErlangParser.h"

namespace ast {

void Function::codegen(erl::Codegen& cg) {
  // invert dependency
  cg.gen_function(this);
}

void FunctionClause::codegen_clause(erl::Codegen& cg) {

}

} // ns ast
