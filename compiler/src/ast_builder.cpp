#include <llvm/Support/raw_ostream.h>
#include "ast_builder.h"

namespace erl {

ast::INode::Ptr&& build_ast(antlr4::tree::ParseTree* tree) {
  ASTBuilder builder;
  builder.visit(tree);
  return std::move(builder.tree_);
}

void ASTBuilder::add_node(ast::INode::Ptr&& n) {
  if (not tree_) {
    tree_ = std::move(n);
  } else {
    tree_->add_child(std::move(n));
  }
}

antlrcpp::Any
ASTBuilder::visitFunction(ErlangParser::FunctionContext* ctx) {
  auto clause0 = ctx->functionClause(0);

  auto ast_fn = new ast::Function(clause0->tokAtom()->getText());
  ast::INode::Ptr ast_fn_uptr(ast_fn);
  llvm::outs() << "FUN " << ast_fn->get_name() << "\n";

  for (ErlangParser::FunctionClauseContext* clause: ctx->functionClause()) {
    ErlangParser::ClauseArgsContext* args = clause->clauseArgs();
    auto clause_name = clause->tokAtom()->getText();
    llvm::outs() << "-- CLAUSE " << clause_name << "\n";

    auto ast_clause = std::make_unique<ast::FunctionClause>();

    // Process argument list, parse every subtree
    auto arg_list0 = args->argumentList();
    auto arg_list1 = arg_list0->commaSeparatedExprs();
    if (arg_list1) {
      auto arg_list2 = arg_list1->expression();
      for (ErlangParser::ExpressionContext* arg: arg_list2) {
        llvm::outs() << "  -- ARG " << arg->getText() << "\n";
        ast_clause->add_arg(std::move(build_ast(arg)));
      }
    }

    ast_fn->add_clause(std::move(ast_clause));
  }

//  return Super::visitFunction(ctx);
  add_node(std::move(ast_fn_uptr));
  return antlrcpp::Any();
}

} // ns erl
