#include "codegen.h"
#include "ast.h"
#include "compile_error.h"

//#include <llvm/IR/IRBuilder.h>
//#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Target/TargetMachine.h>

namespace erl {

void Codegen::init_module_and_pass_manager(const std::string& mod) {
  // Open a new module.
  mod_ = llvm::make_unique<llvm::Module>(mod, ctx_);

//  mod_->setDataLayout(executionEngine->getTargetData()->getStringRepresentation());
//  mod_->setDataLayout(jit.getTargetMachine().createDataLayout());

  // Create a new pass manager attached to it.
  fpm_ = llvm::make_unique<llvm::legacy::FunctionPassManager>(mod_.get());

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  fpm_->add(llvm::createInstructionCombiningPass());

  // Reassociate expressions.
  fpm_->add(llvm::createReassociatePass());

  // Eliminate Common SubExpressions.
  fpm_->add(llvm::createGVNPass());

  // Simplify the control flow graph (deleting unreachable blocks, etc).
  fpm_->add(llvm::createCFGSimplificationPass());

  fpm_->doInitialization();
}

void Codegen::gen_function(ast::Function& ast_fn) {
  E4_ASSERT(ast_fn.arity() >= 0);
  std::vector<llvm::Type*> arg_vec(static_cast<size_t>(ast_fn.arity()),
                                   llvm::Type::getDoubleTy(ctx_));
  auto fun_type =
      llvm::FunctionType::get(llvm::Type::getDoubleTy(ctx_), arg_vec, false);

  auto fun = llvm::Function::Create(fun_type,
                                    llvm::Function::ExternalLinkage,
                                    ast_fn.name(),
                                    mod_.get());

}

} // ns erl
