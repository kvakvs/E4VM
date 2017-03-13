#include "codegen.h"
#include "ast.h"
#include "compile_error.h"
#include "naming.h"

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

  auto fn_name = naming::function_name(ast_fn.name(), ast_fn.arity());
  llvm::outs() << "cg: Creating fun " << fn_name << "\n";

  auto fun = llvm::Function::Create(fun_type,
                                    llvm::Function::ExternalLinkage,
                                    fn_name,
                                    mod_.get());

  // Create a new basic block to start insertion
  llvm::BasicBlock* bb = llvm::BasicBlock::Create(ctx_, fn_name, fun);
  ir_builder_.SetInsertPoint(bb);
  {
    auto v1 = llvm::ConstantInt::get(ctx_, llvm::APInt(32, 123));
    auto v2 = llvm::ConstantInt::get(ctx_, llvm::APInt(32, 456));
    ir_builder_.CreateMul(v1, v2, "hello");
  }

  llvm::outs() << "<- Dumping fun " << fn_name << "\n";
  for (auto& instr: *bb) {
    llvm::outs() << instr << "\n";
  }
}

void Codegen::print() {
}

} // ns erl
