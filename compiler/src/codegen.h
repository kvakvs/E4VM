#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include "ast.h"

namespace erl {

class Codegen {
 public:
  llvm::LLVMContext ctx_;
  llvm::IRBuilder<> ir_builder_;
  std::unique_ptr<llvm::Module> mod_;
  std::map<std::string, llvm::Value*> named_values_;
  std::unique_ptr<llvm::legacy::FunctionPassManager> fpm_;

 public:
  Codegen() : ir_builder_(ctx_) {
  }

  void init_module_and_pass_manager(const std::string& mod);

  void gen_function(const std::string& name, int arity);
};

} // ns erl
