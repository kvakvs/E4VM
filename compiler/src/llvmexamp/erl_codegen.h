#pragma once

#include <map>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Value.h>

#include "erl_ast.h"

namespace llvm {
namespace orc {
class ErlJIT;
}
}

class Codegen {
 public:
  llvm::LLVMContext ctx_;
  llvm::IRBuilder<> ir_builder_;
  std::unique_ptr<llvm::Module> mod_;
  std::map<std::string, llvm::Value*> named_values_;
  std::unique_ptr<llvm::legacy::FunctionPassManager> fpm_;
  std::map<std::string, std::unique_ptr<ast::FunPrototype>> fun_protos_;

 public:
  Codegen() : ir_builder_(ctx_) {}

  llvm::Function* get_function(const std::string& name);

  llvm::Value* error_v(const char* Str);

  void init_module_and_pass_manager(llvm::orc::ErlJIT& jit);
};
