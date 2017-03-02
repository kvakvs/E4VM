#include <map>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include "erl_codegen.h"
//#include "erl_parse.h"

llvm::Value* Codegen::error_v(const char* Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

llvm::Function* Codegen::get_function(const std::string& name) {
  // First, see if the function has already been added to the current module.
  if (auto* fn = mod_->getFunction(name))
    return fn;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto f_iter = fun_protos_.find(name);
  if (f_iter != fun_protos_.end())
    return f_iter->second->codegen(*this);

  // If no existing prototype exists, return null.
  return nullptr;
}

llvm::Value* ast::DoubleLit::codegen(Codegen& cg) {
  return llvm::ConstantFP::get(cg.ctx_, llvm::APFloat(dbl_));
}

llvm::Value* ast::Variable::codegen(Codegen& cg) {
  // Look this variable up in the function.
  llvm::Value* V = cg.named_values_[name_];
  if (!V)
    return cg.error_v("Unknown variable name");
  return V;
}

llvm::Value* ast::BinaryExpr::codegen(Codegen& cg) {
  llvm::Value* L = lhs_->codegen(cg);
  llvm::Value* R = rhs_->codegen(cg);
  if (!L || !R)
    return nullptr;

  switch (op_) {
    case '+':
      return cg.ir_builder_.CreateFAdd(L, R, "addtmp");
    case '-':
      return cg.ir_builder_.CreateFSub(L, R, "subtmp");
    case '*':
      return cg.ir_builder_.CreateFMul(L, R, "multmp");
    case '<':
      L = cg.ir_builder_.CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return cg.ir_builder_.CreateUIToFP(L, llvm::Type::getDoubleTy(cg.ctx_),
                                         "booltmp");
    default:
      return cg.error_v("invalid binary operator");
  }
}

llvm::Value* ast::Call::codegen(Codegen& cg) {
  // Look up the name in the global module table.
  llvm::Function* CalleeF = cg.get_function(callee_);
  if (!CalleeF)
    return cg.error_v("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != args_.size())
    return cg.error_v("Incorrect # arguments passed");

  std::vector<llvm::Value*> args_v;
  for (size_t i = 0, e = args_.size(); i != e; ++i) {
    args_v.push_back(args_[i]->codegen(cg));
    if (!args_v.back())
      return nullptr;
  }

  return cg.ir_builder_.CreateCall(CalleeF, args_v, "calltmp");
}

llvm::Value* ast::IfCondition::codegen(Codegen& cg) {
  llvm::Value* cond_v = cond_->codegen(cg);
  if (!cond_v)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  cond_v = cg.ir_builder_.CreateFCmpONE(
      cond_v, llvm::ConstantFP::get(cg.ctx_, llvm::APFloat(0.0)), "ifcond");

  llvm::Function* f_parent = cg.ir_builder_.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at
  // the
  // end of the function.
  llvm::BasicBlock* true_bb = llvm::BasicBlock::Create(cg.ctx_, "if", f_parent);
  llvm::BasicBlock* false_bb = llvm::BasicBlock::Create(cg.ctx_, "else");
  llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(cg.ctx_, "ifcont");

  cg.ir_builder_.CreateCondBr(cond_v, true_bb, false_bb);

  // Emit then value.
  cg.ir_builder_.SetInsertPoint(true_bb);

  llvm::Value* then_v = then_->codegen(cg);
  if (!then_v)
    return nullptr;

  cg.ir_builder_.CreateBr(merge_bb);
  // Codegen of 'Then' can change the current block, update true_bb for the
  // PHI.
  true_bb = cg.ir_builder_.GetInsertBlock();

  // Emit else block.
  f_parent->getBasicBlockList().push_back(false_bb);
  cg.ir_builder_.SetInsertPoint(false_bb);

  llvm::Value* else_v = else_->codegen(cg);
  if (!else_v)
    return nullptr;

  cg.ir_builder_.CreateBr(merge_bb);
  // Codegen of Else can change the current block, update false_bb for the PHI.
  false_bb = cg.ir_builder_.GetInsertBlock();

  // Emit merge block.
  f_parent->getBasicBlockList().push_back(merge_bb);
  cg.ir_builder_.SetInsertPoint(merge_bb);
  llvm::PHINode* phi_node =
      cg.ir_builder_.CreatePHI(llvm::Type::getDoubleTy(cg.ctx_), 2, "iftmp");

  phi_node->addIncoming(then_v, true_bb);
  phi_node->addIncoming(else_v, false_bb);
  return phi_node;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:
llvm::Value* ast::ForLoop::codegen(Codegen& cg) {
  // Emit the start code first, without 'variable' in scope.
  llvm::Value* start_val = start_->codegen(cg);
  if (!start_val) { return nullptr; }

  // Make the new basic block for the loop header, inserting after current
  // block.
  llvm::Function* f_parent = cg.ir_builder_.GetInsertBlock()->getParent();
  auto preheader_bb = cg.ir_builder_.GetInsertBlock();
  auto loop_bb = llvm::BasicBlock::Create(cg.ctx_, "loop", f_parent);

  // Insert an explicit fall through from the current block to the loop_bb.
  cg.ir_builder_.CreateBr(loop_bb);

  // Start insertion in loop_bb.
  cg.ir_builder_.SetInsertPoint(loop_bb);

  // Start the PHI node with an entry for Start.
  auto var = cg.ir_builder_.CreatePHI(llvm::Type::getDoubleTy(cg.ctx_), 2, var_);
  var->addIncoming(start_val, preheader_bb);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  auto old_val = cg.named_values_[var_];
  cg.named_values_[var_] = var;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but
  // don't
  // allow an error.
  if (!body_->codegen(cg)) { return nullptr; }

  // Emit the step value.
  llvm::Value* step_val = nullptr;
  if (step_) {
    step_val = step_->codegen(cg);
    if (!step_val) { return nullptr; }
  } else {
    // If not specified, use 1.0.
    step_val = llvm::ConstantFP::get(cg.ctx_, llvm::APFloat(1.0));
  }

  auto next_var = cg.ir_builder_.CreateFAdd(var, step_val, "nextvar");

  // Compute the end condition.
  llvm::Value* end_cond = end_->codegen(cg);
  if (!end_cond) { return nullptr; }

  // Convert condition to a bool by comparing non-equal to 0.0.
  end_cond = cg.ir_builder_.CreateFCmpONE(
      end_cond,
      llvm::ConstantFP::get(cg.ctx_, llvm::APFloat(0.0)),
      "loopcond");

  // Create the "after loop" block and insert it.
  llvm::BasicBlock* LoopEndBB = cg.ir_builder_.GetInsertBlock();
  llvm::BasicBlock* AfterBB =
      llvm::BasicBlock::Create(cg.ctx_, "afterloop", f_parent);

  // Insert the conditional branch into the end of LoopEndBB.
  cg.ir_builder_.CreateCondBr(end_cond, loop_bb, AfterBB);

  // Any new code will be inserted in AfterBB.
  cg.ir_builder_.SetInsertPoint(AfterBB);

  // Add a new entry to the PHI node for the backedge.
  var->addIncoming(next_var, LoopEndBB);

  // Restore the unshadowed variable.
  if (old_val)
    cg.named_values_[var_] = old_val;
  else
    cg.named_values_.erase(var_);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(cg.ctx_));
}

llvm::Function* ast::FunPrototype::codegen(Codegen& cg) {
  // Make the function type:  double(double,double) etc.
  std::vector<llvm::Type*> dbl_vec(args_.size(),
                                   llvm::Type::getDoubleTy(cg.ctx_));
  auto fun_type = llvm::FunctionType::get(llvm::Type::getDoubleTy(cg.ctx_),
                                          dbl_vec, false);

  auto fun = llvm::Function::Create(fun_type,
                                    llvm::Function::ExternalLinkage,
                                    fun_name_, cg.mod_.get());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto& Arg : fun->args())
    Arg.setName(args_[Idx++]);

  return fun;
}

llvm::Function* ast::FunctionAST::codegen(Codegen& cg) {
  // Transfer ownership of the prototype to the fun_protos_ map, but keep a
  // reference to it for use below.
  auto& proto = *proto_;
  cg.fun_protos_[proto_->getName()] = std::move(proto_);
  llvm::Function* fn = cg.get_function(proto.getName());
  if (!fn) { return nullptr; }

  // Create a new basic block to start insertion into.
  llvm::BasicBlock* BB = llvm::BasicBlock::Create(cg.ctx_, "entry", fn);
  cg.ir_builder_.SetInsertPoint(BB);

  // Record the function arguments in the named_values_ map.
  cg.named_values_.clear();
  for (auto& Arg : fn->args())
    cg.named_values_[Arg.getName()] = &Arg;

  llvm::Value* ret_val = body_->codegen(cg);
  if (ret_val) {
    // Finish off the function.
    cg.ir_builder_.CreateRet(ret_val);

    // Validate the generated code, checking for consistency.
    verifyFunction(*fn);

    // Run the optimizer on the function.
    cg.fpm_->run(*fn);

    return fn;
  }

  // Error reading body, remove function.
  fn->eraseFromParent();
  return nullptr;
}

void Codegen::init_module_and_pass_manager() {
  // Open a new module.
  mod_ = llvm::make_unique<llvm::Module>("my cool jit", ctx_);
  mod_->setDataLayout(jit_->getTargetMachine().createDataLayout());

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
