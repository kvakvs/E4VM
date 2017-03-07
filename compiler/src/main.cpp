#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <map>
#include <memory>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

//#include "erl_ast.h"
//#include "erl_codegen.h"
//#include "erl_jit.h"
//#include "erl_parse.h"
//#include "erl_scanner.h"
#include "erl_yy_driver.h"

#if 0
class Compiler {
 private:
  Tokenizer tokenizer_;
  Parser parser_;
  Codegen codegen_;
  std::unique_ptr<llvm::orc::ErlJIT> jit_;

 public:
  Compiler() : parser_(tokenizer_) {
    jit_ = llvm::make_unique<llvm::orc::ErlJIT>();

    // Install standard binary operators. 1 is lowest precedence.
    parser_.binop_prio_['<'] = 10;
    parser_.binop_prio_['+'] = 20;
    parser_.binop_prio_['-'] = 20;
    parser_.binop_prio_['*'] = 40;  // highest.

    codegen_.init_module_and_pass_manager(*jit_);
  }

  void main_repl_loop();

  void go() {
    // Prime the first token.
    fprintf(stderr, "ready> ");
    parser_.get_next_token();

    main_repl_loop();
  }

 private:
  void handle_top_level_expr();
  void handle_definition();
  void handle_extern();
};

void Compiler::handle_definition() {
  if (auto fun_ast = parser_.parse_definition()) {
    if (auto* fun_ir = fun_ast->codegen(codegen_)) {
      fprintf(stderr, "Read function definition:");
      fun_ir->print(llvm::errs());

      fprintf(stderr, "\n");
      jit_->addModule(std::move(codegen_.mod_));
      codegen_.init_module_and_pass_manager(*jit_);
    }
  } else {
    // Skip token for error recovery.
    parser_.get_next_token();
  }
}

void Compiler::handle_extern() {
  if (auto proto_ast = parser_.parse_extern()) {
    if (auto* fun_ir = proto_ast->codegen(codegen_)) {
      fprintf(stderr, "Read extern: ");
      fun_ir->print(llvm::errs());

      fprintf(stderr, "\n");
      codegen_.fun_protos_[proto_ast->getName()] = std::move(proto_ast);
    }
  } else {
    // Skip token for error recovery.
    parser_.get_next_token();
  }
}

void Compiler::handle_top_level_expr() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto fun_ast = parser_.parse_top_level_expr()) {
    if (fun_ast->codegen(codegen_)) {
      // JIT the module containing the anonymous expression, keeping a
      // handle so
      // we can free it later.
      auto h = jit_->addModule(std::move(codegen_.mod_));
      codegen_.init_module_and_pass_manager(*jit_);

      // Search the JIT for the __anon_expr symbol.
      auto expr_sym = jit_->findSymbol("__anon_expr");
      assert(expr_sym && "Function not found");

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native
      // function.
      double (*fun_ptr)() = (double (*)())(intptr_t)expr_sym.getAddress();
      double fresult = fun_ptr();
      fprintf(stderr, "Evaluated to %f\n", fresult);

      // Delete the anonymous expression module from the JIT.
      jit_->removeModule(h);
    }
  } else {
    // Skip token for error recovery.
    parser_.get_next_token();
  }
}

/// top ::= definition | external | expression | ';'
void Compiler::main_repl_loop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (parser_.get_cur_tok()) {
      case Token::EndOfFile:
        return;
      case ';':  // ignore top-level semicolons.
        parser_.get_next_token();
        break;
      case Token::FunDef:
        handle_definition();
        break;
      case Token::Extern:
        handle_extern();
        break;
      default:
        handle_top_level_expr();
        break;
    }
  }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef LLVM_ON_WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}
#endif //0

int main(int argc, const char* argv[]) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  // Run the main "interpreter loop" now.
  //  Compiler compiler;
  //  compiler.go();

  int res = 0;
  ErlangDriver driver;
  for (int i = 1; i < argc; ++i) {
    if (argv[i] == std::string("-p")) {
      driver.set_trace_parsing();
    } else if (argv[i] == std::string("-s")) {
      driver.set_trace_scanning();
    } else if (!driver.parse(argv[i])) {
      std::cout << driver.get_result() << std::endl;
    } else {
      res = 1;
    }
  }
  return res;
}
