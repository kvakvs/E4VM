//#include "llvm/IR/Constants.h"
//#include "llvm/IR/IRBuilder.h"
//#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
//#include "llvm/Transforms/Scalar.h"
//#include "llvm/Transforms/Scalar/GVN.h"

#include "compiler.h"

int main(int argc, const char* argv[]) {
//  llvm::InitializeNativeTarget();
//  llvm::InitializeNativeTargetAsmPrinter();
//  llvm::InitializeNativeTargetAsmParser();

  erl::Compiler compiler;
  compiler.process(argv[1]);

  return 0;
}
