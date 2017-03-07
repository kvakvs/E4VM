#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "antlr4/ErlangLexer.h"
#include "antlr4/ErlangParser.h"

class ErlangLexer1 : public ErlangLexer {
public:
  explicit ErlangLexer1(antlr4::CharStream* input) :
      ErlangLexer(input) {}

  virtual const std::vector<std::string>& getChannelNames() const override {
    return channels_;
  }

private:
  std::vector<std::string> channels_;
};

int main(int argc, const char* argv[]) {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  antlr4::ANTLRFileStream f_stream(argv[1]);
  ErlangLexer1 lexer(&f_stream);
  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto token : tokens.getTokens()) {
    std::cout << token->toString() << std::endl;
  }

  ErlangParser parser(&tokens);
  antlr4::tree::ParseTree* tree = parser.forms();

  std::cout << tree->toStringTree(&parser) << std::endl << std::endl;

  return 0;
}
