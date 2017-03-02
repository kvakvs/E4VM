#pragma once

#include <memory>
#include <vector>

#include <llvm/IR/Value.h>

class Codegen;

namespace ast {

/// ExprAST - Base class for all expression nodes.
class IExpr {
 public:
  virtual ~IExpr() = default;

  virtual llvm::Value* codegen(Codegen& cg) = 0;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class DoubleLit : public IExpr {
  double dbl_;

 public:
  explicit DoubleLit(double Val) : dbl_(Val) {}

  llvm::Value* codegen(Codegen& cg) override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class Variable : public IExpr {
  std::string name_;

 public:
  explicit Variable(const std::string& Name) : name_(Name) {}

  llvm::Value* codegen(Codegen& cg) override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExpr : public IExpr {
  char op_;
  std::unique_ptr<IExpr> lhs_, rhs_;

 public:
  BinaryExpr(char Op,
             std::unique_ptr<IExpr>&& LHS,
             std::unique_ptr<IExpr>&& RHS)
      : op_(Op), lhs_(std::move(LHS)), rhs_(std::move(RHS)) {}

  llvm::Value* codegen(Codegen& cg) override;
};

/// CallExprAST - Expression class for function calls.
class Call : public IExpr {
  std::string callee_;
  std::vector<std::unique_ptr<IExpr>> args_;

 public:
  Call(const std::string& Callee, std::vector<std::unique_ptr<IExpr>>&& Args)
      : callee_(Callee), args_(std::move(Args)) {}

  llvm::Value* codegen(Codegen& cg) override;
};

/// IfExprAST - Expression class for if/then/else.
class IfCondition : public IExpr {
  std::unique_ptr<IExpr> cond_, then_, else_;

 public:
  IfCondition(std::unique_ptr<IExpr>&& Cond,
              std::unique_ptr<IExpr>&& Then,
              std::unique_ptr<IExpr>&& Else)
      : cond_(std::move(Cond)),
        then_(std::move(Then)),
        else_(std::move(Else)) {}

  llvm::Value* codegen(Codegen& cg) override;
};

/// ForExprAST - Expression class for for/in.
class ForLoop : public IExpr {
  std::string var_;
  std::unique_ptr<IExpr> start_, end_, step_, body_;

 public:
  ForLoop(const std::string& VarName,
          std::unique_ptr<IExpr>&& Start,
          std::unique_ptr<IExpr>&& End,
          std::unique_ptr<IExpr>&& Step,
          std::unique_ptr<IExpr>&& Body)
      : var_(VarName),
        start_(std::move(Start)),
        end_(std::move(End)),
        step_(std::move(Step)),
        body_(std::move(Body)) {}

  llvm::Value* codegen(Codegen& cg) override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class FunPrototype {
  std::string fun_name_;
  std::vector<std::string> args_;

 public:
  FunPrototype(const std::string& Name, std::vector<std::string>&& Args)
      : fun_name_(Name), args_(std::move(Args)) {}

  llvm::Function* codegen(Codegen& cg);
  const std::string& getName() const { return fun_name_; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<FunPrototype> proto_;
  std::unique_ptr<IExpr> body_;

 public:
  FunctionAST(std::unique_ptr<FunPrototype>&& Proto,
              std::unique_ptr<IExpr>&& Body)
      : proto_(std::move(Proto)), body_(std::move(Body)) {}

  llvm::Function* codegen(Codegen& cg);
};

}  // ns ast
