// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4rt/bytecode.h"
#include "e4rt/term.h"
#include "e4rt/vm.h"
#include "e4std/complicated.h"

namespace e4 {
using e4std::VoidResult;

// Stack implementation
// TODO: stack base pointer to address args and variables together
// TODO: some generic stack frame implementation for enter/leave. Dynamic heap
// frames?
class Stack {
 private:
  PODVector<Word> cells_;

 public:
  Stack() = default;
  // Will grow using vector realloc
  void push_term(Term t) { push(t.get_raw()); }

  void push(Word w) { cells_.push_back(w); }

  // Will shrink size but not shrink memory
  Word pop() {
    E4ASSERT(not cells_.empty());
    auto val = cells_.back();
    cells_.resize(cells_.size() - 1);
    return val;
  }

  Term pop_term() { return Term(pop()); }
};

constexpr Word INIT_PROCESS_HEAP = 64;  // first size for process heap (words)

class RangeChecker {
  const uint8_t* code_range_;
  const uint8_t* code_range_end_;

 public:
  explicit RangeChecker(const uint8_t* code_range,
                        const uint8_t* code_range_end)
    : code_range_(code_range), code_range_end_(code_range_end) {}
  RangeChecker(const RangeChecker& other) = default;

  bool in_range(const uint8_t* p) const {
    return p >= code_range_ && p <= code_range_end_;
  }
  void assert_in_range(const uint8_t* p) const { E4ASSERT(in_range(p)); }
};

// VM runtime context which gets swapped into VM loop and out
class RuntimeContext {
 public:
  CodeAddress pc_;
  Stack ds_;  // data stack
  Stack rs_;  // return stack
  RangeChecker range_checker_;

  RuntimeContext(const RangeChecker& rc) : range_checker_(rc) {}

  J1Opcode8 fetch() {
    auto byte = pc_.fetch();
    // TODO: check code end/code range?
    pc_.advance();
    return J1Opcode8(byte);
  }

  // Joins first byte with next and gives you a 16-bit opcode
  J1Opcode16 fetch(J1Opcode8 first) {
    auto second = pc_.fetch();
    // TODO: check code end/code range?
    pc_.advance();
    return J1Opcode16(first.raw_, second);
  }
};

enum class ProcessPriority : uint8_t { Normal, High };

class Process {
 private:
  Term pid_;
  Heap heap_;
  VM& vm_;
  // [pid()] -- linked processes
  Term links_ = NIL;
  // [pid()] -- processes which monitor this process
  Term monitors_ = NIL;
  ProcessPriority prio_ = ProcessPriority::Normal;

 public:
  RuntimeContext context_;

 public:
  Process() = delete;

  explicit Process(VM& vm, Term pid)
    : pid_(pid),
      heap_(INIT_PROCESS_HEAP),
      vm_(vm),
      context_(vm.get_code_range_checker()) {}

  Term self() const { return pid_; }

  ProcessPriority priority() const { return prio_; }

  // Sets arguments and enters mfarity with args, does not wait for execution
  // but just sets instruction pointer instead
  E4_NODISCARD VoidResult apply(const MFArgs& mfargs);

  // TODO: maybe belongs to runtime context
  void jump(CodeAddress newpc) {
    E4LOG1("[proc] jump 0x%zx\n", newpc.get_index());
    context_.pc_ = newpc;
  }

  // TODO: maybe belongs to runtime context
  void jump_rel(SignedWord offs) {
    E4ASSERT(offs != 0);
    E4LOG1("[proc] jump-rel %zd\n", offs);
    context_.pc_ += offs;
  }
};

}  // ns e4
