//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#pragma once

#include "e4rt/vm.h"
#include "e4rt/term.h"
#include "e4rt/bytecode.h"
#include "e4std/complicated.h"

namespace e4 {
using e4std::VoidResult;

// Stack implementation
// TODO: stack base pointer to address args and variables together
// TODO: some generic stack frame implementation for enter/leave. Dynamic heap frames?
class Stack {
private:
    Vector<Word> cells_;
public:
    Stack() = default;
    // Will grow using vector realloc
    void push_term(Term t) {
        push(t.get_raw());
    }

    void push(Word w) {
        cells_.push_back(w);
    }

    // Will shrink size but not shrink memory
    Word pop() {
        E4ASSERT(not cells_.empty());
        auto val = cells_.back();
        cells_.resize(cells_.size() - 1);
        return val;
    }

    Term pop_term() {
        return Term(pop());
    }
};

constexpr Word INIT_PROCESS_HEAP = 64; // first size for process heap (words)

class Process {
private:
    Term pid_;
    Heap heap_;
    Stack stack_;
    VM& vm_;

public:
    // VM runtime context which gets swapped into VM loop and out
    class RuntimeContext {
    public:
        CodeAddress pc_;
        RuntimeContext() = default;

        J1Opcode fetch() {
            auto opcode = pc_.fetch();
            // TODO: check code end/code range?
            pc_.advance();
            return opcode;
        }
    };
    RuntimeContext context_;

public:
    Process() = delete;
    explicit Process(VM& vm, Term pid)
            : pid_(pid), heap_(INIT_PROCESS_HEAP), vm_(vm), context_() {}

    Term self() const { return pid_; }

    // Sets arguments and enters mfarity with args, does not wait for execution
    // but just sets instruction pointer instead
    E4_NODISCARD VoidResult
    apply(const MFArgs& mfargs);

    void jump(CodeAddress newpc) {
        E4LOG1("[proc] jump 0x%zx\n", newpc.ptr_);
        context_.pc_ = newpc;
    }
};

} // ns e4
