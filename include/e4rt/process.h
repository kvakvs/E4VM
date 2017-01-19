#pragma once

#include "e4rt/vm.h"
#include "e4rt/term.h"

namespace e4 {

class Process {
private:
    Term pid_;
    // TODO: vm runtime context

public:
    Process() = delete;
    Process(Term pid): pid_(pid) {
    }

    // Sets arguments and enters mfarity with args, does not wait for execution
    // but just sets instruction pointer instead
    void apply(const e4::VM& vm, const MFArgs& mfargs);
};

} // ns e4
