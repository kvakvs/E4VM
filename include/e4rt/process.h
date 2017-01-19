//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#pragma once

#include "e4rt/vm.h"
#include "e4rt/term.h"
#include "e4std/complicated.h"

namespace e4 {
using e4std::MaybeError;

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
    E4_NODISCARD MaybeError
    apply(const e4::VM& vm, const MFArgs& mfargs);
};

} // ns e4
