//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#include "e4rt/messages.h"
#include "e4rt/process.h"

namespace e4 {

VoidResult Process::apply(const MFArgs& mfargs) {
    auto mod = vm_.modules_.find(mfargs.mod_);
    if (!mod) {
        return VoidResult::fail(e4err::proc_not_exist);
    }

    mfargs.print(vm_);
    auto pexport = mod->find_export(mfargs.as_mfarity());
    if (!pexport) {
        return VoidResult::fail(e4err::code_undef);
    }

    // TODO: Set call arguments
    // TODO: jump
    return VoidResult::success();
}

} // ns e4
