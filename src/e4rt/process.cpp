//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#include "e4rt/messages.h"
#include "e4rt/process.h"

namespace e4 {

MaybeError Process::apply(const e4::VM& vm, const MFArgs& mfargs) {
    auto mod = vm.modules_.find(mfargs.mod_);
    if (!mod) {
        return MaybeError::fail(e4err::proc_not_exist);
    }

    auto pexport = mod->find_export(mfargs.as_mfarity());
    if (!pexport) {
        return MaybeError::fail(e4err::code_undef);
    }
    return MaybeError::success();
}

} // ns e4
