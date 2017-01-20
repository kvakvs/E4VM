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

//    mfargs.print(vm_);
    auto pexport = mod->find_export(mfargs.as_mfarity());
    if (!pexport) {
        return VoidResult::fail(e4err::code_undef);
    }

    // Reverse order: push args
    auto arg_first = mfargs.args_.first();
    for (const Term* arg = arg_first + mfargs.args_.count();
         arg != arg_first; --arg) {
        stack_.push_term(*arg);
    }

    jump(mod->get_export_address(*pexport));
    return VoidResult::success();
}

} // ns e4
