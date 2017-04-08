// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4rt/process.h"
#include "e4rt/messages.h"

namespace e4 {

VoidResult Process::apply(const MFArgs& mfargs) {
  auto mod = vm_.modules_.find(mfargs.mod_);
  if (!mod) {
    return VoidResult::fail(e4err::mod_not_exist);
  }

  auto pexport = mod->find_export(mfargs.as_mfarity());
  if (!pexport) {
    return VoidResult::fail(e4err::code_undef);
  }

  // Reverse order: push args
  auto arg_first = mfargs.args_.first();
  for (const Term* arg = arg_first + mfargs.args_.count(); arg != arg_first;
       --arg) {
    context_.ds_.push_term(*arg);
  }

  E4LOG("[proc] Entering function\n");
  jump(mod->get_export_address(*pexport));
  return VoidResult::success();
}

Process::Process(VM& vm, Term pid)
    : pid_(pid),
      heap_(INIT_PROCESS_HEAP),
      vm_(vm),
      context_(vm.get_code_range_checker()) {}

}  // ns e4
