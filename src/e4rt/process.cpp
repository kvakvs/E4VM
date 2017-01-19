#include "e4rt/process.h"

namespace e4 {

void Process::apply(const e4::VM& vm, const MFArgs& mfargs) {
    auto mod = vm.modules_.find(mfargs.mod_);
    if (!mod) {
        // Not found
        return;
    }
}

} // ns e4
