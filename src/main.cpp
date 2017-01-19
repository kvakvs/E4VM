//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4.h"

int main(int /*argc*/, const char ** /*argv [] */) {
    e4::VM vm;
    using e4std::String;

    vm.modules_.path_add(String("e4compile/priv"));
    auto mod_name = vm.modules_.load(vm, "test1.e4b");

    e4::MFArgs mfargs(mod_name, vm.add_atom(e4std::String("start")),
                      e4std::ArrayRef<e4::Term>());
    auto root_proc = vm.spawn(e4::NON_VALUE, mfargs);
    (void)root_proc;

    return 0;
}
