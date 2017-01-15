//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4.h"

extern "C" void e4_rtems_main() {
    e4::VM vm;
    using e4std::String;

    vm.modules_.path_add(String("/mnt"));
    vm.modules_.load(vm, "test1.e4b");
}
