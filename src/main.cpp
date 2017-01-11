//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4.h"

int main(int /*argc*/, const char ** /*argv [] */) {
    e4::VM vm;
    vm.modules_.path_add("e4compile/priv");
    vm.modules_.load(vm, "test1.e4b");
    return 0;
}
