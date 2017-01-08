//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "gluon.h"

int main(int /*argc*/, const char ** /*argv [] */) {
    gluon::VM vm;
    vm.modules_.load_file(vm, "e4compile/priv/test1.e4b");
    return 0;
}
