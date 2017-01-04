#include "gluon.h"

int main(int argc, const char *argv[]) {
    gluon::VM vm;
    vm.modules_.load_file(vm, "e4compile/priv/test1.e4b");
    return 0;
}
