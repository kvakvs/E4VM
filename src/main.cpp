#include "gluon.h"

int main(int argc, const char *argv[]) {
    gluon::VM vm;
    vm.modules_.from_file(vm, "../test/test1.erl.3eam");
    return 0;
}