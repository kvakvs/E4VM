#include "gluon.h"

int main(int argc, const char *argv[]) {
    gluon::Gleam vm;
    vm.modules_.from_file("test1");
    return 0;
}