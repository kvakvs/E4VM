#pragma once

#include "gluon/types.h"
#include "gluon/module.h"
#include "gluon/debug.h"

namespace gluon {

class VM;

class Module {
private:
    UniqueBox<Uint8> code_;
public:
    Module(VM &vm, const BoxView<Uint8> &data) {
        load(vm, data);
    }

    void load(VM &vm, const BoxView<Uint8> &data);

private:
    static BoxView<Uint8> find_section(const char *signature,
                                       const BoxView<Uint8> &data);
    Vector <String> load_atoms(const BoxView <Uint8> &adata);
};

} // ns gluon
