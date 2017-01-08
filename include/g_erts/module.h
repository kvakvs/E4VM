/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "g_platform/debug.h"
#include "g_platform/types.h"

#include "g_erts/term.h"
#include "g_erts/heap.h"
#include "g_erts/module.h"

namespace gluon {

class VM;

class Module {
private:
    Term name_ = NON_VALUE; // atom name
    UniqueBox<Uint8> code_;
    Heap literals_;
    VM &vm_;

public:
    explicit Module(VM &vm): literals_(64), vm_(vm) {}
    void load(const BoxView<Uint8> &data);
    Term name() const { return name_; }

private:
    static BoxView<Uint8> find_section(const char *want_sig,
                                       const BoxView<Uint8> &data);
    Vector<String> load_atoms(const BoxView <Uint8> &adata);
    Vector<Term> load_literals(const BoxView<Uint8> &adata);
};

} // ns g_erts
