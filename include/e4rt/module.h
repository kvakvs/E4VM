/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */
#pragma once

#include "e4platf/debug.h"
#include "e4platf/types.h"

#include "e4rt/term.h"
#include "e4rt/heap.h"
#include "e4rt/module.h"

#include "e4std/view.h"

namespace e4 {

class VM;

class Module {
private:
    Term name_ = NON_VALUE; // atom name
    Vector<Uint8> code_;
    Heap literals_;
    VM& vm_;

public:
    explicit Module(VM& vm) : literals_(64), vm_(vm) {}

    void load(const e4std::BoxView<Uint8>& data);

    Term name() const { return name_; }

private:
    void load_atoms(const e4std::BoxView<Uint8>& adata, Vector<String>& out);
    void load_literals(const e4std::BoxView<Uint8>& adata, Vector<Term>& out);
};

} // ns e4
