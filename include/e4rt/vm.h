/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4rt/code_mgr.h"
#include "e4rt/dist.h"

#include "e4std/array.h"
#include "e4std/string.h"
#include "atom_store.h"

namespace e4 {

DECL_EXCEPTION(FeatureMissing)
DECL_EXCEPTION(CodeLoader)
DECL_EXCEPTION(Scheduler)
DECL_EXCEPTION(CodeServer)
DECL_EXCEPTION(Process)

class Process;
using e4std::ArrayRef;

// Erlang-Forth Abstract Machine (E4VM)
class VM {
private:
    // Stores all atom names once
    AtomStore atoms_;

    Node* this_node_ = nullptr;

    Word pid_counter_ = 0;
    Map<Term, Process*> processes_;

public:
    CodeManager modules_;
    Heap binary_heap_;

    explicit VM() : binary_heap_(1024) {}

    //
    // Atom storage stuff
    //
    Term add_atom(const String& atom_name);
    const char* find_atom(Term atom) const;

    Node* dist_this_node();

    //
    // Process and pid stuff
    //
    Process* spawn(Term parent_pid, const MFArgs& mfargs);
    Term make_pid();

#if E4DEBUG
    void print(Term t) const;
    void print_imm(Term t) const;
//    void print_atoms() const;
#endif
};

} // ns e4
