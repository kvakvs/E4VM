/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4rt/code_mgr.h"
#include "e4rt/dist.h"

#include "e4std/array.h"
#include "e4std/string.h"

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
    Vector<String> atom_interned_names_;
    Map<Term, CString> atoms_;
    Map<CString, Term> atoms_reverse_;

    Node* this_node_ = nullptr;

    Word pid_counter_ = 0;
    Map<Term, Process*> processes_;

public:
    CodeManager modules_;
    Heap binary_heap_;

    explicit VM() : binary_heap_(1024) {}

    Term add_atom(const String& atom_name);
    Node* dist_this_node();

    Process* spawn(Term parent_pid, const MFArgs& mfargs);
    Term make_pid();
};

} // ns e4
