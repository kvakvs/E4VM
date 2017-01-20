/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4rt/code_mgr.h"
#include "e4rt/atom_store.h"
#include "e4rt/scheduler.h"
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
    AtomStore atoms_;
    Node* this_node_ = nullptr;
    Scheduler sched_;

public:
    CodeManager modules_;
    Heap binary_heap_;

    explicit VM() : binary_heap_(1024) {}
    void run();
    inline void run_alu(J1Opcode instr);

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

#if E4DEBUG
    void print(Term t) const;
    void print_imm(Term t) const;
//    void print_atoms() const;
#endif
};

} // ns e4
