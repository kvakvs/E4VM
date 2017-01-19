//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4rt/vm.h"
//#include "e4platf/mem.h"
#include "e4rt/process.h"

namespace e4 {

IMPL_EXCEPTION(FeatureMissing)
IMPL_EXCEPTION(CodeLoader)
IMPL_EXCEPTION(Scheduler)
IMPL_EXCEPTION(CodeServer)
IMPL_EXCEPTION(Process)

Term VM::add_atom(const String &atom_name) {
    CString atom_namei(atom_name.c_str());

    // Try find atom (already exists)
    auto rev_i = atoms_reverse_.find(atom_namei);
    if (rev_i) {
        // duplicate
        return rev_i->value_;
    }
    E4ASSERT(fits_in<Word>(atoms_.size())); // 64bit machine with 32bit words
    Word atom_id = static_cast<Word>(atoms_.size());

    // Add name to interned names and use only pointer to it in dicts
    atom_interned_names_.push_back(atom_name);
    CString interned_name(atom_interned_names_.back().c_str());

    // Add atom to direct lookup and reverse lookup table
    auto a = Term::make_atom(atom_id);
    atoms_.insert(a, interned_name);
    atoms_reverse_.insert(interned_name, a);
    return a;
}

Node *VM::dist_this_node() {
#if E4FEATURE_ERLDIST
    E4TODO("implement Node and this node variable")
#endif
    return this_node_;
}

Process* VM::spawn(Term parent_pid, const MFArgs& mfargs) {
    (void)parent_pid;

    auto pid = make_pid();

    using platf::SingleAlloc;
    auto proc = SingleAlloc::alloc_class<Process>(pid);
    auto apply_res = proc->apply(*this, mfargs);
    apply_res.assert();

    processes_.insert(pid, proc);
    return nullptr;
}

Term VM::make_pid() {
    auto t = Term::make_short_pid(pid_counter_++);
    // TODO: implement wrap when word counter overflows
    E4ASSERT(processes_.find(t) == nullptr);
    return t;
}

} // ns e4
