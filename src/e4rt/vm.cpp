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
    auto proc = SingleAlloc::alloc_class<Process>(*this, pid);
    auto apply_res = proc->apply(mfargs);
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

#if E4DEBUG
void VM::print(Term t) const {
    switch (t.as_primary_.tag_) {
        case primary_tag::Immediate: {
            print_imm(t);
        } break;
        case primary_tag::Boxed: {} break;
        case primary_tag::Header: {} break;
        case primary_tag::Cons: {} break;
    }
}
#endif // DEBUG

#if E4DEBUG
void VM::print_imm(Term t) const {
    switch (t.as_imm_.imm_tag_) {
        case immediate_tag::Atom: {
            debug_printf("'%s'", find_atom(t));
        } break;
        case immediate_tag::SmallInt: {} break;
        case immediate_tag::ShortPid: {} break;
        case immediate_tag::ShortPort: {} break;
        case immediate_tag::FpRegister: {} break;
        case immediate_tag::Catch: {} break;
        case immediate_tag::XRegister: {} break;
        case immediate_tag::YRegister: {} break;
        case immediate_tag::Special: {} break;
    }
}

const char* VM::find_atom(Term atom) const {
    E4ASSERT(atom.is_atom());
    auto node = atoms_.find(atom);
    if (!node) {
        return "?atom";
    }
    return node->value_;
}

#endif // DEBUG

} // ns e4
