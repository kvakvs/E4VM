#include "g_erts/vm.h"

namespace gluon {

namespace err {
    IMPL_EXCEPTION(FeatureMissing)
    IMPL_EXCEPTION(TODO)
    IMPL_EXCEPTION(BeamLoad)
    IMPL_EXCEPTION(Scheduler)
    IMPL_EXCEPTION(CodeServer)
    IMPL_EXCEPTION(Process)
}  // ns err

Term VM::add_atom(const String &atom_name) {
    CString atom_namei(atom_name.c_str());

    // Try find atom (already exists)
    auto rev_i = atoms_reverse_.find(atom_namei);
    if (rev_i != atoms_reverse_.end()) {
        // duplicate
        return Term::make_atom(rev_i->second);
    }
    dprintf("add atom: %s\n", (const char *) atom_namei);

    G_ASSERT(fits_in<Word>(atoms_.size())); // 64bit machine with 32bit words
    Word atom_id = (Word)atoms_.size();

    // Add name to interned names and use only pointer to it in dicts
    atom_interned_names_.push_back(atom_name);
    CString interned_name(atom_interned_names_.back().c_str());

    // Add atom to direct lookup and reverse lookup table
    atoms_[atom_id] = interned_name;
    atoms_reverse_[interned_name] = atom_id;
    return Term::make_atom(atom_id);
}

Node *VM::dist_this_node() {
#if GLUON_FEATURE_ERLDIST
    G_TODO("implement Node and this node variable")
#endif
    return this_node_;
}

} // ns g_erts
