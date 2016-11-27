#include "gluon/vm.h"

namespace gluon {

void VM::add_atom(const String &atom_name) {
    CString atom_namei(atom_name.c_str());
    if (atoms_reverse_.find(atom_namei) != atoms_reverse_.end()) {
        // duplicate
        return;
    }
    dprintf("add atom: %s\n", (const char *) atom_namei);

    auto atom_id = atoms_.size();

    // Add name to interned names and use only pointer to it in dicts
    atom_interned_names_.push_back(atom_name);
    CString interned_name(atom_interned_names_.back().c_str());

    // Add atom to direct lookup and reverse lookup table
    atoms_[atom_id] = interned_name;
    atoms_reverse_[interned_name] = atom_id;
}

} // ns gluon
