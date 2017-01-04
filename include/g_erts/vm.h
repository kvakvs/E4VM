#pragma once

#include "g_erts/code_mgr.h"
#include "g_erts/dist.h"

#include "g_platform/interned_string.h"

namespace gluon {

namespace err {
    DECL_EXCEPTION(FeatureMissing)
    DECL_EXCEPTION(TODO)
    DECL_EXCEPTION(BeamLoad)
    DECL_EXCEPTION(Scheduler)
    DECL_EXCEPTION(CodeServer)
    DECL_EXCEPTION(Process)
}  // ns err

// Gluon Erlang Abstract Machine (GLEAM)
class VM {
private:
    // Stores all atom names once
    Vector<String> atom_interned_names_;
    Dict<Word, CString> atoms_;
    Dict<CString, Word> atoms_reverse_;

    Node *this_node_ = nullptr;

public:
    CodeManager modules_;

    explicit VM() {}

    Term add_atom(const String &atom_name);
    Node *dist_this_node();
};

} // ns g_erts
