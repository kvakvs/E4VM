#pragma once

#include "gluon/code_mgr.h"
#include "gluon/interned_string.h"

namespace gluon {

    // Gluon Erlang Abstract Machine (GLEAM)
    class VM {
    private:
        // Stores all atom names once
        Vector<String> atom_interned_names_;
        Dict<Word, CString> atoms_;
        Dict<CString, Word> atoms_reverse_;

    public:
        CodeManager modules_;

        explicit VM() {}

        void add_atom(const String &atom_name);
    };

} // ns gluon
