//
// Term memory layout rules
//
#pragma once

#include "g_platform/types.h"

namespace gluon { namespace layout {

    // Memory layout rules for tuples
    class Tuple {
    public:
        static constexpr Word box_size(Word arity) { return arity+1; }
        // Query tuple element using zero-based index
        static Term element(BoxHeader *box, Word z_index) {
            return Term(box->element(z_index));
        }
        static void set_element(BoxHeader *box, Word z_index, Term val) {
            return box->set_element(z_index, val.get_raw());
        }
    };

    } // ns gluon::layout
} // ns gluon
