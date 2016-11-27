#pragma once

#include "gluon/code_mgr.h"

namespace gluon {

    // Gluon Erlang Abstract Machine (GLEAM)
    class Gleam {
    private:
    public:
        CodeManager modules_;

        explicit Gleam() {}
    };

} // ns gluon
