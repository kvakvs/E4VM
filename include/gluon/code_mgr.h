#pragma once

#include "gluon/types.h"
#include "gluon/module.h"
#include "gluon/conf.h"

namespace gluon {

    class CodeManager {
    private:
        Dict<String, std::shared_ptr<Module>> mods_;
    public:
        explicit CodeManager() {
            // TODO: load preloaded modules
        }

        void from_file(const char *fn);
    };

} // ns gluon
