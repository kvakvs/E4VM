#pragma once

#include "g_platform/types.h"
#include "g_platform/conf.h"

#include "g_erts/module.h"
#include "g_erts/term.h"

namespace gluon {

class VM;

class CodeManager {
private:
    Dict<Word, std::shared_ptr<Module>> mods_;
public:
    explicit CodeManager() {
        // TODO: load preloaded modules
    }

    void load_file(VM &vm, const char *fn);
    void add(std::shared_ptr<Module> &m) {  }
};

} // ns g_erts
