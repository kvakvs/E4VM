/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */


#pragma once

#include "e4platf/types.h"
#include "e4platf/conf.h"

#include "e4rt/module.h"
#include "e4rt/term.h"

namespace e4 {

class VM;

class CodeManager {
private:
    Map<Word, Module*> mods_;
public:
    explicit CodeManager(): mods_() {
        // TODO: load preloaded modules
    }

    void load_file(VM& vm, const char* fn);

    void add(Module* m) {
        mods_.insert(m->name().get_raw(), m);
    }
};

} // ns e4
