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
    // Code search paths, starting with "."
    Vector<String> paths_;

public:
    explicit CodeManager() : mods_() {
        // TODO: load preloaded modules
        paths_.push_back(String("."));
    }

    void load(VM& vm, const char* name);

    void add(Module* m) {
        mods_.insert(m->name().get_raw(), m);
    }

    void path_add(const String &p) {
        paths_.push_back(p);
    }
};

} // ns e4
