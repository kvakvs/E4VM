//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/debug.h"
#include "e4platf/fs.h"

#include "e4rt/vm.h"
#include "e4rt/code_mgr.h"

namespace e4 {

Term CodeManager::load(VM &vm, const char *fn) {
    auto data = platf::fs::read(paths_, fn);
    auto m = new Module(vm);
    m->load(ByteView(data));
    vm.modules_.add(m);
    return m->name();
}

void CodeManager::add(Module* m) {
    auto mname = m->name();
    auto old_m = mods_.find(mname);
#if E4FEATURE_HOTCODELOAD
    if (old_m) {
        // Unload old module or rotate?
        E4TODO("Module unload/duplicate load")
    }
#else
    if (old_m) {
        delete old_m->value_;
    }
    mods_.remove(mname);
#endif
    mods_.insert(mname, m);
}

} // ns e4
