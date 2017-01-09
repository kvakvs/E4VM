//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/debug.h"
#include "e4platf/fs.h"

#include "e4rt/vm.h"
#include "e4rt/code_mgr.h"

namespace e4 {

void CodeManager::load_file(VM &vm, const char *fn) {
    auto data = File::read_file(fn);
    auto m = new Module(vm);
    m->load(e4std::BoxView<Uint8>(data));
    vm.modules_.add(m);
}

} // ns e4
