// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/debug.h"
#include "e4platf/fs.h"

#include "e4rt/code_mgr.h"
#include "e4rt/vm.h"

namespace e4 {

Term CodeManager::load(Term modn) {
  String mod_filename(vm_.find_atom(modn));
  mod_filename += ".e4b";

  auto data = platf::fs::read(paths_, mod_filename.c_str());
  auto m = new Module(vm_);
  m->load(ByteView(data));
  register_module(m);
  return m->name();
}

void CodeManager::register_module(Module *m) {
  Term mname = m->name();
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

  vm_.print(mname);
  mods_.insert(mname, m);
}

}  // ns e4
