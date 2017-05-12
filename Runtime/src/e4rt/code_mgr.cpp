// This is an open source non-commercial project. Dear PVS-Studio, please check it.
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
  auto m = platf::SystemAllocator::alloc_one<Module>(vm_);
  m->load(BoxView<uint8_t>::view(data));

  auto m_name = m->name();
  register_module(std::move(m));

  return m_name;
}


void CodeManager::register_module(UniquePtr<Module> && m) {
  Term m_name = m->name();

  #if E4FEATURE_HOTCODELOAD
  auto old_m = mods_.find(m_name);
  if (old_m) {
    // Unload old module or rotate?
    E4TODO("Module unload/duplicate load")
  }
#endif

  mods_[m_name] = std::move(m);
}

}  // ns e4
