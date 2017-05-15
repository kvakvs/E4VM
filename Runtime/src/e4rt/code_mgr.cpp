// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/debug.h"
#include "e4platf/fs.h"
#include "e4rt/code_mgr.h"
#include "e4rt/vm.h"

namespace e4 {


Term CodeManager::load(Term modn) {
  String mod_filename(vm()->find_atom(modn));

  mod_filename += ".e4b";

  auto data = platf::fs::read(paths_, mod_filename.c_str());
  auto m = platf::SystemAllocator::alloc_one<Module>();

  m->load(BoxView<uint8_t>::view(data));

  auto tmp = m->get_name();
  register_module(std::move(m));
  return tmp;
}


void CodeManager::register_module(UniquePtr<Module>&& m) {
  Term m_name = m->get_name();

  #if E4FEATURE_HOTCODELOAD
  auto old_m = mods_.find(m_name);
  if (old_m) {
    // Unload old module or rotate?
    E4TODO("Module unload/duplicate load")
  }
#endif

  mods_.emplace(m_name, std::move(m));
}


Module* CodeManager::find_module(Term name) const {
  ::printf("find module ");
  vm()->print(name);
  ::printf("\n");

  auto node = mods_.find(name);
  return (node != mods_.end()) ? node->second.get() : nullptr;
}


#if E4DEBUG
void CodeManager::debug_print() {
  for (auto& p: mods_) {
    vm()->print(p.first);
    ::printf(" -> %p\n", p.second.get());
  }
}
#endif

}  // ns e4
