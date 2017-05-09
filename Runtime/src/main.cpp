//
// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4.h"

int main(int /*argc*/, const char** /*argv [] */) {
  e4::VM vm;
  using e4std::String;

  vm.modules_.path_add(String("../Compiler/priv"));
  auto mod_name = vm.add_atom(String("test1"));
  vm.modules_.load(mod_name);

  e4::MFArgs mfargs(mod_name, mod_name, e4std::ArrayRef<e4::Term>());
  // vm.print_atoms();
  auto root_proc = vm.spawn(e4::NON_VALUE, mfargs);
  (void)root_proc;

  vm.run();

  return 0;
}
