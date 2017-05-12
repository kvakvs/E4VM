// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/conf.h"
#include "e4platf/types.h"

#include "e4rt/module.h"
#include "e4rt/term.h"
#include "e4rt/term_as_map_key.h"

namespace e4 {

class VM;

class CodeManager {
 private:
  VM& vm_;

  HashMap<Term, UniquePtr<Module>> mods_;

  Vector<String> paths_;  // Code search paths, starting with "."

 public:
  explicit CodeManager(VM& vm) : vm_(vm), mods_() {
    // TODO: load preloaded modules
    paths_.push_back(String("."));
  }

  Term load(Term name);

  void register_module(UniquePtr<Module> && m);

  const Module* find(Term name) const {
    auto node = mods_.find(name);
    return (node != mods_.end()) ? node->second.get() : nullptr;
  }

  void path_add(const String& p) { paths_.push_back(p); }
};

}  // ns e4
