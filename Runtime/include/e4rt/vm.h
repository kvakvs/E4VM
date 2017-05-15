// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4rt/atom_store.h"
#include "e4rt/code_mgr.h"
#include "e4rt/dist.h"
#include "e4rt/range_checker.h"
#include "e4rt/scheduler.h"
#include "e4std/array.h"
#include "e4std/string.h"
#include "process.h"

namespace e4 {

DECL_EXCEPTION(FeatureMissing)
DECL_EXCEPTION(CodeLoader)
DECL_EXCEPTION(Scheduler)
DECL_EXCEPTION(CodeServer)
DECL_EXCEPTION(Process)

class Process;
using e4::ArrayRef;

// Erlang-Forth Abstract Machine (E4VM)
class VM {
private:
  Node* this_node_ = nullptr;

  Scheduler sched_;

public:
  static VM* singleton_;

  Word atom_id_ = 0;

  AtomStore atom_store_;

  CodeManager modules_;

  Heap binary_heap_;

  RangeChecker range_checker_;

  explicit VM()
    : modules_(),
      binary_heap_(1024),
      range_checker_(nullptr, nullptr)
  {
    singleton_ = this;
  }  //-V730

  void run();

  const RangeChecker& get_code_range_checker() { return range_checker_; }

  //
  // Atom storage stuff
  //
  Term add_atom(const String& atom_name);

  const char* find_atom(Term atom) const;

  Node* dist_this_node();

  //
  // Process and pid stuff
  //
  Process* spawn(Term parent_pid, const MFArgs& mfargs);

#if E4DEBUG

  void print(Term t) const;

  void print_imm(Term t) const;
//    void print_atoms() const;
#endif

  void print_imm_imm3(const Term& t) const;

  void print_imm_imm2(const Term& t) const;
};

inline VM* vm() { return VM::singleton_; }

}  // ns e4
