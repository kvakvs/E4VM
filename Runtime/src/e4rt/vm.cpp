// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include <stdio.h>
#include <cstdlib>

#include "e4rt/vm.h"
//#include "e4platf/mem.h"
#include "e4rt/process.h"

namespace e4 {

IMPL_EXCEPTION(FeatureMissing)
IMPL_EXCEPTION(CodeLoader)
IMPL_EXCEPTION(Scheduler)
IMPL_EXCEPTION(CodeServer)
IMPL_EXCEPTION(Process)

Term VM::add_atom(const String& atom_name) {
  auto atom_name_s = atom_name.c_str();

  // Try find atom (already exists)
  auto exists = atoms_.find(atom_name_s);
  if (exists.is_value()) {  // duplicate
    return exists;
  }
  E4ASSERT(fits_in<Word>(atoms_.size()));  // 64bit machine with 32bit words
  Word atom_id = static_cast<Word>(atoms_.size());

  // Add atom to direct lookup and reverse lookup table
  auto a = Term::make_atom(atom_id);
  atoms_.insert(a, atom_name_s);
  return a;
}

Node* VM::dist_this_node() {
#if E4FEATURE_ERLDIST
  E4TODO("notimpl dist_this_node");
#endif
  return this_node_;
}

Process* VM::spawn(Term parent_pid, const MFArgs& mfargs) {
  (void)parent_pid;

  auto pid = sched_.make_pid();

  using platf::SingleAlloc;
  auto proc = SingleAlloc::alloc_class<Process>(*this, pid);
  auto apply_res = proc->apply(mfargs);
  apply_res.dassert();

  sched_.register_proc(proc);
  return proc;
}

#if E4DEBUG
void VM::print(Term t) const {
  switch (t.as_primary_.tag_) {
    case primary_tag::Immediate: {
      print_imm(t);
    } break;
    case primary_tag::Boxed: {
    } break;
    case primary_tag::Header: {
    } break;
    case primary_tag::Cons: {
    } break;
  }
}
#endif  // DEBUG

#if E4DEBUG
void VM::print_imm(Term t) const {
  switch (t.as_imm_.imm_tag_) {
    case immediate_tag::Atom: {
      debug_printf("'%s'", find_atom(t));
    } break;
    case immediate_tag::SmallInt: {
      debug_printf("%d", t.as_small_.val_);
    } break;
    case immediate_tag::ShortPid: {
    } break;
    case immediate_tag::ShortPort: {
    } break;
    case immediate_tag::FpRegister: {
      debug_printf("#fp<%d>", t.as_imm_.val_);
    } break;
    case immediate_tag::Catch: {
    } break;
    case immediate_tag::XRegister: {
      debug_printf("#x<%d>", t.as_imm_.val_);
    } break;
    case immediate_tag::YRegister: {
      debug_printf("#y<%d>", t.as_imm_.val_);
    } break;
    case immediate_tag::Special: {
    } break;
  }
}
#endif  // DEBUG

const char* VM::find_atom(Term atom) const {
  E4ASSERT(atom.is_atom());
  return atoms_.find(atom);
}

#ifdef DEBUG
#define VMDBG(T) e4::debug_printf("vm loop: " T "\n")
#else
#define VMDBG(T)
#endif

// Switch-based VM loop (slower but compact code and compact bytecode)
//
// To go faster: see threaded goto(void*) VM loop and convert bytecode to
// label addresses during the load-time.
void VM::run() {
  // schedule:
  auto proc = sched_.next();
  if (not proc) {
    VMDBG("idle");
    return;  // nothing to do
             // TODO: perform maintenance tasks, enter energy saving idle
  }

  auto& context_ = proc->context_;

  auto pc0 = context_.pc_.ptr();
  auto pc_last = pc0 + 100;

fetch:
  Instruction instruction = (Instruction)(*pc0);
  E4LOG2("[%p] %02x ", pc0, instruction);
  pc0++;

  switch (instruction) {
    case instr::FuncInfo:break;
    case instr::CallLocal:break;
    case instr::CallExt:break;
    case instr::Bif:break;
    case instr::AllocStack:break;
    case instr::AllocStackHeap:break;
    case instr::GetElement:break;
    case instr::Move:break;
    case instr::CallFun:break;
    case instr::SetNil:break;
    case instr::TestHeap:break;
    case instr::PutTuple:break;
    case instr::Put:break;
    case instr::Ret0:break;
    case instr::RetN:break;
    case instr::SelectVal:break;
    case instr::Cons:break;
    case instr::Jump:
//      proc->jump_rel(offs);
      break;
    case instr::Trim:break;
    case instr::MakeFun:break;
    case instr::SetElement:break;
    case instr::ClearStack:break;
  }

  E4LOG("\n");
  if (pc0 < pc_last) goto fetch;
}

}  // ns e4
