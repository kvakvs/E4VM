// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4rt/process.h"
#include "e4rt/vm.h"
#include <cstdlib>
#include <stdio.h>

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

  auto proc = platf::SystemAllocator::alloc_one<Process>(*this, pid);
  auto apply_res = proc->apply(mfargs);
  apply_res.dassert();

  auto result = proc.get();
  sched_.register_proc(result);
  return result;
}

#if E4DEBUG
void VM::print(Term t) const {
  switch (t.as_primary_.get_primary_tag()) {
    case PrimaryTag::Immediate: {
      print_imm(t);
    } break;
    case PrimaryTag::Boxed: {
    } break;
    case PrimaryTag::Header: {
    } break;
    case PrimaryTag::Cons: {
    } break;
  }
}
#endif  // DEBUG

#if E4DEBUG
void VM::print_imm(Term t) const {
  if (t.is_immed1()) {
    switch (t.as_imm1_.get_imm1_tag()) {
      case Immed1Tag::Small: {
        debug_printf("%d", t.as_imm1_.get_signed_val());
      } break;

      case Immed1Tag::Pid: {
        debug_printf("#pid<%zu>", t.as_imm1_.get_value());
      } break;

      case Immed1Tag::Port: {
        debug_printf("#port<%zu>", t.as_imm1_.get_value());
      } break;

      case Immed1Tag::Immed2: {
        print_imm_imm2(t);
      } break;
    }
  }
}

void VM::print_imm_imm2(const Term &t) const {
  switch (t.as_imm2_.get_imm2_tag()) {
    case Immed2Tag::Atom: {
      debug_printf("'%s'", find_atom(t));
    } break;

    case Immed2Tag::Catch:
      break;

    case Immed2Tag::Special: {
      switch (t.as_imm2_.get_value()) {
        case 0:
          debug_printf("[]");
          break;
        case 1:
          debug_printf("#NonV");
          break;
        default:
          break;
      }
    } break;

    case Immed2Tag::Immed3: {
      print_imm_imm3(t);
    } break;
  }
}

void VM::print_imm_imm3(const Term &t) const {
  switch (t.as_imm3_.get_imm3_tag()) {
    case Immed3Tag::FloatReg: {
      debug_printf("#fp<%d>", t.as_imm3_.get_value());
    } break;

    case Immed3Tag::XReg: {
      debug_printf("#x<%d>", t.as_imm3_.get_value());
    } break;

    case Immed3Tag::Label: {
      debug_printf("#label<%d>", t.as_imm3_.get_value());
    } break;

    case Immed3Tag::YReg: {
      debug_printf("#y<%d>", t.as_imm3_.get_value());
    } break;
  }
}

#endif  // DEBUG

const char* VM::find_atom(Term atom) const {
  E4ASSERT(atom.is_atom());
  return atoms_.find(atom);
}

}  // ns e4
