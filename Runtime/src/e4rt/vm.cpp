// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4rt/process.h"
#include "e4rt/vm.h"
#include <cstdlib>
#include <stdio.h>

namespace e4 {

VM* VM::singleton_ = nullptr;

IMPL_EXCEPTION(FeatureMissing)
IMPL_EXCEPTION(CodeLoader)
IMPL_EXCEPTION(Scheduler)
IMPL_EXCEPTION(CodeServer)
IMPL_EXCEPTION(Process)

Term VM::add_atom(const String& atom_name) {
  auto atom_name_s = atom_name.c_str();

  // Try find atom (already exists)
  auto exists = atom_store_.find(atom_name_s);
  if (exists.is_value()) {  // duplicate
    return exists;
  }

  E4ASSERT(fits_in<Word>(atom_store_.size()));  // 64bit machine with 32bit words

  // Add atom to direct lookup and reverse lookup table
  auto a = Term::make_atom(atom_id_++);
  atom_store_.insert(a, atom_name_s);
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

  auto proc = platf::SystemAllocator::alloc_one<Process>(pid);

  auto err = proc->apply(mfargs);
  err.assert_success();

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
      debug_printf("#box<%p>", t.unbox());
    } break;
    case PrimaryTag::Header: {
      debug_printf("#hdr<%p>", t.unbox());
    } break;
    case PrimaryTag::Cons: {
      debug_printf("#cons<%p>", t.unbox());
    } break;
  }
}
#endif  // DEBUG


#if E4DEBUG
void VM::print_imm(Term t) const {
  if (t.is_immediate()) {
    switch (t.as_imm1_.get_imm1_tag()) {
      case Immed1Tag::Small: {
        return debug_printf("%d", t.as_imm1_.get_signed_val());
      }

      case Immed1Tag::Pid: {
        return debug_printf("#pid<%zu>", t.as_imm1_.get_value());
      }

      case Immed1Tag::Port: {
        return debug_printf("#port<%zu>", t.as_imm1_.get_value());
      }

      case Immed1Tag::Immed2: {
        return print_imm_imm2(t);
      }
    }
  }
  E4FAIL("imm1 print fail");
}


void VM::print_imm_imm2(const Term &t) const {
  switch (t.as_imm2_.get_imm2_tag()) {
    case Immed2Tag::Atom: {
      return debug_printf("'%s'", find_atom(t));
    }

    case Immed2Tag::Catch:
      return debug_printf("#catch<%p>", t.as_imm2_.get_value());

    case Immed2Tag::Special: {
      switch (t.as_imm2_.get_value()) {
        case 0:
          return debug_printf("[]");
        case 1:
          return debug_printf("#NonV");
        default:
          return debug_printf("#?imm2:%d", t.as_imm2_.get_value());
      }
    } break;

    case Immed2Tag::Immed3: {
      return print_imm_imm3(t);
    }
  }
  E4FAIL("imm2 print fail");
}

void VM::print_imm_imm3(const Term &t) const {
  switch (t.as_imm3_.get_imm3_tag()) {
    case Immed3Tag::FloatReg: {
      return debug_printf("#fp<%d>", t.as_imm3_.get_value());
    }

    case Immed3Tag::XReg: {
      return debug_printf("#x<%d>", t.as_imm3_.get_value());
    }

    case Immed3Tag::Label: {
      return debug_printf("#label<%d>", t.as_imm3_.get_value());
    }

    case Immed3Tag::YReg: {
      return debug_printf("#y<%d>", t.as_imm3_.get_value());
    }
  }
  E4FAIL("imm3 print fail");
}

#endif  // DEBUG

const char* VM::find_atom(Term atom) const {
  E4ASSERT(atom.is_atom());
  return atom_store_.find(atom);
}

}  // ns e4
