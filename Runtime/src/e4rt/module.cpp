// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include <stdio.h>

#include "e4platf/byte_stream_reader.h"

#include "e4rt/ext_term_format.h"
#include "e4rt/module.h"
#include "e4rt/vm.h"

namespace e4 {

constexpr Word SIG_SIZE = 2;  // module and section signature length
constexpr const char* SIG_MODULE = "E4";  // Erl-Forth BEAM-like format

// This is asserted in loader code below
#if E4_WORD_SIZE == 32
constexpr const char* SIG_MODULE_BITNESS = "32";  // E432 marks 4 byte terms
#elif E4_WORD_SIZE == 64
constexpr const char* SIG_MODULE_BITNESS = "64";  // E464 marks 8 byte terms
#endif

constexpr const char* SIG_ATOMS = "At";  // atoms section tag
constexpr const char* SIG_LBLS  = "Lb";  // labels section tag
constexpr const char* SIG_CODE  = "Co";  // code section tag
constexpr const char* SIG_LTRL  = "Lt";  // literals section tag
constexpr const char* SIG_EXPT  = "Xp";  // exports section tag
constexpr const char* SIG_IMPT  = "Im";  // imports section tag
constexpr const char* SIG_JMPT  = "Jt";  // jump table tag
constexpr const char* SIG_FUNT  = "Fn";  // function table tag


void Module::load(const ByteView& data) {
  tool::Reader bsr(data);

  // Read header E4
  bsr.assert_and_advance(SIG_MODULE, ByteSize(SIG_SIZE));

  // Read bitness, E432 or E464 and assert
  bsr.assert_and_advance(SIG_MODULE_BITNESS, ByteSize(SIG_SIZE));

  // Storage for section headers
  char section_sig[SIG_SIZE+1] = {0, };
  ModuleLoaderState lstate;

  ByteSize all_sz(bsr.read_big_u32());
  bsr.assert_have(all_sz);

  // Read another section, and switch based on its value
  while (bsr.have(ByteSize(SIG_SIZE + 4))) {
    // Section header 2 characters and Size:32/big
    bsr.read<char>(section_sig, SIG_SIZE);
    ByteSize section_sz(bsr.read_big_u32());

    auto section_view = ByteView(bsr.pos(), section_sz.bytes());

    if (not::memcmp(section_sig, SIG_ATOMS, SIG_SIZE)) {
      //
      // Atoms table (atom[0] is module name)
      //
      this->load_atoms_section(section_view, MUTABLE lstate);
      this->name_ = lstate.get_atom(0);

    } else if (not::memcmp(section_sig, SIG_CODE, SIG_SIZE)) {
      //
      // Code section
      //
      auto c_data = platf::ArrayAlloc::alloc<uint8_t>(section_sz.bytes());
      code_.reset(c_data);
      std::copy(bsr.pos(),
                bsr.pos() + section_sz.bytes(),
                c_data);

      // TODO: set up literal refs in code

    } else if (not::memcmp(section_sig, SIG_LTRL, SIG_SIZE)) {
      //
      // Literals table
      //
      E4ASSERT(env_.literals_.empty());
      E4ASSERT(env_.literal_heap_.empty());
      load_literals(section_view);

    } else if (not::memcmp(section_sig, SIG_LBLS, SIG_SIZE)) {
      load_labels(section_view, MUTABLE lstate);

    } else if (not::memcmp(section_sig, SIG_EXPT, SIG_SIZE)) {
      load_exports(section_view, lstate);

    } else if (not::memcmp(section_sig, SIG_IMPT, SIG_SIZE)) {
      load_imports(section_view, lstate);

    } else if (not::memcmp(section_sig, SIG_JMPT, SIG_SIZE)) {
      load_jump_tables(section_view, lstate);

    } else if (not::memcmp(section_sig, SIG_FUNT, SIG_SIZE)) {
//      load_fun_table(section_view);

    } else {
#if E4DEBUG
      e4::failf("e4b loader: Unknown section '%s'", section_sig);
#else
      E4FAIL("bad module: unk section");
#endif
    }

    bsr.advance(section_sz);
  }
  // TODO: set up atom refs in code
}


void Module::load_atoms_section(const e4std::BoxView<uint8_t>& section_view,
                                MUTABLE ModuleLoaderState& lstate) {
  tool::Reader bsr(section_view);
  Word count = bsr.read_big_u32();
  lstate.reserve_atoms(count);

//  this->name_ = this->vm_.add_atom(result.front());
  for (Word i = 0; i < count; ++i) {
    auto a = bsr.read_varlength_string();
    lstate.add_atom(this->vm_.add_atom(a));
  }
}


void Module::load_literals(const ByteView& adata) {
  tool::Reader bsr(adata);
  Word count = bsr.read_big_u32();
  env_.literals_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    const auto lit = ExtTerm::read_with_marker(vm_, env_.literal_heap_, bsr);
    env_.literals_.push_back(lit);
  }
}


void Module::load_exports(const ByteView& adata,
                          const ModuleLoaderState& lstate) {
  tool::Reader bsr(adata);
  Word n = bsr.read_big_u32();
  env_.exports_.reserve(n);

  for (Word i = 0; i < n; ++i) {
    auto fn_atom_index = bsr.read_varint_u();

    Arity arity { bsr.read_varint_u() };
    auto label = bsr.read_varint_u();

    // Create and insert an export, resolve label index to an offset
    Export ex(lstate.get_atom(fn_atom_index),
              arity,
              env_.get_label(label));

    ex.print(vm_);
    ::printf("\n");

    env_.exports_.push_back(ex);
  }
  // We then use binary search so better this be sorted
  std::sort(
    env_.exports_.begin(),
    env_.exports_.end(),
    [](const Export &a, const Export &b) -> bool {
      return Export::compare_less_pvoid(&a, &b);
    }
  );
}


Export* Module::find_export(const MFArity& mfa) const {
  Export sample(mfa.fun_, mfa.arity_, 0);
  auto r = const_cast<Export*>(e4std::binary_search(
    env_.exports_.begin(),
    env_.exports_.end(),
    sample,
    [](const Export &a, const Export &b) -> bool {
        return Export::compare_less_pvoid(&a, &b);
    }
  ));
  E4ASSERT(mfa.fun_.raw_equal(r->get_fun())
           && mfa.arity_ == r->get_arity());
  r->print(vm_);
  return r;
}


CodeAddress Module::get_export_address(const Export& exp) const {
  return CodeAddress(code_.get() + exp.get_offset());
}


void Module::load_imports(const ByteView &adata,
                          const ModuleLoaderState& lstate) {
  tool::Reader bsr(adata);
  Word count = bsr.read_big_u32();
  env_.imports_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    auto mod_atom_index = bsr.read_varint_u();
    auto fn_atom_index = bsr.read_varint_u();

    Arity arity { bsr.read_varint_u() };

    Import im(lstate.get_atom(mod_atom_index),
              lstate.get_atom(fn_atom_index),
              arity);
    env_.imports_.push_back(im);
  }
}


void Module::load_jump_tables(const ByteView &adata,
                              const ModuleLoaderState& lstate) {
  tool::Reader bsr(adata);
  Word count = bsr.read_big_u32();

  env_.jump_tables_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    auto jt_size = bsr.read_varint_u();
    env_.jump_tables_.emplace_back(jt_size);

    auto &jt = env_.jump_tables_.back();
    for (Word j = 0; j < jt_size; ++j) {
      const auto term = bsr.read_term();
      jt.push_back(term, bsr.read_varint_u());
    }
  }
}

void Module::load_labels(const ByteView &adata, ModuleLoaderState &lstate) {
  tool::Reader bsr(adata);
  Word count = bsr.read_big_u32();

  env_.labels_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    env_.labels_.push_back(bsr.read_varint_u());
  }
}


int Export::compare_pvoid(const void *a, const void *b) {
  auto pa = static_cast<const Export*>(a);
  auto pb = static_cast<const Export*>(b);

  if (e4std::compare_less(pa->fun_, pb->fun_)) {
    return -1;
  } else if (e4std::compare_equal(pa->fun_, pb->fun_)) {
    if (pa->arity_ < pb->arity_) {
      return -1;
    } else if (pa->arity_ == pb->arity_) {
      return 0;
    }
  }
  return 1;
}


#if E4DEBUG
void Export::print(const VM& vm) const {
  ::printf("#exp<");
  vm.print(fun_);
  ::printf("/%zu>@%zu", arity_.get<size_t>(), offset_);
}
#endif  // DEBUG

}  // ns e4
