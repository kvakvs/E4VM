// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include <stdio.h>

#include "e4platf/byte_stream_reader.h"

#include "e4rt/ext_term_format.h"
#include "e4rt/module.h"
#include "e4rt/vm.h"

namespace e4 {

constexpr Word SIG_SIZE = 2;  // module and section signature length
constexpr const char* SIG_MODULE = "E4";  // Erl-Forth J1Forth Flavour

constexpr const char* SIG_ATOMS = "At";  // atoms section tag
constexpr const char* SIG_CODE = "Co";   // code section tag
constexpr const char* SIG_LTRL = "Lt";   // literals section tag
constexpr const char* SIG_EXPT = "Xp";   // exports section tag
constexpr const char* SIG_IMPT = "Im";   // imports section tag
constexpr const char* SIG_JMPT = "Jt";   // jump table tag
constexpr const char* SIG_FUNT = "Fn";   // function table tag


void Module::load(const ByteView& data) {
  tool::Reader bsr(data);

  // Read header E4
  bsr.assert_and_advance(SIG_MODULE, ByteSize(SIG_SIZE));

  ByteSize all_sz(bsr.read_big_u32());
  bsr.assert_have(all_sz);

  // Storage for section headers
  char section_sig[SIG_SIZE+1] = {0, };
  Vector<Term> atoms_tab;  // after loaded, will be used in exports

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
      this->load_atoms_section(/*out*/atoms_tab, section_view);
      this->name_ = atoms_tab.front();

    } else if (not::memcmp(section_sig, SIG_CODE, SIG_SIZE)) {
      //
      // Code
      //
      code_.resize(section_sz.bytes());
      std::copy(bsr.pos(), bsr.pos() + section_sz.bytes(), code_.data());
      // ::memcpy(code_.data(), bsr.pos(), section_sz.bytes());

      // TODO: set up literal refs in code

    } else if (not::memcmp(section_sig, SIG_LTRL, SIG_SIZE)) {
      //
      // Literals table
      //
      E4ASSERT(env_.literals_.empty());
      E4ASSERT(env_.literal_heap_.empty());
      load_literals(section_view);

    } else if (not::memcmp(section_sig, SIG_EXPT, SIG_SIZE)) {
      load_exports(section_view, atoms_tab);

    } else if (not::memcmp(section_sig, SIG_IMPT, SIG_SIZE)) {
      load_imports(section_view, atoms_tab);

    } else if (not::memcmp(section_sig, SIG_JMPT, SIG_SIZE)) {
      load_jump_tables(section_view);

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


void Module::load_atoms_section(Vector <e4::Term>& atoms_t,
                                const e4std::BoxView<uint8_t>& section_view) {
  tool::Reader bsr(section_view);
  Word count = bsr.read_varint_u();

//  this->name_ = this->vm_.add_atom(result.front());
  for (Word i = 0; i < count; ++i) {
    auto a = bsr.read_varlength_string();
    atoms_t.push_back(this->vm_.add_atom(a));
  }
}


void Module::load_literals(const ByteView& adata) {
  tool::Reader bsr(adata);
  Word count = bsr.read_varint_u();
  env_.literals_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    const auto lit = ExtTerm::read_with_marker(vm_, env_.literal_heap_, bsr);
    env_.literals_.push_back(lit);
  }
}


void Module::load_exports(const ByteView& adata,
                          const Vector<Term>& atoms_lookup) {
  tool::Reader bsr(adata);
  Word n = bsr.read_varint_u();
  env_.exports_.reserve(n);

  for (Word i = 0; i < n; ++i) {
    auto fn_atom_index = bsr.read_varint_u();

    E4ASSERT(atoms_lookup.size() > fn_atom_index);
    Arity arity { bsr.read_varint_u() };
    auto offset = bsr.read_varint_u();

    Export ex(atoms_lookup[fn_atom_index], arity, offset);
    env_.exports_.push_back(ex);
  }
  // We then use binary search so better this be sorted
  std::sort(
    env_.exports_.begin(),
    env_.exports_.end(),
    [](const Export &a, const Export &b) -> bool {
      return Export::compare_pvoid(&a, &b) < 0;
    }
  );
}


Export* Module::find_export(const MFArity& mfa) const {
  Export sample(mfa.fun_, mfa.arity_, 0);
  auto r = e4std::binary_search(
    env_.exports_.begin(),
    env_.exports_.end(),
    sample,
    [](const Export &a, const Export &b) -> bool {
      return Export::compare_pvoid(&a, &b) == 0;
    }
  );
  return const_cast<Export*>(r);
}


CodeAddress Module::get_export_address(const Export& exp) const {
  return CodeAddress(code_.data() + exp.get_offset());
}


void Module::load_imports(const ByteView &adata,
                          const Vector<Term> &atoms_lookup) {
  tool::Reader bsr(adata);
  Word count = bsr.read_varint_u();
  env_.imports_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    auto mod_atom_index = bsr.read_varint_u();
    E4ASSERT(atoms_lookup.size() > mod_atom_index);

    auto fn_atom_index = bsr.read_varint_u();
    E4ASSERT(atoms_lookup.size() > fn_atom_index);

    Arity arity { bsr.read_varint_u() };

    Import im(atoms_lookup[mod_atom_index],
              atoms_lookup[fn_atom_index],
              arity);
    env_.imports_.push_back(im);
  }
}


void Module::load_jump_tables(const ByteView &adata) {
  tool::Reader bsr(adata);
  Word count = bsr.read_varint_u();

  env_.jump_tables_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    auto jt_size = bsr.read_varint_u();
    env_.jump_tables_.emplace_back(jt_size);

    auto &jt = env_.jump_tables_.back();
    for (Word j = 0; j < jt_size; ++j) {
      const auto term = bsr.read_compact_term(env_);
      jt.push_back(term,
                   bsr.read_varint_u());
    }
  }
}


int Export::compare_pvoid(const void* a, const void* b) {
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
  vm.print(fun_);
  ::printf("/%zu", arity_.get<size_t>());
}
#endif  // DEBUG

}  // ns e4
