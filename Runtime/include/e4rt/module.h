// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/debug.h"
#include "e4platf/types.h"

#include "e4rt/bytecode.h"
#include "e4rt/heap.h"
#include "e4rt/module.h"
#include "e4rt/term.h"

#include "e4std/view.h"

namespace e4 {

class VM;
//using e4::ByteView;

// Element in exports table. Used to find functions referred by {M,F,Arity}
// from the outside
class Export {
private:
  Term fun_;

  Arity arity_;

  Word offset_;  // where the code begins

public:
  explicit Export() : Export(NON_VALUE, Arity {0}, 0) {}

  Export(Term f, Arity a, Word offs)
          : fun_(f), arity_(a), offset_(offs) {
  }

  const Term &get_fun() const {
    return fun_;
  }

  Word get_offset() const {
    return offset_;
  }

  const Arity &get_arity() const {
    return arity_;
  }

  // Compares two exports as void* vs void*, returns -1 if a<b, 1 if a>b, or 0
  static int compare_pvoid(const void *a, const void *b);

  static bool compare_less_pvoid(const void *a, const void *b) {
    return compare_pvoid(a, b) < 0;
  }

  bool operator == (const Export& other) const {
    return fun_.raw_equal(other.fun_) && arity_ == other.arity_;
  }

#if E4DEBUG
  void print(const VM &vm) const;
#else
  void print(const VM &vm) const {}
#endif  // DEBUG
};


class Import {
private:
  Term mod_;
  Term fun_;
  Arity arity_;

public:
  const Term &get_mod() const {
    return mod_;
  }

  const Term &get_fun() const {
    return fun_;
  }

  const Arity &get_arity() const {
    return arity_;
  }

  Import(Term m, Term f, Arity a)
    : mod_(m), fun_(f), arity_(a) {
  }
};


// A list of pairs [value | label] to use in select_val jumps
class JumpTable {
private:
  using Pair = std::tuple<Term, Word>;
  Vector<Pair> pairs_;

public:
  JumpTable(Word capacity) {
    pairs_.reserve(capacity);
  }

  void push_back(Term t, Word label) {
    pairs_.emplace_back(t, label);
  }
};


// Loader state to pass as argument to those who need it
class ModuleLoaderState {
private:
  Word atom_i_ = 0;
  UniquePtr<Term> atoms_; // atoms table from the module file, for lookups

public:
  void reserve_atoms(size_t count) {
    atoms_ = platf::SystemAllocator::alloc_raw<Term>(count);
  }

  void add_atom(Term a) {
    auto aptr = atoms_.get();
    aptr[atom_i_++] = a;
  }

  Term get_atom(size_t i) const {
    E4ASSERT(atom_i_ > i);
    auto aptr = atoms_.get();
    return aptr[i];
  }
};

constexpr Word BAD_LABEL = ~0UL;

// Captures things private for a module, groups them for convenient passing
// to those who might need it
class ModuleEnv {
protected:
  friend class Module;

  Word literals_count_ = 0;
  UniquePtr<Term> literals_;

  Heap literal_heap_;

  Word exports_count_ = 0;
  UniquePtr<Export> exports_;

  Word imports_count_ = 0;
  UniquePtr<Import> imports_;

  Vector<JumpTable> jump_tables_;

  PODVector<Word> labels_;

public:
  explicit ModuleEnv(): literal_heap_(64) {
  }

  const Word get_label(size_t i) const {
    if (not i) {
      return BAD_LABEL;
    }
    return labels_[i - 1];
  }

  const Term &get_literal(size_t i) const {
    return literals_.get()[i];
  }

  const Export &get_export(size_t i) const {
    return exports_.get()[i];
  }

  const Import &get_import(size_t i) const {
    return imports_.get()[i];
  }
};


class Module {
private:
  Term name_ = NON_VALUE;  // atom name

  UniqueArrayPtr<uint8_t> code_;

  ModuleEnv env_;

  VM& vm_;

public:
  explicit Module(VM& vm) : env_(), vm_(vm) {}

  void load(const BoxView<uint8_t>& data);

  Term name() const {
    return name_;
  }

  Export* find_export(const MFArity& mfa) const;

  // Adds code start to export offset
  CodeAddress get_export_address(const Export& exp) const;

private:
  void load_literals(const BoxView<uint8_t>& adata);

  void load_exports(const BoxView<uint8_t>& adata,
                    const ModuleLoaderState& lstate);

  void load_imports(const BoxView<uint8_t>& adata,
                    const ModuleLoaderState& lstate);

  void load_atoms_section(const BoxView<uint8_t>& section_view,
                          MUTABLE ModuleLoaderState& lstate);

  void load_labels(const BoxView<uint8_t>& adata,
                   MUTABLE ModuleLoaderState& lstate);

  void load_jump_tables(const BoxView<uint8_t>& adata,
                        const ModuleLoaderState& lstate);
};

}  // ns e4
