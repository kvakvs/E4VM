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
using e4std::ByteView;

// Element in exports table. Used to find functions referred by {M,F,Arity}
// from the outside
class Export {
private:
  Term fun_;
  Word offset_;  // how far from the module code start, in 16bit words
  Arity arity_;

public:
  const Term &get_fun() const { return fun_; }

  Word get_offset() const { return offset_; }

  const Arity &get_arity() const { return arity_; }

  explicit Export() : Export(NON_VALUE, Arity {0}, 0) {}

  Export(Term f, Arity a, Word offs) : fun_(f), offset_(offs), arity_(a) {}

  // Compares two exports as void* vs void*, returns -1 if a<b, 1 if a>b, or 0
  static int compare_pvoid(const void *a, const void *b);

#if E4DEBUG
  void print(const VM &vm) const;
#endif  // DEBUG
};


class Import {
private:
  Term mod_;
  Term fun_;
  Arity arity_;

public:
  const Term &get_mod() const { return mod_; }

  const Term &get_fun() const { return fun_; }

  const Arity &get_arity() const { return arity_; }

  Import(Term m, Term f, Arity a) : mod_(m), fun_(f), arity_(a) {}
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
  Vector<Term> atoms_; // atoms table from the module file, for lookups

public:
  void reserve_atoms(size_t n) { atoms_.reserve(n); }

  void add_atom(Term a) { atoms_.push_back(a); }

  Term get_atom(size_t i) const {
    E4ASSERT(atoms_.size() > i);
    return atoms_[i];
  }
};


// Captures things private for a module, groups them for convenient passing
// to those who might need it
class ModuleEnv {
protected:
  friend class Module;

  PODVector<Term> literals_;
  Heap literal_heap_;
  PODVector<Export> exports_;
  PODVector<Import> imports_;
  Vector<JumpTable> jump_tables_;

public:
  explicit ModuleEnv(): literal_heap_(64) {
  }

  const Term &get_literal(size_t i) const {
    return literals_[i];
  }

  const Export &get_export(size_t i) const {
    return exports_[i];
  }

  const Import &get_import(size_t i) const {
    return imports_[i];
  }
};


class Module {
private:
  Term name_ = NON_VALUE;  // atom name

  PODVector<uint8_t> code_;

  ModuleEnv env_;

  VM &vm_;

public:
  explicit Module(VM &vm) : env_(), vm_(vm) {}

  void load(const ByteView &data);

  Term name() const {
    return name_;
  }

  Export *find_export(const MFArity &mfa) const;

  // Adds code start to export offset
  CodeAddress get_export_address(const Export &exp) const;

private:
  void load_literals(const ByteView &adata);

  void load_exports(const ByteView &adata,
                    const ModuleLoaderState& lstate);

  void load_imports(const ByteView &adata,
                    const ModuleLoaderState& lstate);

  void load_atoms_section(MUTABLE ModuleLoaderState& lstate,
                          const e4std::BoxView<uint8_t> &section_view);

  void load_jump_tables(const ByteView &adata,
                        const ModuleLoaderState& lstate);
};

}  // ns e4
