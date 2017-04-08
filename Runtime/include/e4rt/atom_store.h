#pragma once

#include "e4std/map.h"
#include "e4std/vector.h"

#include "e4rt/term.h"
#include "e4rt/term_as_map_key.h"

namespace e4 {

// Stores short strings which never get freed in blocks which never move
class AtomStore {
 private:
  constexpr static ::size_t BLOCK_SZ = 1024;

  Vector<char*> blocks_;
  ::size_t block_capacity_ = 0;
  char* block_pos_ = nullptr;

  Map<Term, const char*> atom_to_str_;
  Map<const char*, Term> str_to_atom_;

 public:
  AtomStore() = default;
  ::size_t size() const { return atom_to_str_.size(); }
  void insert(Term a, const char* str);
  const char* find(Term atom) const;
  Term find(const char* s) const;  // returns NON_VALUE or an atom
 private:
  // Copies s to blocks_ forever (or allocates a new block)
  const char* intern(const char* s);
};

}  // ns e4
