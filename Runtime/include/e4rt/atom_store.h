#pragma once

#include "e4std/map.h"
#include "e4std/vector.h"

#include "e4rt/term.h"
#include "e4rt/term_as_map_key.h"

namespace e4 {

namespace impl {
  struct CStrHasher {
    std::size_t operator()(const char* k) const {
      return std::_Hash_bytes(k, std::strlen(k), 0x13131313);
//      size_t hash = 0;
//#if E4_WORD_SIZE == 32
//      size_t seed = 0x13131313;
//#elif E4_WORD_SIZE == 64
//      size_t seed = 0x1313131313131313UL;
//#endif
//      while (*k) {
//        hash = (hash * seed) + (*k);
//        k++;
//      }
//      return hash;
    }
  };

  struct CStrEqual: public std::binary_function<const char*, const char*, bool> {
    bool operator() (const char* a, const char *b) const {
      return std::strcmp(a, b) == 0;
    }
  };
} // ns impl


// Stores short strings which never get freed in blocks which never move
class AtomStore {
private:
  constexpr static size_t BLOCK_SZ = 1024;

  Word atom_id_ = 0;

  Vector<UniquePtr<char>> blocks_;

  size_t block_remaining_ = 0;

  char* block_pos_ = nullptr;

  HashMap2<Term, const char*, impl::TermHasher> atom_to_str_;

  HashMap3<const char*, Term, impl::CStrHasher, impl::CStrEqual> str_to_atom_;

public:
  AtomStore() = default;

  size_t size() const {
    return atom_to_str_.size();
  }

  const char* find_atom(Term atom) const;

  Term find_atom(const char* s) const;  // returns NON_VALUE or an atom

  Term insert(const char* str);

#if E4DEBUG
  void debug_print();
#else
  void debug_print() {}
#endif

private:
  // Copies s to blocks_ forever (or allocates a new block)
  const char* intern(const char* s);
};


}  // ns e4
