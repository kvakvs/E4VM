#include "e4rt/atom_store.h"

namespace e4 {

const char* AtomStore::find(Term atom) const {
  auto node = atom_to_str_.find(atom);
  return (node != atom_to_str_.end()) ? node->second : nullptr;
}


Term AtomStore::find(const char* s) const {
  auto node = str_to_atom_.find(s);
  return (node != str_to_atom_.end()) ? node->second : NON_VALUE;
}


void AtomStore::insert(Term a, const char* str) {
  auto interned_str = intern(str);
  atom_to_str_[a] = interned_str;
  str_to_atom_[interned_str] = a;
}


const char* AtomStore::intern(const char* s) {
  auto len = ::strlen(s) + 1;  // with zero byte
  E4ASSERT(len < 255 && len < BLOCK_SZ);
  E4ASSERT(not find(s).is_value());  // assert does not already exist

  // if last block is full (capacity cannot accomodate s)
  if (block_capacity_ < len) {
    auto mem = platf::SystemAllocator::alloc_raw<char>(BLOCK_SZ);
    block_pos_ = mem.get();
    block_capacity_ = BLOCK_SZ;
    blocks_.push_back(std::move(mem));
  }

  // now store and return stored position
  auto dst = block_pos_;
  block_capacity_ += len;
  block_pos_ += len;

  ::strcpy(dst, s);
  return dst;
}

}  // ns e4
