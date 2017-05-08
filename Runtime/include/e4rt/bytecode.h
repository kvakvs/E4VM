// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>
#include "e4platf/types.h"

namespace e4 {

namespace instr {
enum {
  FuncInfo = 0,
  CallLocal = 1,
  CallExt = 2,
  Bif = 3,
  AllocStack = 4,
  AllocStackHeap = 5,
  GetElement = 6,
  Move = 7,
  CallFun = 8,
  SetNil = 9,
  TestHeap = 10,
  PutTuple = 11,
  Put = 12,
  Ret0 = 13,
  RetN = 14,
  SelectVal = 15,
  Cons = 16,
  Jump = 17,
  Trim = 18,
  MakeFun = 19,
  SetElement = 20,
  ClearStack = 21, // setnil instead?
};
} // instr

class CodeAddress {
  Word index_ = 0;
  // Start of code array used as the pointer base
  static uint8_t* BASE_ADDR;

 public:
  CodeAddress() = default;
  explicit CodeAddress(Word offset) : index_(offset) {}
  explicit CodeAddress(const uint8_t* p) : index_(p - BASE_ADDR) {}

  static uint8_t* base() { return BASE_ADDR; }

  uint8_t* ptr() { return BASE_ADDR + index_; }

  Word get_index() const { return index_; }

  const uint8_t* ptr() const { return BASE_ADDR + index_; }

  uint8_t fetch() const { return *ptr(); }

  CodeAddress& operator+=(SignedWord s) {
    index_ += s;
    return *this;
  }

  void advance() { index_++; }
};

}  // ns e4
