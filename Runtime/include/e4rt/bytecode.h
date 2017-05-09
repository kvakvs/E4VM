// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>
#include "e4platf/types.h"

namespace e4 {

namespace instr {
typedef enum {
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
} Type;
} // instr
using Instruction = instr::Type;


//
// A pointer to code, abstracting away memory access to be able to optimize
// this later and avoid unaligned byte reads
//
class CodeAddress {
  const uint8_t *ptr_ = nullptr;
  static const uint8_t *BASE_ADDR;

public:
  CodeAddress() = default;

  explicit CodeAddress(const uint8_t *p) : ptr_(p) {}

  static const uint8_t *get_base() { return BASE_ADDR; }

  const uint8_t *ptr() const { return ptr_; }

  uint8_t fetch() const { return *ptr_; }

  CodeAddress& operator += (Word s) {
    ptr_ += s;
    return *this;
  }

  CodeAddress& operator ++ () {
    ptr_++;
    return *this;
  }
};

}  // ns e4
