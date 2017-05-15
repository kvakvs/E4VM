// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>
#include "e4platf/types.h"

namespace e4 {

namespace instr {
typedef enum {
  FuncInfo = 0x01,
  Label = 0x02,     // removed at load time
  LineInfo = 0x03,  // removed at load time; UNUSED

  CallLocal = 0x10,
  CallLocalTail = 0x11,
  CallExt = 0x12,
  CallExtTail = 0x13,
  CallBif = 0x14,
  CallBifGc = 0x15,
  Ret0 = 0x16,
  RetN = 0x17,
  Jump = 0x18,
  SelectVal = 0x19,
  CallFun = 0x1A,

  Alloc = 0x20,
  GetElement = 0x21,
  Move = 0x22,
  SetNil = 0x23,
  PutTuple = 0x24,
  Put = 0x25,
  Cons = 0x26,
  Trim = 0x27,
  MakeFun = 0x28,
  SetElement = 0x29,
  ClearStack = 0x2A, // use setnil?
  TestHeap = 0x2B,
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

  static const uint8_t *get_base() {
    return BASE_ADDR;
  }

  const uint8_t *ptr() const {
    return ptr_;
  }

  uint8_t fetch() const {
    return *ptr_;
  }

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
