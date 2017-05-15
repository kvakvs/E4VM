// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>
#include "e4platf/types.h"

namespace e4 {

namespace instr {
typedef enum {
  FuncInfo = 0,     // 0
  CallLocal,
  CallExt,
  Bif,
  Alloc,
  GetElement,       // 5
  Move,
  CallFun,
  SetNil,
  TestHeap,
  PutTuple,         // 10
  Put,
  Ret0,
  RetN,
  SelectVal,
  Cons,             // 15
  Jump,
  Trim,
  MakeFun,
  SetElement,
  ClearStack        // 20 -- use setnil instead?
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
