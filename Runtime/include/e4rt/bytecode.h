// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>

#include "e4platf/types.h"

namespace e4 {

enum class Instruction: uint8_t {
  JUMP,
  GET_ELEMENT,
  ENTER
};

class CodeAddress {
  Word index_ = 0;
  // Start of code array used as the pointer base
  static Instruction* BASE_ADDR;

 public:
  CodeAddress() = default;
  explicit CodeAddress(Word offset) : index_(offset) {}
  explicit CodeAddress(const Instruction* p) : index_(p - BASE_ADDR) {}

  static Instruction* base() { return BASE_ADDR; }

  Instruction* ptr() { return BASE_ADDR + index_; }

  Word get_index() const { return index_; }

  const Instruction* ptr() const { return BASE_ADDR + index_; }

  Instruction fetch() const { return *ptr(); }

  CodeAddress& operator+=(SignedWord s) {
    index_ += s;
    return *this;
  }

  void advance() { index_++; }
};

}  // ns e4
