// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>

#include "e4platf/types.h"

namespace e4 {

// Compared to the original J1 Forth ALU RPC flag is moved to the lower byte and
// instruction tag extended to a full 4-bit nibble. This allowed to introduce
// more 16-bit instructions and few 8-bit to optimize for shorter code size.
//
// A regular indexed instruction
// +-------------+---------------------------+
// | 15 14 13 12 | 11 10 9 8 7 6 5 4 3 2 1 0 |
// | instr_tag   | signed or unsigned int    |
// +-------------+---------------------------+
//
// ALU instruction
// +--------------+-----------+----------------------+
// | 15 14 13 12  | 11 10 9 8 |  7  6  5   4 3 2 1 0 |
// | J1INSTR::ALU | J1ALUTag  |RPC TN TR NTI -Ds -Rs |
// +--------------+-----------+----------------------+
//
// Single Byte instruction (short int literal, small ld, small st, single_byte)
// +-----------+------------------------+
// | 7  6  5 4 | 3 2 1 0                | lower nibble can contain a value or
// | instr_tag | signed or unsigned int | another 4 bits of instruction code
// +-----------+------------------------+ (when there's no argument to it)

namespace j1_instr_tag {
typedef enum {
  JUMP = 0,       // Forth jump
  JUMP_COND = 1,  // jump if zero
  CALL = 2,       // Forth call (erl-call is another instruction)
  ALU = 3,
  GET_ELEMENT = 4,                // Erlang get-element
  LD = 5,                         // LD with 12-bit signed argument
  ST = 6,                         // ST with 12-bit signed argument
  ENTER = 7,                      // ENTER with 12-bit signed argument
                                  // 8
  LITERAL_SMALL_POS_INTEGER = 9,  // 4-bit positive integer literal
  LD_SMALL = 10,                  // 1-byte signed 4-bit LD
  ST_SMALL = 11,                  // 1-byte signed 4-bit ST

  LITERAL_ATOM = 12,       // push atom using 12-bit unsigned index
  LITERAL_ARBITRARY = 13,  // push arbitrary literal using 12-bit uns index
  LITERAL_INTEGER = 14,    // push 12-bit signed integer
  SINGLE_BYTE = 15,        // nibble 0xF marks 1-byte instruction
} Type;
}  // ns j1_instr_tag
using J1InstrTag = j1_instr_tag::Type;

namespace j1_alu_tag {
typedef enum {
  Top = 0,
  Nxt = 1,
  Top_PLUS_Nxt = 2,
  Top_AND_Nxt = 3,
  Top_OR_Nxt = 4,
  Top_XOR_Nxt = 5,
  INVERT_Top = 6,
  Nxt_EQ_Top = 7,
  Nxt_LESS_Top = 8,
  Nxt_RSHIFT_Top = 9,
  Top_MINUS_1 = 10,
  Rtop = 11,
  INDEX_Top = 12,
  Nxt_LSHIFT_Top = 13,
  DEPTH = 14,
  Nxt_UNSIGNED_LESS_Top = 15,
} Type;
}  // ns j1_alu_tag
using J1ALUTag = j1_alu_tag::Type;

constexpr Word INSTR_TAG_BITS = 4;
constexpr Word INSTR_VALUE_BITS = (16 - INSTR_TAG_BITS);
constexpr Word BYTE_INSTR_VALUE_BITS = (8 - INSTR_TAG_BITS);

constexpr Word ALU_OP_BITS = 4;

#pragma pack(push, 1)
class alignas(1) J1Opcode16 {
 public:
  struct alignas(1) J1AsALU {
    J1InstrTag instr_tag_ : INSTR_TAG_BITS;
    J1ALUTag op_ : ALU_OP_BITS;
    bool rpc_ : 1;
    bool tn_ : 1;
    bool tr_ : 1;
    bool nti_ : 1;
    int ds_ : 2;
    int rs_ : 2;
  };

  // Indexed instruction with 4 bit instr code and 12 bit argument
  struct alignas(1) J1UnsignedIndexOp {
    unsigned int instr_tag_ : INSTR_TAG_BITS;
    unsigned int val_ : INSTR_VALUE_BITS;
  };
  struct alignas(1) J1SignedIndexOp {
    unsigned int instr_tag_ : INSTR_TAG_BITS;
    int val_ : INSTR_VALUE_BITS;
  };

  union {
    uint16_t raw_;
    uint8_t byte_[2];
    J1AsALU alu_;
    J1UnsignedIndexOp unsigned_;
    J1SignedIndexOp signed_;
  };

  explicit J1Opcode16(uint8_t first, uint8_t second) : byte_{first, second} {}
};
static_assert(sizeof(J1Opcode16) == sizeof(uint16_t),
              "Long opcode must fit in 16bit");

class alignas(1) J1Opcode8 {
 public:
  struct alignas(1) UnsignedOp {
    J1InstrTag instr_tag_ : INSTR_TAG_BITS;
    unsigned int val_ : BYTE_INSTR_VALUE_BITS;
  };

  struct alignas(1) SignedOp {
    J1InstrTag instr_tag_ : INSTR_TAG_BITS;
    int val_ : BYTE_INSTR_VALUE_BITS;
  };

  union {
    uint8_t raw_;
    UnsignedOp unsigned_;
    SignedOp signed_;
  };

  explicit J1Opcode8(uint8_t raw) : raw_(raw) {}
};

#pragma pack(pop)

class CodeAddress {
  Word index_ = 0;
  // Start of code array used as the pointer base
  static uint8_t* base_;

 public:
  CodeAddress() = default;
  explicit CodeAddress(Word offset) : index_(offset) {}
  explicit CodeAddress(const uint8_t* p) : index_(p - base_) {}

  static uint8_t* base() { return base_; }

  uint8_t* ptr() { return base_ + index_; }

  Word get_index() const { return index_; }

  const uint8_t* ptr() const { return base_ + index_; }

  uint8_t fetch() const { return *ptr(); }

  CodeAddress& operator+=(SignedWord s) {
    index_ += s;
    return *this;
  }

  void advance() { index_++; }
};

// Given next value after the command, joins them together as big endian
// This is still not enough for a 32-bit address so we use implicit code
// base address to make this offset smaller
// inline CodeAddress j1jump_addr(const J1Opcode16 jump, const J1Opcode16 next)
// {
//    return CodeAddress((jump.index_.addr_ << 16) + next.raw_);
//}

}  // ns e4
