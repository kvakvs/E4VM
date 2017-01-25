#pragma once

#include <cstdint>
#include "e4platf/types.h"

namespace e4 {

namespace j1_instr_tag {
typedef enum {
    JUMP       = 0,
    JUMP_COND  = 1, // jump if zero
    CALL       = 2,
    ALU        = 3,
    LITERAL    = 4,
} Type;
} // ns j1_instr_tag
using J1InstrTag = j1_instr_tag::Type;

#pragma pack(push, 2)
class alignas(2)
J1Opcode {
public:
    struct alignas(2)
    J1AsOpcode {
        J1InstrTag  instr_tag_:3;
        bool        rpc_:1;
        uint16_t  op_:4;
        bool        tn_:1;
        bool        tr_:1;
        bool        nti_:1;
        bool        unused_:1;
        uint16_t  ds_:2;
        uint16_t  rs_:2;
    };

    struct alignas(2)
    J1AsLiteral {
        bool tag_:1; // this is always true for literals
        ::int16_t val_:15;
    };

    struct alignas(2)
    J1AsJump {
        uint16_t instr_:4;  // skip instr tag 4 bits
        uint16_t addr_:12;  // jump address 12-bit
    };

    union {
        uint16_t raw_;
        J1AsOpcode op_;
        J1AsLiteral lit_;
        J1AsJump jmp_;
    };
};
static_assert(sizeof(J1Opcode) == sizeof(uint16_t), "Opcode must be 16bit");
#pragma pack(pop)

class CodeAddress {
    Word index_ = 0;
    // Start of code array used as the pointer base
    static J1Opcode* base_;

public:
    CodeAddress() = default;
    explicit CodeAddress(Word offset): index_(offset) {}
    explicit CodeAddress(const J1Opcode* p): index_(p - base_) {}

    static J1Opcode* base() { return base_; }

    J1Opcode* ptr() { return base_ + index_; }

    Word get_index() const { return index_; }

    const J1Opcode* ptr() const { return base_ + index_; }

    J1Opcode fetch() const { return *ptr(); }

    CodeAddress& operator += (SignedWord s) {
        index_ += s;
        return *this;
    }

    Word as_word() const {
        return reinterpret_cast<Word>(index_);
    }

    void advance() { index_++; }
};

// Given next value after the command, joins them together as big endian
// This is still not enough for a 32-bit address so we use implicit code
// base address to make this offset smaller
inline CodeAddress j1jump_addr(const J1Opcode jump, const J1Opcode next) {
    return CodeAddress((jump.jmp_.addr_ << 16) + next.raw_);
}

} // ns e4
