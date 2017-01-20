#pragma once

#include <stdint.h>

namespace e4 {

namespace j1_instr_tag {
typedef enum {
    JUMP       = 0,
    JUMP_COND  = 1,
    CALL       = 2,
    ALU        = 3,
    LITERAL    = 8,
} Type;
} // ns j1_instr_tag
using J1InstrTag = j1_instr_tag::Type;

#pragma pack(push, 2)
class alignas(2)
J1Opcode {
public:
    struct alignas(2)
    J1OpcodeBits {
        J1InstrTag  instr_tag_:3;
        bool        rpc_:1;
        ::uint16_t  op_:4;
        bool        tn_:1;
        bool        tr_:1;
        bool        nti_:1;
        bool        unused_:1;
        ::uint16_t  ds_:2;
        ::uint16_t  rs_:2;
    };

    union {
        ::uint16_t raw_;
        J1OpcodeBits op_;
    };
};
static_assert(sizeof(J1Opcode) == sizeof(::uint16_t), "Opcode must be 16bit");
#pragma pack(pop)

class CodeAddress {
public:
    const J1Opcode* ptr_ = nullptr;
public:
    CodeAddress() = default;
    explicit CodeAddress(const J1Opcode* p): ptr_(p) {}

    J1Opcode fetch() const { return *ptr_; }

    void advance() { ptr_++; }
};

} // ns e4
