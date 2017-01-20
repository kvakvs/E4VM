#pragma once

#include <stdint.h>

namespace e4 {

using J1Opcode = ::uint16_t;

class CodeAddress {
public:
    const J1Opcode* ptr_ = nullptr;
public:
    CodeAddress() = default;
    explicit CodeAddress(const J1Opcode* p): ptr_(p) {}
};

} // ns e4
