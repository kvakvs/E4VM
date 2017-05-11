// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/types.h"

namespace e4 {

// primary tag bits
constexpr Word PRIMARY_TAG_BITS = 2;
constexpr Word PRIMARY_VALUE_BITS = BITS_PER_WORD - PRIMARY_TAG_BITS;
constexpr Word PRIMARY_VALUE_MASK = (~0ULL) << PRIMARY_TAG_BITS;

enum class PrimaryTag : Word {
  Header = 0,
  Cons = 1,
  Boxed = 2,
  Immediate = 3,
};  // ns primary_tag

enum class Immed1Tag {
  Pid = 0,
  Port = 1,
  Immed2 = 2,
  Small = 3
};

enum class Immed2Tag {
  Atom = 0,
  Catch = 1,
  Immed3 = 2,
  Special = 3
};

enum class Immed3Tag {
  XReg = 0,
  YReg = 1,
  _Unused = 2,
  FloatReg = 3
};

constexpr Word IMM1_TAG_BITS = 2;
constexpr Word IMM1_VALUE_BITS = PRIMARY_VALUE_BITS - IMM1_TAG_BITS;
constexpr Word IMM1_MAX_VALUE = (1UL << IMM1_VALUE_BITS) - 1;

constexpr Word IMM2_TAG_BITS = 2;
constexpr Word IMM2_VALUE_BITS = IMM1_VALUE_BITS - IMM2_TAG_BITS;
constexpr Word IMM2_MAX_VALUE = (1UL << IMM2_VALUE_BITS) - 1;

constexpr Word IMM3_TAG_BITS = 2;
constexpr Word IMM3_VALUE_BITS = IMM2_VALUE_BITS - IMM3_TAG_BITS;
constexpr Word IMM3_MAX_VALUE = (1UL << IMM3_VALUE_BITS) - 1;

static constexpr Word BOXED_TAG_BITS = 4;
static constexpr Word BOXED_VALUE_BITS = PRIMARY_VALUE_BITS - BOXED_TAG_BITS;

}  // ns e4
