// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/debug.h"

namespace platferr {
#define DEFERR(NAME, VAL) constexpr const char* NAME = VAL;

DEFERR(r_data_exhausted, E4CHOICE("Reader: input data exhausted", "P:R1"))
DEFERR(r_varint_too_long, E4CHOICE("Reader: varint is too long", "P:R2"))

DEFERR(fs_notfound, E4CHOICE("File not found", "ENOENT"))

#undef DEFERR
}  // ns platferr
