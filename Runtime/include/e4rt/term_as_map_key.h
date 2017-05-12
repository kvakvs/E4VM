// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once
//
// Include this header to be able to use Term as a key for e4::Map
//

#include "e4rt/term.h"
#include "e4std/map.h"

namespace e4 {

template <>
inline bool compare_equal(const e4::Term a, const e4::Term b) {
  return a.get_raw() == b.get_raw();
}

template <>
inline bool compare_less(const e4::Term a, const e4::Term b) {
  return a.get_raw() < b.get_raw();
}

}  // ns e4
