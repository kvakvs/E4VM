// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/conf.h"
#include "e4std/stuff.h"
#include <cstdint>
#include <cstring>
#include <stddef.h>

namespace platf {

// ARM compilers have this keyword to specify unaligned memory pointer
#if E4_ARM
  #define E4PACKED __packed
#else
  #define E4PACKED
#endif

// portable unaligned memory read
template <typename T, typename U>
inline T unaligned_read(const U* src) {
  E4PACKED const T* src2 = (const T *)src;
  return *src2;
}

//
// Endian Swap helpers
//

#if E4_BIG_ENDIAN
  constexpr uint16_t big_to_native(uint16_t x) { return x; }
  constexpr uint32_t big_to_native(uint32_t x) { return x; }
  constexpr uint64_t big_to_native(uint64_t x) { return x; }
#else
  // GCC 4.3+ builtins
  constexpr uint16_t big_to_native(uint16_t x) { return __builtin_bswap16(x); }
  constexpr uint32_t big_to_native(uint32_t x) { return __builtin_bswap32(x); }
  constexpr uint64_t big_to_native(uint64_t x) { return __builtin_bswap64(x); }
#endif

//
// Allocates a single object on general heap
//
struct SingleAlloc {
  template <class Type, class... Args>
  E4_NODISCARD static Type* alloc_class(Args&&... args) {
    return new Type(std::forward<Args>(args)...);
  }

  template <class T>
  static void free(T* p) {
    if (p) {
      delete p;
    }
  }
};

//
// Allocates array on general heap
//
struct ArrayAlloc {
  template <class T>
  E4_NODISCARD static T* alloc(::size_t sz) {
    return new T[sz];
  }

  template <class T>
  static void free(T* p) {
    if (p) {
      delete[] p;
    }
  }
};

}  // ns platf
