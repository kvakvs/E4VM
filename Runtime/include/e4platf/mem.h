// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <stddef.h>

#include "e4platf/conf.h"
#include "e4std/stuff.h"

namespace platf {

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
