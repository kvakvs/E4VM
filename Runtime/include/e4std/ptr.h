// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/mem.h"
#include "e4platf/mem.h"
#include "e4std/stuff.h"
#include <cstdint>
#include <memory>
#include <stddef.h>

namespace e4 {

template <class T> using UniquePtr = std::unique_ptr<T>;
template <class T> using UniqueArrayPtr = std::unique_ptr<T[]>;


//
// Owns smart pointer to own memory and has a size
//
template <class Type>
class Box {
private:
  UniquePtr<Type> p_;
  size_t count_;

public:
  Box(Type* p, size_t c): p_(p), count_(c) {
  }

  Box(UniquePtr<Type> && p, size_t c): p_(std::move(p)), count_(c) {
  }

  Type* get() {
    return p_.get();
  }

  const Type* get() const {
    return p_.get();
  }

  size_t size() const {
    return count_;
  }

  // Iterator interface
  const Type *cbegin() const {
    return p_.get();
  }

  const Type *cend() const {
    return cbegin() + count_;
  }
};


//
// A size-delimited data block which does not own its data.
// Views point to read-only data.
//
template<class T>
class BoxView {
private:
  const T* data_;

  size_t size_;

public:
  BoxView(const BoxView<T>& other) = default;

  explicit BoxView() : data_(nullptr), size_(0) {}

  explicit BoxView(const T* data, size_t s)
    : data_(data), size_(s) {}

  explicit BoxView(const Box<T>& vec)
    : data_(vec.get()), size_(vec.size()) {}

  bool is_empty() const { return not data_ || not size_; }

  size_t size() const { return size_; }

  // Iterator interface
  const T* cbegin() const { return data_; }

  const T* cend() const { return cbegin() + size_; }

  static BoxView<T> view(const Box<T>& box) {
    return BoxView(box.get(), box.size());
  }
};


}  // ns e4
