// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once
//
// A custom, badly written replacement for C++ Array
//

#include <cstddef>

namespace e4std {

template <class ValueType, unsigned int size>
class Array {};

// A non-owning pointer with size (element count)
template <class ValueType>
class ArrayRef {
 private:
  ValueType* ptr_;
  ::size_t count_;

 public:
  ArrayRef() = default;
  ArrayRef(ValueType* p, ::size_t count) : ptr_(p), count_(count) {}

  ::size_t count() const { return count_; }
  const ValueType* first() const { return ptr_; }
  ValueType* first() { return ptr_; }
};

}  // ns e4std
