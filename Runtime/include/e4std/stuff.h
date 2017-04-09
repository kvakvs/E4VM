// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

//
// Various algorithm and other stuff which did not deserve its own file yet
//

#include <algorithm>

namespace e4std {

//
// std::runtime_error
//
class RuntimeError {
 private:
  const char* err_ = nullptr;

 public:
  explicit RuntimeError(const char* e) : err_(e) {}

  RuntimeError(const RuntimeError&) = default;
  virtual ~RuntimeError();

  virtual const char* what() const noexcept { return err_; }
};

template<class ForwardIt, class T, class Compare>
const T* binary_search(ForwardIt first, ForwardIt last,
                       T& value, Compare fn_comp) {
  first = std::lower_bound(first, last, value, fn_comp);
  if (not (first == last) && not fn_comp(value, *first)) {
    return &(*first);
  }
  return nullptr;
}

}  // ns e4std
