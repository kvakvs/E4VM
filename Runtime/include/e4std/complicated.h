// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

namespace e4 {

class Error {
  const char* what_ = nullptr;

  explicit Error(const char* w) : what_(w) {}

public:
  static Error success() { return Error(nullptr); }

  static Error fail(const char* w) { return Error(w); }

  bool is_success() const { return what_ == nullptr; }

  bool is_fail() const { return what_ != nullptr; }

  const char* get_fail() const { return what_; }

  void assert_success() const {
#if E4DEBUG
    if (is_fail()) {
      E4ASSERT(what_);
      e4::failf(RED "%s" RESET "\n", what_);
    }
#endif
  }
};

// template <class ValueType>
// class Maybe {
//    ValueType   val_;
//    bool        have_value_ = false;
// public:
//};

}  // ns e4
