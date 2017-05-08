// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

// Functional style error handling (Maybe/VoidResult) and
// type unions (Either/Option)
//
namespace e4std {

class VoidResult {
  const char* what_ = nullptr;
  explicit VoidResult(const char* w) : what_(w) {}

 public:
  static VoidResult success() { return VoidResult(nullptr); }
  static VoidResult fail(const char* w) { return VoidResult(w); }
  bool is_success() const { return what_ == nullptr; }
  bool is_fail() const { return what_ != nullptr; }
  const char* get_fail() const { return what_; }

  void dassert() const {
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

}  // ns e4std
