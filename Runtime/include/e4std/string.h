// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once
//
// A custom, badly written replacement for C++ String/C string wrapper
//
#include "e4std/ptr.h"
#include "e4std/stuff.h"
#include "e4std/vector.h"

#include "e4platf/debug.h"

#include <string.h>

namespace e4std {

using String = std::string;
#if 0
class String {
 private:
  Vector<char> content_;

 public:
  String() { clear(); }

  explicit String(const char* src) { *this = src; }

  // Move ctor
  String(String&& mv) { content_ = std::move(mv.content_); }

  void clear() {
    content_.clear();
    content_.push_back('\0');
  }

  // Move assignment
  String& operator=(String&& other) {
    content_ = std::move(other.content_);
    return *this;
  }

  // Copy assignment
  String& operator=(const String& other) {
    content_.resize(other.size() + 1);  // extra for trailing '\0'
    if (content_.data() && other.content_.data()) {
      E4ASSERT(content_.capacity() >= other.content_.size() + 1);
      ::memcpy(content_.data(), other.content_.data(), other.content_.size());
    }
    return *this;
  }

  // Copy ctor
  explicit String(const String& other) { *this = other; }

  String(const char* src, ::size_t sz) {
    content_.resize(sz + 1);
    auto datap = content_.data();
    ::memcpy(datap, src, sz);
    datap[sz] = '\0';
  }

  ::size_t size() const { return content_.size() + 1; }

  ::size_t capacity() const { return content_.capacity() - 1; }

  String& operator=(const char* src) {
    auto srclen = ::strlen(src);
    content_.resize(srclen + 1);

    auto datap = content_.data();
    ::memcpy(datap, src, srclen);
    datap[srclen] = '\0';

    return *this;
  }

  String operator+(const char* str) const {
    String tmp(*this);
    tmp.resize(size() + ::strlen(str));
    ::strcat(tmp.data(), str);
    return tmp;
  }

  const char* data() const { return content_.data(); }

  char* data() { return content_.data(); }

  void resize(::size_t sz) { return content_.resize(sz); }

  void reserve(::size_t capacity) { return content_.reserve(capacity + 1); }

  String& operator+=(char t) {
    E4ASSERT(content_.size() > 0);
    content_.resize(content_.size() - 1);  // cut the trailing zero
    content_.push_back(t);
    content_.push_back('\0');

    return *this;
  }

  const char* c_str() const { return content_.data(); }
};
#endif // 0

}  // ns e4std
