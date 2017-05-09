// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/conf.h"
#include "e4platf/debug.h"
#include "e4platf/messages.h"
#include "e4platf/types.h"
#include "e4rt/term.h"
#include "e4std/view.h"
#include <string.h>

namespace e4 {

class ModuleEnv;

namespace tool {

class Reader {
 private:
  const uint8_t* ptr_;
  const uint8_t* end_;

 public:
  explicit Reader(const e4std::BoxView<uint8_t>& data)
    : ptr_(data.cbegin()), end_(data.cend()) {}

  // Advance by 1 byte, assert its value equal to 'value'
  void assert_byte(uint8_t value) { E4ASSERT(value == read_byte()); }

  template <class T>
  void assert_and_advance(const T* content, ByteSize sz) {
    if (not have(sz)) {
      E4FAIL(platferr::r_data_exhausted);
    }
    E4ASSERT(0 == ::memcmp(content, ptr_, sz.bytes()));
    ptr_ += sz.bytes();
  }

  template <class StoredType>
  bool have(GenericSize<StoredType> sz) const {
    return end_ - ptr_ >= static_cast<SignedWord>(sz.bytes());
  }

  template <class StoredType>
  void assert_have(GenericSize<StoredType> want_have) const {
    auto have_remaining = end_ - ptr_;
    E4ASSERT_GTE(have_remaining, static_cast<SignedWord>(want_have.bytes()));
  }

  const uint8_t* pos() const { return ptr_; }

  // Looks ahead if next bytes are same as the 'sample'
  template <class StoredType>
  bool compare_ahead(const StoredType* sample, GenericSize<StoredType> sz) {
    assert_have(sz);
    return 0 == ::memcmp(sample, ptr_, sz.bytes());
  }

  template <class StoredType>
  void advance(GenericSize<StoredType> sz) {
    assert_have(sz);
    ptr_ += sz.bytes();
  }

  uint8_t read_byte() {
    assert_have(ByteSize(1));
    return *(ptr_++);
  }

  template <class T>
  void read(T* dst, Count units) {
    assert_have(ByteSize(units));
    ::memcpy(dst, ptr_, units * sizeof(T));
    advance(GenericSize<T>(units));
  }

  // Unsigned varint, word
  template <typename T = Word>
  T read_varint_u() {
    int safety_limit = sizeof(T) + 1;  // limit loop
    T result = 0;
    while (end_ > ptr_) {
      uint8_t n = read_byte();
      result <<= 7;
      result |= (n & 0x7F);
      if ((n & 0x80) == 0) {
        break;
      }

      if (safety_limit) {
        safety_limit--;
      } else {
        E4FAIL(platferr::r_varint_too_long);
      }
    }
    return result;
  }

  String read_varlength_string() {
    auto sz = ByteSize(read_varint_u<Word>());
    assert_have(sz);

    String result(reinterpret_cast<const char*>(ptr_), sz.bytes());
    //        E4LOG("read_v_str: %s\n", result.c_str());
    ptr_ += sz.bytes();
    return result;
  }

  String read_string(Word size) {
    assert_have(ByteSize(size));
    String result;
    result.reserve(size);
    for (Word i = 0; i < size; ++i) {
      result += static_cast<char>(read_byte());
    }
    return result;
  }

  Word read_big_u16() {
    Word result =
      (static_cast<Word>(ptr_[0]) << 8) | static_cast<Word>(ptr_[1]);
    ptr_ += 2;
    return result;
  }

  Word read_big_u32() {
    Word result =
      (static_cast<Word>(ptr_[0]) << 24) | (static_cast<Word>(ptr_[1]) << 16) |
      (static_cast<Word>(ptr_[2]) << 8) | static_cast<Word>(ptr_[3]);
    ptr_ += 4;
    return result;
  }

  SignedWord read_big_s(Word bytes) {
    SignedWord result = read_byte();
    if (result & 128) {
      // set all bytes above first to 0xFF
      result = static_cast<SignedWord>((~0xFFul) | static_cast<Word>(result));
    }
    for (Word i = 1; i < bytes; i++) {
      result <<= 8;
      result += read_byte();
    }
    return result;
  }

  // Parse a compacted term, using module env as a source for table lookups
  Term read_compact_term(ModuleEnv& env);

  Word read_cte_word(uint8_t b) {
    if (not (b & 0b100)) {
      // Bit 3 is 0 marks that 4 following bits contain the value
      return (Word)b >> 4;
    } else {
      // Bit 3 is 1, but...
      if (not (b & 0b1000)) {
        // Bit 4 is 0, marks that the following 3 bits (most significant) and
        // the following byte (least significant) will contain the 11-bit value
        return ((Word)b & 0b1110'0000) << 3 | (Word)read_byte();
      } else {
        // Bit 4 is 1 means that bits 5-6-7 contain amount of bytes+2 to store
        // the value
        size_t bytes = ((Word)b & 0b1110'0000 >> 5) + 2;
        if (bytes == 9) {
          // bytes=9 means upper 5 bits were set to 1, special case 0b11111xxx
          // which means that following nested tagged value encodes size,
          // followed by the bytes (Size+9)
          bytes = read_cte_word(read_byte());
        }
        return read_word(bytes);
      } // if larger than 11 bits
    } // if larger than 4 bits
  }

  Word read_word(size_t bytes) {
    Word result = 0;

    E4ASSERT(sizeof(Word) <= bytes); // must fit else have to form bigint
    // TODO: handle bigint or maybe just fail here?

    for (size_t i = 0; i < bytes; ++i) {
      result = (result << 8) | read_byte();
    }
    return result;
  }
};

}  // ns e4::tool
}  // ns e4
