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
class ModuleLoaderState;

namespace tool {

// Reads varint encoded integer where highest bit marks whether it is the
// last byte (bit 7 is 0) or there are more (bit 7 is 1).
inline const uint8_t*
read_varint(const uint8_t *ptr, MUTABLE Word& out) {
  int safety_limit = sizeof(Word) + 1;  // limit loop
  Word result = 0;
  while (safety_limit) {
    uint8_t n = *(ptr++);

    result <<= 7;
    result |= (n & 0x7F);

    if ((n & 0x80) == 0) {
      out = result;
      return ptr;
    }

    safety_limit--;
  }
  E4FAIL(platferr::r_varint_too_long);
}


// Reads 'bytes' amount of bytes as a big endian unsigned and returns the result
inline const uint8_t*
read_serialized_bytes_as_word(const uint8_t* ptr,
                              size_t bytes,
                              MUTABLE Word& result) {
  result = 0;

  E4ASSERT(sizeof(Word) <= bytes); // must fit else have to form bigint
  // TODO: handle bigint or maybe just fail here?

  for (size_t i = 0; i < bytes; ++i) {
    result = (result << 8) | *(ptr++);
  }
  return ptr;
}


// Gathers up bits from the first byte 'b' and possibly following bytes, to form
// an integer encoded with Compact Term Encoding (used in BEAM)
inline const uint8_t*
read_cte_word(const uint8_t* ptr,
              uint8_t b,
              MUTABLE Word& result) {
  if (not (b & 0b100)) {
    // Bit 3 is 0 marks that 4 following bits contain the value
    result = (Word)b >> 4;
    return ptr;

  } else {
    // Bit 3 is 1, but...
    if (not (b & 0b1000)) {
      // Bit 4 is 0, marks that the following 3 bits (most significant) and
      // the following byte (least significant) will contain the 11-bit value
      result = ((Word)b & 0b1110'0000) << 3 | (Word)*(ptr++);
      return ptr;

    } else {
      // Bit 4 is 1 means that bits 5-6-7 contain amount of bytes+2 to store
      // the value
      size_t bytes = ((Word)b & 0b1110'0000 >> 5) + 2;
      if (bytes == 9) {
        // bytes=9 means upper 5 bits were set to 1, special case 0b11111xxx
        // which means that following nested tagged value encodes size,
        // followed by the bytes (Size+9)
        auto bnext = *(ptr++);
        ptr = read_cte_word(ptr, bnext, MUTABLE bytes);
      }

      ptr = read_serialized_bytes_as_word(ptr, bytes, MUTABLE result);
      return ptr;
    } // if larger than 11 bits
  } // if larger than 4 bits
}


class Reader {
 private:
  const uint8_t* ptr_;

  const uint8_t* end_;

 public:
  explicit Reader(const BoxView<uint8_t> & data)
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


  const uint8_t* pos() const {
    return ptr_;
  }


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
    T result;
    ptr_ = read_varint(ptr_, MUTABLE result);
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


  uint16_t read_big_u16() {
    uint16_t r = platf::big_to_native(platf::unaligned_read<uint16_t>(ptr_));
    ptr_ += 2;
    return r;
  }


  uint32_t read_big_u32() {
    uint32_t r = platf::big_to_native(platf::unaligned_read<uint32_t>(ptr_));
    ptr_ += 4;
    return r;
  }


  uint64_t read_big_u64() {
    uint64_t r = platf::big_to_native(platf::unaligned_read<uint64_t>(ptr_));
    ptr_ += 8;
    return r;
  }


  Term read_term() {
#if E4_WORD_SIZE == 32
    return Term(read_big_u32());
#elif E4_WORD_SIZE == 64
    return Term(read_big_u64());
#endif
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


  Float read_float() {
    Float val;
    // protocol stores floats as 8 bytes always. In memory we might store as
    // 4 or 8 bytes
    // TODO: endian conversion for floats?
    read((char *)&val, Count(sizeof(double)));
    return val;
  }


  // Parse a compacted term, using module env as a source for table lookups
  Term read_compact_term(const ModuleEnv& env,
                         const ModuleLoaderState& lstate);
};

}  // ns e4::tool
}  // ns e4
