/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include <cstring>

#include "g_platform/types.h"
#include "g_platform/conf.h"
#include "g_platform/debug.h"

namespace gluon { namespace tool {

class Reader {
private:
    const Uint8 *ptr_;
    const Uint8 *end_;
public:
    explicit Reader(const BoxView<Uint8> &data)
            : ptr_(data.cbegin()), end_(data.cend()) {
    }

    // Advance by 1 byte, assert its value equal to 'value'
    void assert_byte(Uint8 value) { G_ASSERT(value == read_byte()); }

    template <class T>
    void assert_and_advance(const T *content, ByteSize sz) {
        G_ASSERT(have(sz));
        G_ASSERT(0 == std::memcmp(content, ptr_, sz.bytes()));
        ptr_ += sz.bytes();
    }

    template <class StoredType>
    bool have(GenericSize<StoredType> sz) const {
        return end_ - ptr_ >= static_cast<SignedWord>(sz.bytes());
    }

    template <class StoredType>
    void assert_have(GenericSize<StoredType> want_have) const {
        auto have_remaining = end_ - ptr_;
        G_ASSERT_GTE(have_remaining,
                     static_cast<SignedWord>(want_have.bytes()));
    }

    const Uint8 *pos() const { return ptr_; }

    // Looks ahead if next bytes are same as the 'sample'
    template <class StoredType>
    bool compare_ahead(const StoredType *sample, GenericSize<StoredType> sz) {
        assert_have(sz);
        return 0 == std::memcmp(sample, ptr_, sz.bytes());
    }

    template <class StoredType>
    void advance(GenericSize<StoredType> sz) {
        assert_have(sz);
        ptr_ += sz.bytes();
    }

    Uint8 read_byte() {
        assert_have(ByteSize(1));
        return *(ptr_++);
    }

    template <class T>
    void read(T *dst, Count units) {
        assert_have(ByteSize(units));
        std::memcpy(dst, ptr_, units * sizeof(T));
        advance(GenericSize<T>(units));
    }

    // Unsigned varint, word
    template <typename T>
    T read_varint_u() {
        int safety_limit = sizeof(T) + 1; // limit loop
        T result = 0;
        while (end_ > ptr_) {
            Uint8 n = read_byte();
            result <<= 7;
            result |= (n & 0x7F);
            if ((n & 0x80) == 0) { break; }

            if (safety_limit) {
                safety_limit--;
            } else {
                G_FAIL("Varint too long");
            }
        }
        return result;
    }

    String read_varlength_string() {
        auto sz = ByteSize(read_varint_u<Word>());
        assert_have(sz);

        String result(reinterpret_cast<const char *>(ptr_), sz.bytes());
//        dprintf("read_v_str: %s\n", result.c_str());
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
        Word result = (static_cast<Word>(ptr_[0]) << 8)
                      | static_cast<Word>(ptr_[1]);
        ptr_ += 2;
        return result;
    }

    Word read_big_u32() {
        Word result = (static_cast<Word>(ptr_[0]) << 24)
                       | (static_cast<Word>(ptr_[1]) << 16)
                       | (static_cast<Word>(ptr_[2]) << 8)
                       | static_cast<Word>(ptr_[3]);
        ptr_ += 4;
        return result;
    }

    SignedWord read_big_s(Word bytes) {
        SignedWord result = read_byte();
        if (result & 128) {
            // set all bytes above first to 0xFF
            result = static_cast<SignedWord>((~0xFFul)
                                             | static_cast<Word>(result));
        }
        for (Word i = 1; i < bytes; i++) {
            result <<= 8;
            result += read_byte();
        }
        return result;
    }
};

    } // ns gluon::tool
} // ns gluon
