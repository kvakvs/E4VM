#pragma once

#include <cstring>
#include "gluon/types.h"
#include "gluon/conf.h"
#include "gluon/debug.h"

namespace gluon {

class ByteStreamReader {
private:
    const Uint8 *pos_;
    const Uint8 *end_;
public:
    explicit ByteStreamReader(const BoxView<Uint8> &data)
            : pos_(data.cbegin()), end_(data.cend()) {
    }

    template <class T>
    void assert_and_skip(const T *content, Bytes sz) {
        Gluon_assert(have(sz));
        Gluon_assert(0 == std::memcmp(content, pos_, sz.bytes()));
        pos_ += sz.bytes();
    }

    template <Count UNIT>
    bool have(GenericSize<UNIT> sz) const {
        return end_ - pos_ >= sz.bytes();
    }

    const Uint8 *pos() const { return pos_; }

    // Looks ahead if next bytes are same as the 'sample'
    template <class T, Count UNIT>
    bool see_ahead(const T *sample, GenericSize<UNIT> sz) {
        Gluon_assert(have(sz));
        return 0 == std::memcmp(sample, pos_, sz.bytes());
    }

    template <Count UNIT>
    void advance(GenericSize<UNIT> sz) {
        Gluon_assert(have(sz));
        pos_ += sz.bytes();
    }

    Uint8 byte() {
        Gluon_assert(have(Bytes(1)));
        return *(pos_++);
    }

    // Unsigned varint, word
    template <typename T>
    T varint_u() {
        T result = 0;
        Uint8 n = byte();
        while (end_ > pos_) {
            result <<= 7;
            result |= (n & 127);
            if ((n & 128) == 0) { break; }
        }
        return result;
    }
};

} // ns gluon
