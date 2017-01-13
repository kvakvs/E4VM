//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#pragma once

#include "e4platf/types.h"
#include "e4std/vector.h"

namespace e4std {

using e4::Count;

//
// A size-delimited data block which does not own its data.
// Views point to read-only data.
//
template <class T>
class BoxView {
private:
    const T *data_;
    Count size_;
public:
    BoxView(const BoxView<T> &other) = default;
    explicit BoxView(): data_(nullptr), size_(0) {}
    explicit BoxView(const T *data, Count s)
            : data_(data), size_(s) {
    }
    explicit BoxView(const Vector<T>& vec)
            : data_(vec.data()), size_(vec.size()) {
    }
    bool is_empty() const { return not data_ || not size_; }
    Count size() const { return size_; }

    // Iterator interface
    const T *cbegin() const { return data_; }
    const T *cend() const { return cbegin() + size_; }
};

using ByteView = BoxView<::uint8_t>;

} // ns e4std
