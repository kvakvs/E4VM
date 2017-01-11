/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once
//
// A custom, badly written replacement for C++ Vector relying on E4VM
// platform memory allocation tools
//
#include "e4std/ptr.h"
#include "e4platf/debug.h"

#include <string.h>

namespace e4std {

    static constexpr ::size_t VECTOR_MIN_GROWTH = 4;

template<class ValueType>
class Vector {
private:
    ::size_t size_ = 0;
    ::size_t capacity_ = 0;
    UniqueArrayPtr<ValueType> data_;
public:
    Vector() {}

    Vector(Vector&& other) = default;
    Vector& operator=(Vector&& other) = default;

    ValueType* data() { return data_.get(); }

    const ValueType* data() const { return data_.get(); }

    ::size_t size() const { return size_; }

    ::size_t capacity() const { return capacity_; }

    void clear() {
        resize(0);
    }

    void push_back(const ValueType& v) {
        if (size_ >= capacity_) {
            auto newcap = grow(capacity_);
            change_capacity(newcap);
        }
        (data_.get())[size_] = v;
        size_++;
    }

    void reserve(::size_t capacity) {
        if (capacity <= capacity_) { return; }
        change_capacity(capacity);
    }

    void resize(::size_t newlength) {
        // TODO: call dtors on elements?
        if (newlength <= capacity_) { // avoid realloc just shrink or grow
            size_ = newlength;

            auto datap = data_.get();
            for (auto i = size_; i < newlength; ++i) {
                datap[i].~ValueType();
                datap[i] = ValueType();
            }
            return;
        }
        E4ASSERT(newlength > 0);
        auto newdata = e4std::make_array<ValueType>(newlength);
        if (data_) { // there may possibly be empty src data
            E4ASSERT(newlength >= size_);
            ::memmove(newdata.get(), data_.get(), size_);
        }
        capacity_ = size_ = newlength;
        data_.take_over(newdata);
    }

    ValueType& back() {
        E4ASSERT(size_ > 0);
        return *(data_.get() + size_ - 1);
    }

    const ValueType& back() const {
        E4ASSERT(size_ > 0);
        return *(data_.get() + size_ - 1);
    }

    ValueType& front() {
        E4ASSERT(size_ > 0);
        return *(data_.get());
    }

    const ValueType& front() const {
        E4ASSERT(size_ > 0);
        return *(data_.get());
    }

    using ForwardIterator = ValueType*;

    ForwardIterator begin() { return data_.get(); }

    ForwardIterator end() { return data_.get() + size_; }

private:
    // Growth strategy! +25% or 4 cells, whatever is smaller
    ::size_t grow(::size_t old_capacity) {
        return e4std::max((old_capacity * 5) / 4, VECTOR_MIN_GROWTH);
    }

    void change_capacity(::size_t newcap) {
        if (newcap <= capacity_) { return; }
        E4ASSERT(newcap > size_);
        auto newdata = e4std::make_array<ValueType>(newcap);
        if (size_) {
            e4std::move_objects<ValueType>(data_.get(),
                                         data_.get() + size_,
                                         newdata.get());
        }

        // Fill the new cells with default values
        auto newdatap = newdata.get();
        for (auto i = capacity_; i < newcap; ++i) {
            newdatap[i] = ValueType();
        }

        data_.take_over(newdata);
        capacity_ = newcap;
    }
};

} // ns e4std
