/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once
//
// A custom, badly written replacement for C++ Vector relying on E4VM
// platform memory allocation tools
//
#include "e4std/ptr.h"
#include "e4std/free_fun.h"

#include "e4platf/debug.h"

#include <string.h>
#include <stdlib.h>

namespace e4std {

static constexpr ::size_t VECTOR_MIN_GROWTH = 4;

namespace impl {

// A typeless search algorithm
//void* binary_search(const void* from, const void* to, ::size_t stride,
//                    const void* sample, VoidpCompareFun cmp);

} // ns impl

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

    bool empty() const { return size_ == 0; }

    ::size_t size() const { return size_; }

    ::size_t capacity() const { return capacity_; }

    void clear() {
        resize(0);
    }

    const ValueType& operator[] (::size_t i) const {
        E4ASSERT(i < size());
        return data_.get()[i];
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

    // TODO: Move out of the template, generalize
    // TODO: Can make free destroy/construct funs
    void resize(::size_t newlength) {
        // TODO: call dtors on elements?
        if (newlength <= capacity_) { // avoid realloc just shrink or grow
            size_ = newlength;

            auto datap = data_.get();
            for (auto i = size_; i < newlength; ++i) {
                e4std::destruct(datap + i);
                e4std::construct_in(datap + i);
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

    //
    // Iterator stuff
    //
    template <class V>
    class TIterator {
    private:
        V* ptr_;
    public:
        explicit TIterator(V* v): ptr_(v) {}
        bool operator != (const TIterator& other) {
            return ptr_ != other.ptr_;
        }
        TIterator& operator++() {
            ptr_++;
            return *this;
        }
        V& operator*() const { return *ptr_; }
        const void* as_cpvoid() const { return static_cast<const void*>(ptr_); }
    };
    using Iterator = TIterator<ValueType>;
    using ConstIterator = TIterator<const ValueType>;

    Iterator begin() { return Iterator(data_.get()); }

    Iterator end() { return Iterator(data_.get() + size_); }

    ConstIterator begin() const { return ConstIterator(data_.get()); }

    ConstIterator end() const { return ConstIterator(data_.get() + size_); }

    ValueType* binary_search(const ValueType* val, VoidpCompareFun cmp) const {
        return static_cast<ValueType*>(
                ::bsearch(static_cast<const void*>(val),
                          static_cast<const void*>(data_.get()),
                          size(),
                          sizeof(ValueType),
                          cmp)
        );
    }

    void sort(VoidpCompareFun cmp) {
        ::qsort(static_cast<void*>(data_.get()),
                size(),
                sizeof(ValueType),
                cmp);
    }

private:
    // Growth strategy! +25% or 4 cells, whatever is smaller
    ::size_t grow(::size_t old_capacity) {
        return e4std::max((old_capacity * 5) / 4, VECTOR_MIN_GROWTH);
    }

    // TODO: Move out of the template, generalize
    // TODO: Can make free destroy/construct funs
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
