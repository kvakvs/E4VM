// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

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
#include "e4std/vector_impl.h"

#include <functional>

namespace e4std {

static constexpr ::size_t VECTOR_MIN_GROWTH = 4;

namespace impl {

template<class V>
class Iterator {
private:
    V* ptr_;
public:
    explicit Iterator(V* v) : ptr_(v) {}

    bool operator!=(const Iterator& other) const {
        return ptr_ != other.ptr_;
    }

    Iterator& operator++() {
        ptr_++;
        return *this;
    }

    void incr() { ptr_++; }

    V& operator*() const { return *ptr_; }

    const void* as_cpvoid() const { return static_cast<const void*>(ptr_); }
};

} // ns impl

template<class ValueType>
class Vector {
private:
    ::size_t size_ = 0;
    ::size_t capacity_ = 0;
    UniqueArrayPtr<ValueType> data_;
public:
    Vector() {}

    explicit Vector(::size_t init_size) {
        resize(init_size);
    }
    Vector(Vector&& other) = default;
    Vector& operator=(Vector&& other) = default;

    ValueType* data() { return data_.get(); }

    const ValueType* data() const { return data_.get(); }

    bool empty() const { return size_ == 0; }

    // Perform linear search for value by pointer, pointer to each element
    // is passed to compare function
//    bool find(const ValueType* val, VoidpCompareFun cmp) const {
//        return ::lsearch(
//                static_cast<const void*>(val),
//                static_cast<const void*>(data_.get()),
//                size(),
//                sizeof(ValueType),
//                cmp);
//    }

    // Perform linear search for value by value, compare_equal is used
    // (passing by value)
    bool contains_val(const ValueType& val) const {
        for (auto p = cbegin(); p != cend(); p.incr()) {
            if (compare_equal(*p, val)) { return true; }
        }
        return false;
    }

    ::size_t size() const { return size_; }

    ::size_t capacity() const { return capacity_; }

    void clear() {
        resize(0);
    }

    const ValueType& operator[](::size_t i) const {
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

    // TODO: Move out of the template, generalize (vector_impl:VectorImpl)
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
    using Iterator = impl::Iterator<ValueType>;
    using ConstIterator = impl::Iterator<const ValueType>;

    Iterator begin() { return Iterator(data_.get()); }

    Iterator end() { return Iterator(data_.get() + size_); }

    ConstIterator begin() const { return cbegin(); }

    ConstIterator end() const { return cend(); }

    ConstIterator cbegin() const { return ConstIterator(data_.get()); }

    ConstIterator cend() const { return ConstIterator(data_.get() + size_); }

    //
    // end iterator stuff
    //


private:
    // Growth strategy! +25% or 4 cells, whatever is smaller
    ::size_t grow(::size_t old_capacity) {
        return std::max((old_capacity * 5) / 4, VECTOR_MIN_GROWTH);
    }

    // TODO: Move out of the template, generalize (vector_impl:change_capacity)
    // TODO: Can make free destroy/construct funs
    void change_capacity(::size_t newcap) {
        if (newcap <= capacity_) { return; }
        E4ASSERT(newcap > size_);
        auto newdata = e4std::make_array<ValueType>(newcap);
        if (size_) {
            std::move(data_.get(), data_.get() + size_, newdata.get());
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

//
// Plain old data Vector. Ignores C++ construction, zeroes memory, very generic
// Overhead 2x size_t + memory pointer
//
template <class ValueType>
class PODVector: public impl::VectorImpl {
public:
    using VectorImpl = impl::VectorImpl;

    PODVector() {}

    explicit PODVector(::size_t init_size) {
        resize(init_size);
    }

    PODVector(PODVector&& other) = default;

    PODVector& operator=(PODVector&& other) = default;

    ValueType* data() {
        return reinterpret_cast<ValueType*>(data_.get());
    }

    const ValueType* data() const {
        return reinterpret_cast<const ValueType*>(data_.get());
    }

    bool empty() const { return size_ == 0; }

    ::size_t size() const { return size_; }

    void resize(::size_t newsize) {
        VectorImpl::resize(sizeof(ValueType), newsize);
    }

    void reserve(::size_t capacity) {
        if (capacity <= capacity_) { return; }
        VectorImpl::change_capacity(sizeof(ValueType), capacity);
    }

    void push_back(const ValueType& v) {
        if (size_ >= capacity_) {
            auto newcap = VectorImpl::grow(capacity_);
            VectorImpl::change_capacity(sizeof(ValueType), newcap);
        }
        set(size_, v);
        size_++;
    }

    void set(::size_t index, const ValueType& v) {
        // ValueType* datap = reinterpret_cast<ValueType*>(data_.get());
        data()[index] = v;
    }

        ValueType& back() {
        E4ASSERT(size_ > 0);
        return *(data() + size_ - 1);
    }

    const ValueType& back() const {
        E4ASSERT(size_ > 0);
        return *(data_.get() + size_ - 1);
    }

    ValueType& front() {
        E4ASSERT(size_ > 0);
        return *data();
    }

    const ValueType& front() const {
        E4ASSERT(size_ > 0);
        return *data();
    }

    //
    // Iterator stuff
    //
    using Iterator = impl::Iterator<ValueType>;
    using ConstIterator = impl::Iterator<const ValueType>;

    Iterator begin() { return Iterator(data_.get()); }

    Iterator end() { return Iterator(data_.get() + size_); }

    ConstIterator begin() const { return cbegin(); }

    ConstIterator end() const { return cend(); }

    ConstIterator cbegin() const { return ConstIterator(data()); }

    ConstIterator cend() const { return ConstIterator(data() + size_); }

    //
    // Algorithms
    //
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

    using ForEachFun = std::function<void(ValueType&)>;
    using ForEachFunConst = std::function<void(const ValueType&)>;

    void for_each(ForEachFun fun) {
        auto last = data() + size();
        for (ValueType* p = data(); p != last; ++p) {
            fun(*p);
        }
    }

    void for_each(ForEachFunConst fun) const {
        auto last = data() + size();
        for (const ValueType* p = data(); p != last; ++p) {
            fun(*p);
        }
    }

    // Perform linear search for value by value, compare_equal is used
    // (passing by value)
    bool contains_val(const ValueType& val) const {
        for (auto p = cbegin(); p != cend(); p.incr()) {
            if (compare_equal(*p, val)) { return true; }
        }
        return false;
    }

    const ValueType& operator[](::size_t i) const {
        E4ASSERT(i < size());
        return data()[i];
    }
};

} // ns e4std
