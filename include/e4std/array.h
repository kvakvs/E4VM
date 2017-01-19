/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once
//
// A custom, badly written replacement for C++ Array
//

namespace e4std {

template <class ValueType, unsigned int size>
class Array {

};

// A non-owning pointer with size (element count)
template <class ValueType>
class ArrayRef {
    ValueType* ptr_;
    ::size_t count_;
public:
    ArrayRef() = default;
    ArrayRef(ValueType* p, ::size_t count): ptr_(p), count_(count) {}
};

} // ns e4std
