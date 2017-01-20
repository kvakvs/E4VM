#pragma once

#include <string.h>

namespace e4std {

// C style C++ constructor without args: constructs a class of some type in dst
using CtorFun0 = void (*)(void* dst);
// C style C++ destructor, calls ~Class on dst
using DtorFun = void (*)(void* dst);

// Override this or use this as a function pointer in generic functions which
// have to construct different types
template <class Type>
void destruct(Type* t) {
    t->~Type();
}

// Override this or use this as a function pointer in generic functions which
// have to construct different types
template <class Type>
void construct_in(Type* t) {
    *t = Type();
}

// Override this for your beautiful key type
template <class Type>
bool compare_equal(const Type a, const Type b) {
    return a == b;
}
template<>
bool compare_equal(const char* a, const char* b);

// Override this for your beautiful key type
template <class Type>
bool compare_less(const Type a, const Type b) {
    return a < b;
}
template<>
bool compare_less(const char* a, const char* b);

// C style comparison function which returns -1 if a<b, 1 if a>b else 0
using VoidpCompareFun = int (*)(const void* a, const void* b);

} // ns e4std
