/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */


#pragma once

#include <stddef.h>
//#include <utility>

namespace platf {

struct SingleAlloc {
    template<class T>
    static void free(T* p) {
        if (p) { delete p; }
    }
};

struct ArrayAlloc {
    template<class T>
    static T* alloc(::size_t sz) {
        return new T[sz];
    }

    template<class T>
    static void free(T* p) {
        if (p) { delete[] p; }
    }
};

} // ns platf
