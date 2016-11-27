#pragma once

#include <cstddef>
#include <utility>
#include <memory>

namespace gluon {
    namespace mem {

//template <typename T>
//T *new_array(size_t n) {
//    return new T[n];
//}

template <typename T>
std::unique_ptr<T[]> make_uniq_array(size_t n) {
    return std::make_unique<T[]>(n);
}

//template <class T, typename... Args>
//T *alloc(Args&&... args) {
//    return new T(std::forward<Args>(args)...);
//}

template <class T, typename... Args>
std::unique_ptr<T> make_uniq(Args&&... args) {
    return std::make_unique<T>(std::forward<Args>(args)...);
}

    } // ns mem
} // ns gluon
