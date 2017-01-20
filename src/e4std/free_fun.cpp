#include "e4std/free_fun.h"

namespace e4std {

template<>
bool compare_equal(const char* a, const char* b) {
    return ::strcmp(a, b) == 0;
}

template<>
bool compare_less(const char* a, const char* b) {
    return ::strcmp(a, b) < 0;
}

} // ns e4std
