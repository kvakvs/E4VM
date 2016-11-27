#include <cstdio>
#include <stdlib.h>
#include "gluon/debug.h"
//#include <stdarg.h>

namespace gluon {

void fail(const char *m) {
    std::puts(m);
    std::abort();
}

void failf(const char *format, ...) {
    va_list args;
    va_start (args, format);
    std::vprintf(format, args);
    va_end (args);
    std::abort();
}

void dprintf(const char *format, ...) {
    va_list args;
    va_start (args, format);
    std::vprintf(format, args);
    va_end (args);
}

} // ns gluon
