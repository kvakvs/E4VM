#include <cstdio>
#include <stdlib.h>
#include "gluon/debug.h"
//#include <stdarg.h>

namespace gluon {

void fail(const char *m) {
    std::puts(m);
    std::exit(1);
}

void failf(const char *format, ...) {
    va_list args;
    va_start (args, format);
    std::vprintf(format, args);
    va_end (args);
    std::exit(1);
}

} // ns gluon
