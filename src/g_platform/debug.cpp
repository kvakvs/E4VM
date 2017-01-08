//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include <cstdio>
#include <stdlib.h>

#include "g_platform/conf.h"
#include "g_platform/debug.h"
//#include <stdarg.h>

namespace gluon {

GLUON_NORETURN void fail(const char *m) {
    std::puts(m);
    std::abort();
}

GLUON_NORETURN void failf(const char *format, ...) {
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
