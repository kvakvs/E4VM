//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include <stdio.h>
#include <stdlib.h>

#include "e4platf/conf.h"
#include "e4platf/debug.h"
#include <stdarg.h>

namespace e4 {

E4_NORETURN void fail(const char *m) {
    ::puts(m);
    ::abort();
}

E4_NORETURN void failf(const char *format, ...) {
    va_list args;
    va_start (args, format);
    ::vprintf(format, args);
    va_end (args);
    ::abort();
}

#if E4DEBUG
void debug_printf(const char* format, ...) {
    va_list args;
    va_start (args, format);
    ::vprintf(format, args);
    va_end (args);
}
#endif // E4DEBUG

} // ns e4
