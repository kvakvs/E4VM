/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4platf/conf.h"

namespace e4 {

E4_NORETURN void fail(const char *m);
E4_NORETURN void failf(const char *format, ...);
void dprintf(const char *format, ...);

//the following are UBUNTU/LINUX ONLY terminal color codes.
#define RESET   "\033[0m"
#define BLACK   "\033[30m"      /* Black */
#define RED     "\033[31m"      /* Red */
#define GREEN   "\033[32m"      /* Green */
#define YELLOW  "\033[33m"      /* Yellow */
#define BLUE    "\033[34m"      /* Blue */
#define MAGENTA "\033[35m"      /* Magenta */
#define CYAN    "\033[36m"      /* Cyan */
#define WHITE   "\033[37m"      /* White */
#define BOLDBLACK   "\033[1m\033[30m"      /* Bold Black */
#define BOLDRED     "\033[1m\033[31m"      /* Bold Red */
#define BOLDGREEN   "\033[1m\033[32m"      /* Bold Green */
#define BOLDYELLOW  "\033[1m\033[33m"      /* Bold Yellow */
#define BOLDBLUE    "\033[1m\033[34m"      /* Bold Blue */
#define BOLDMAGENTA "\033[1m\033[35m"      /* Bold Magenta */
#define BOLDCYAN    "\033[1m\033[36m"      /* Bold Cyan */
#define BOLDWHITE   "\033[1m\033[37m"      /* Bold White */

#define G_TODO(what)                                                \
    e4::failf(BOLDYELLOW "TODO:" RESET " %s (%s:%d)\n", what,    \
          __FILE__, __LINE__);

#define G_FAIL(T) e4::failf(BOLDRED "FAIL:" RESET \
            " %s:%d -- " BOLDYELLOW #T "\n" RESET, \
             __FILE__, __LINE__);

#if E4DEBUG
    // TODO: file, line, assert text format
#   define G_ASSERT(C) if (!(C)) { \
        e4::failf(BOLDRED "ASSERT:" RESET \
            " %s:%d -- " BOLDYELLOW #C "\n" RESET, \
             __FILE__, __LINE__); \
        }
#   define G_ASSERT_GTE(A, B) if (A < B) { \
        e4::failf(BOLDRED "ASSERT:" RESET \
            " %s:%d -- " BOLDYELLOW #A " (%zu) is not gt-eq " #B " (%zu) \n" \
            RESET, __FILE__, __LINE__, A, B); \
        }
#   define G_IF_NODEBUG(X)
#else
#   define G_IF_NODEBUG(X) X
#endif

} // ns e4
