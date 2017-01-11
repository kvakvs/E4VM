/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4platf/conf.h"

namespace e4 {

E4_NORETURN void fail(const char *m);
E4_NORETURN void failf(const char *format, ...);

#if E4DEBUG
    void debug_printf(const char* format, ...);
#endif // E4DEBUG

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

#if E4DEBUG
    // TODO: file, line, assert text format
    #define E4ASSERT(C) if (!(C)) {                 \
        e4::failf(BOLDRED "ASSERT:" RESET           \
            " %s:%d -- " BOLDYELLOW #C "\n" RESET,  \
             __FILE__, __LINE__);                   \
        }
    #define E4ASSERT_GTE(A, B) if (A < B) {         \
        e4::failf(BOLDRED "ASSERT:" RESET           \
            " %s:%d -- " BOLDYELLOW #A " (%zu) is not gt-eq " #B " (%zu) \n" \
            RESET, __FILE__, __LINE__, A, B);       \
        }
    #define E4IF_NODEBUG(X)

    #define E4TODO(T)                                           \
        e4::failf(BOLDYELLOW "TODO:" RESET " %s (%s:%d)\n",     \
            T, __FILE__, __LINE__);

    #define E4FAIL(T) e4::failf(BOLDRED "FAIL:" RESET           \
            " %s:%d -- " BOLDYELLOW "%s\n" RESET,               \
            T, __FILE__, __LINE__);

    // Choose a debug value in debug build
    #define E4CHOICE(DEBUG, NODEBUG) ((void)(NODEBUG), DEBUG)
    #define E4LOG(A) debug_printf(A);
    #define E4LOG1(A, V) debug_printf(A, V);
    #define E4LOG2(A, V1, V2) debug_printf(A, V1, V2);
    #define E4LOG3(A, V1, V2, V3) debug_printf(A, V1, V2, V3);
#else
    //
    // Release variants do nothing or print very compact reports
    //
    #define E4ASSERT(C) (void)(C)
    #define E4ASSERT_GTE(A, B) (void)(A), (void)(B)
    #define E4IF_NODEBUG(X) X
    #define E4TODO(T) e4::failf("TODO:%s\n", T);
    #define E4FAIL(T) e4::failf("FAIL:%s\n", T);

    // Choose a nodebug value in release
    #define E4CHOICE(DEBUG, NODEBUG) ((void)(DEBUG), NODEBUG)
    #define E4LOG(A) (void)0
    #define E4LOG1(A, V) (void)0
    #define E4LOG2(A, V1, V2) (void)0
    #define E4LOG3(A, V1, V2, V3) (void)0
#endif

} // ns e4
