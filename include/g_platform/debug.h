#pragma once

namespace gluon {

void fail(const char *m);
void failf(const char *format, ...);
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
    gluon::failf(BOLDYELLOW "TODO:" RESET " %s (%s:%d)\n", what,    \
          __FILE__, __LINE__);

#if GLUON_DEBUG
    // TODO: file, line, assert text format
#   define G_ASSERT(C) if (!(C)) { \
        gluon::failf(BOLDRED "ASSERT:" RESET \
            " %s:%d -- " BOLDYELLOW #C "\n" RESET, \
             __FILE__, __LINE__); \
        }
#   define G_ASSERT_GTE(A, B) if (A < B) { \
        gluon::failf(BOLDRED "ASSERT:" RESET \
            " %s:%d -- " BOLDYELLOW #A " (%zu) is not gt-eq " #B " (%zu) \n" \
            RESET, __FILE__, __LINE__, A, B); \
        }
#   define G_IF_NODEBUG(X) X
#else
#   define G_IF_NODEBUG(X)
#endif

} // ns g_erts
