#pragma once

namespace gluon {

void fail(const char *m);
void failf(const char *format, ...);
void dprintf(const char *format, ...);

#if GLUON_DEBUG

// TODO: file, line, assert text format
#define Gluon_assert(C) if (!(C)) { \
    gluon::failf("ASSERT: %s:%d -- " #C "\n", __FILE__, __LINE__); \
    }

#endif

} // ns gluon
