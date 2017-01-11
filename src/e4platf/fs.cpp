#include "e4platf/fs.h"

namespace platf { namespace fs {

IMPL_EXCEPTION(Filesystem)

Vector<Uint8> read(const Vector<String>& search_paths, const char* fn) {
#if E4FEATURE_FS
    for (auto path: search_paths) {

    }
    throw FilesystemError("not found");
#else
    E4TODO("Without FS feature a registry of statically compiled files would "
           "be useful.")
#endif
}

}} // ns platf::fs