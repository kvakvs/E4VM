#include "e4platf/fs.h"
//#include "e4platf/conf.h"

#include <sys/stat.h>

namespace platf { namespace fs {

IMPL_EXCEPTION(Filesystem)

Vector<Uint8> read(const Vector<String>& search_paths, const char* fn) {
#if E4FEATURE_FS
    for (auto &path: search_paths) {
        auto try_path = path + "/" + fn;
        if (exists(try_path)) {
            return File::read_file(try_path.c_str());
        }
    }
    throw FilesystemError("not found");
#else
    E4TODO("Registry of statically linked files");
#endif
}

bool exists(String& path) {
#if E4FEATURE_FS
    struct ::stat buffer;
    return (::stat(path.c_str(), &buffer) == 0);
#else
    E4TODO("Check static file storage");
#endif
}

}} // ns platf::fs
