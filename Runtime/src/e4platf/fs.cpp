// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/fs.h"
#include "e4platf/messages.h"

#if E4FEATURE_FS
#include <sys/stat.h>
#endif

namespace platf { namespace fs {

IMPL_EXCEPTION(Filesystem)

Vector<uint8_t> read(const Vector<String>& search_paths, const char* fn) {
#if E4FEATURE_FS
    for (auto &path: search_paths) {
        auto try_path = path + "/" + fn;
        if (exists(try_path)) {
            return File::read_file(try_path.c_str());
        }
    }
    E4FAIL(platferr::fs_notfound);
#else
    // TODO: For data in const memory return a boxview instead?
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

Vector<uint8_t> File::read_file() {
    ::fseek(f_, 0, SEEK_END);
    Count size = static_cast<Count>(::ftell(f_));

    Vector<uint8_t> result(size);

    ::fseek(f_, 0, SEEK_SET);
    auto sz_read = ::fread(result.data(), 1, size, f_);
    if (sz_read != size * 1) {
        E4FAIL("read err");
    }
    return result;
}

Vector<uint8_t> File::read_file(const char* fn) {
    File f(fn, "rb");
    return f.read_file();
}

}} // ns platf::fs
