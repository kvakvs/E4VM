#pragma once

#include <cstdio>
#include "g_platform/types.h"

namespace gluon {

#if GLUON_FEATURE_FS
class File {
private:
    std::FILE *f_ = nullptr;
public:
    explicit File(const char *fn, const char *mode) {
        f_ = std::fopen(fn, mode);
        G_ASSERT(is_open());
    }
    ~File() {
        close();
    }
    bool is_open() const {
        return f_ != nullptr;
    }
    void close() {
        if (f_) {
            std::fclose(f_);
        }
        f_ = nullptr;
    }

    static UniqueBox<Uint8> read_file(const char *fn) {
        File f(fn, "rb");
        return f.read_file();
    }

    UniqueBox<Uint8> read_file() {
        std::fseek(f_, 0, SEEK_END);
        Count size = (Count) std::ftell(f_);

        std::fseek(f_, 0, SEEK_SET);
        auto data = mem::make_uniq_array<Uint8>(size);
        std::fread(data.get(), size, 1, f_);

        // Pass ownership out
        return UniqueBox<Uint8>(std::move(data), size);
    }
}; // class File
#endif // GLUON_FEATURE_FS

} // ns g_erts

