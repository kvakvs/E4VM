#pragma once

#if GLUON_FEATURE_FS

#include <cstdio>
#include "gluon/types.h"

namespace gluon {
    class File {
    private:
        std::FILE *f_ = nullptr;
    public:
        explicit File(const char *fn, const char *mode) {
            f_ = std::fopen(fn, mode);
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
        UniqueBox<Uint8> read_everything() {
            std::fseek(f_, 0, SEEK_END);
            auto size = (size_t) std::ftell(f_);

            std::fseek(f_, 0, SEEK_SET);
            auto data = new Uint8[size];
            std::fread(data, size, 1, f_);
            return UniqueBox<Uint8>(data, size);
        }
    }; // class File
} // ns gluon

#endif // GLUON_FEATURE_FS
