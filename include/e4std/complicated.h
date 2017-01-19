//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//
#pragma once

namespace e4std {

class MaybeError {
    const char* what_;
    explicit MaybeError(const char* w): what_(w) {}
public:
    static MaybeError success() { return MaybeError(nullptr); }
    static MaybeError fail(const char* w) { return MaybeError(w); }
    bool is_success() const { return what_ == nullptr; }
    bool is_fail() const { return what_ != nullptr; }
    const char* get_fail() const { return what_; }

    void assert() const {
#if E4DEBUG
        if (is_fail()) {
            E4ASSERT(what_);
            e4::failf("MaybeErr: " RED "%s" RESET "\n", what_);
        }
#endif
    }
};

template <class ValueType>
class Maybe {
    ValueType   val_;
    bool        have_value_ = false;
public:
};


} // ns e4std
