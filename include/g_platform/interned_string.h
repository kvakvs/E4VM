#pragma once

#include <cstring>

namespace gluon {

//
// A C string which can compare itself with other C strings
//
class CString {
private:
    const char *s_;
public:
    CString(): s_(nullptr) {}

    CString(CString&& other) = delete;

    CString(const CString& other): s_(other.s_) {}

    explicit CString(const char *s): s_(s) {
    }

    bool operator < (const CString &other) const {
        return std::strcmp(s_, other.s_) < 0;
    }

    operator const char *() const { return s_; }

    CString& operator=(const CString &other) {
        s_ = other.s_;
        return *this;
    }
};

} // ns g_erts
