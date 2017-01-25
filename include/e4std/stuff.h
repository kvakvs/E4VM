/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once
//
// Various algorithm and other stuff which did not deserve its own file yet
//

#include <algorithm>

namespace e4std {

//
// std::runtime_error
//
class RuntimeError {
private:
    const char* err_ = nullptr;
public:
    explicit RuntimeError(const char* e) : err_(e) {}

    RuntimeError(const RuntimeError&) = default;
    virtual ~RuntimeError();

    virtual const char* what() const noexcept { return err_; }
};

} // ns e4std
