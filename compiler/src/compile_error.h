#pragma once

#include <stdexcept>
#include <cassert>

#define E4_ASSERT(X) assert(X)

namespace err {

class CompileError: public std::logic_error {
public:
  explicit CompileError(const std::string& w): std::logic_error(w) {}
};

} // ns err
