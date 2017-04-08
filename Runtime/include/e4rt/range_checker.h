#pragma once

#include <cstdint>
#include <e4platf/debug.h>

namespace e4 {

class RangeChecker {
  const uint8_t* code_range_;
  const uint8_t* code_range_end_;

 public:
  explicit RangeChecker(const uint8_t* code_range,
                        const uint8_t* code_range_end)
    : code_range_(code_range), code_range_end_(code_range_end) {}
  RangeChecker(const RangeChecker& other) = default;

  bool in_range(const uint8_t* p) const {
    return p >= code_range_ && p <= code_range_end_;
  }
  void assert_in_range(const uint8_t* p) const { E4ASSERT(in_range(p)); }
};

} // ns e4
