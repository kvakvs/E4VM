// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <cstdint>

namespace e4std {

// Generic size type which respects units
template <class StoredType, class StorageType = ::size_t>
class GenericSize {
 private:
  static constexpr ::size_t UNIT_SIZE = sizeof(StoredType);
  // stores amount in units (UNIT_SIZE), multiply when bytes are requested
  StorageType units_;

 public:
  GenericSize(const GenericSize<StoredType, StorageType>& other)
    : units_(other.units_) {}

  explicit GenericSize(::size_t n) : units_(n) {}

  // Returns byte size to store this count of units
  StorageType bytes() const { return units_ * UNIT_SIZE; }

  // Returns unit count as is
  StorageType units() const { return units_; }

  // Recalculates how many units of AS_UNIT_SIZE would fit into this size
  template <typename OtherType>
  StorageType as_units() const {
    return (bytes() + sizeof(OtherType) - 1) / sizeof(OtherType);
  }
};

}  // ns e4std
