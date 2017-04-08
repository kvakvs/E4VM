/* * This is an open source non-commercial project. Dear PVS-Studio, please
 * check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */
#pragma once

//
// A custom, badly written replacement for stupid vector which operates raw
// blocks of uniform size, zeroes memory and ignores C++ constructors
//
namespace e4std {
namespace impl {

static constexpr ::size_t VECTOR_MIN_GROWTH = 4;

//
// A very simple vector implementation that ignores C++ types and constructors,
// copies its cells and zeroes the memory
//
class VectorImpl {
 protected:
  ::size_t size_ = 0;
  ::size_t capacity_ = 0;
  UniqueArrayPtr<uint8_t> data_;

  void resize(::size_t element_size, ::size_t newlength) {
    if (newlength <= capacity_) {  // avoid realloc just shrink or grow
      size_ = newlength;

      auto datap = data_.get();
      ::memset(datap + size_ * element_size, 0,
               (newlength - size_) * element_size);
      return;
    }
    E4ASSERT(newlength > 0);
    auto newdata = e4std::make_array<uint8_t>(newlength * element_size);
    if (data_) {  // there may possibly be empty src data
      E4ASSERT(newlength >= size_);
      ::memmove(newdata.get(), data_.get(), size_);
    }
    capacity_ = size_ = newlength;
    data_.take_over(newdata);
  }

  void change_capacity(::size_t element_size, ::size_t newcap) {
    if (newcap <= capacity_) {
      return;
    }
    E4ASSERT(newcap > size_);
    auto newdata = e4std::make_array<uint8_t>(newcap * element_size);
    auto newdatap = newdata.get();
    if (size_) {
      auto datap = data_.get();
      ::memmove(datap, newdatap, size_ * element_size);
    }

    // Fill the new cells with default values
    ::memset(newdatap + capacity_ * element_size, 0, newcap - capacity_);
    data_.take_over(newdata);
    capacity_ = newcap;
  }

  // Growth strategy! +25% or 4 cells, whatever is smaller
  static ::size_t grow(::size_t old_capacity) {
    return std::max((old_capacity * 5) / 4, VECTOR_MIN_GROWTH);
  }
};
}
}  // ns e4std::impl
