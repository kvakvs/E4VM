#pragma once

#include <vector>
#include <array>
#include <map>
#include <cstddef>
#include <cassert>
#include <string>
#include <memory>

namespace gluon {
    using Word = std::size_t;

    using Uint8  = std::uint8_t;
    using Uint16 = std::uint16_t;
    using Uint32 = std::uint32_t;
    using Uint64 = std::uint64_t;

    // count type
    using Count = std::size_t;

    constexpr Word WORD_SIZE = sizeof(size_t);

    // Generic size type which respects units
    template <Word UNIT_SIZE, class StorageType = Word>
    class GenericSize {
    private:
        // stores amount in units (UNIT_SIZE), multiply when bytes are requested
        StorageType units_;
    public:
        GenericSize(const GenericSize<UNIT_SIZE, StorageType> &other)
                : units_(other.units_) {
        }
        explicit GenericSize(Word n)
                : units_(n / UNIT_SIZE) {
            assert(n % UNIT_SIZE == 0);
        }

        // Returns byte size to store this count of units
        StorageType bytes() const { return units_ * UNIT_SIZE; }

        // Returns unit count as is
        StorageType units() const { return units_; }

        // Recalculates how many units of AS_UNIT_SIZE would fit into this size
        template <StorageType AS_UNIT_SIZE>
        StorageType units() const {
            return (bytes() + AS_UNIT_SIZE - 1) / AS_UNIT_SIZE;
        }
    };

    // size types for byte and word arrays
    using Bytes = GenericSize<1>;
    using Words = GenericSize<WORD_SIZE>;

    template <class TType>
    using Vector = std::vector<TType>;

    template <class TType, Word ARRAY_SIZE>
    using Array = std::array<TType, ARRAY_SIZE>;

    template <class TKey, class TValue>
    using Dict = std::map<TKey, TValue>;

    using String = std::string;

    //
    // A size-delimited data block which does not own its data.
    // Views point to read-only data.
    //
    template <class T>
    class BoxView {
    private:
        const T *data_;
        Count size_;
    public:
        BoxView(const BoxView<T> &other) = default;
        explicit BoxView(const T *data, Count s)
                : data_(data), size_(s) {
        }

        // Iterator interface
        const T *cbegin() const { return data_; }
        const T *cend() const { return cbegin() + size_; }
    };

    //
    // A data block, represented by unique_ptr and size (owns and destroys
    // the data on death)
    //
    template <class T>
    class UniqueBox {
    private:
        using DataPtr = std::unique_ptr<T[]>;
        DataPtr data_;
        Count size_;
    public:
        explicit UniqueBox(DataPtr &&data, Count s)
                : data_(std::move(data)), size_(s) {
        }
        explicit UniqueBox(T *data, Count s)
                : data_(data), size_(s) {
        }
        BoxView<T> view() const { return BoxView<T>(data_.get(), size_); }
        // Iterator interface
        const T *cbegin() const { return data_.get(); }
        const T *cend() const { return cbegin() + size_; }
    };
} // ns gluon
