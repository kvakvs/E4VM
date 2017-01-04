#pragma once

#include <vector>
#include <array>
#include <map>
#include <cstddef>
#include <cassert>
#include <string>
#include <memory>

#include "g_platform/mem.h"
#include "g_platform/debug.h"

namespace gluon {
#if GLUON_WORD_SIZE == 64
    using Word = std::uint64_t;
    using SignedWord = std::int64_t;
    constexpr Word BYTES_PER_WORD = 8;
#elif GLUON_WORD_SIZE == 32
    using Word = std::uint32_t;
    using SignedWord = std::int32_t;
    constexpr Word BYTES_PER_WORD = 4;
#else
#error It is nice to have GLUON_WORD_SIZE other than 32/64 but not supported yet
#endif
    constexpr Word BITS_PER_WORD = sizeof(Word) * 8;

    // Ensure that casting to word will not lose any bits
    template <typename ContainerType, typename ValueType>
    constexpr bool fits_in(ValueType i) {
        return (ValueType)(ContainerType)i == i;
    }

    using Uint8  = std::uint8_t;
    using Uint16 = std::uint16_t;
    using Uint32 = std::uint32_t;
    using Uint64 = std::uint64_t;

    // count type
    using Count = Word;
    using SignedCount = SignedWord;

    // Generic size type which respects units
    template <class StoredType, class StorageType = Word>
    class GenericSize {
    private:
        static constexpr Word UNIT_SIZE = sizeof(StoredType);
        // stores amount in units (UNIT_SIZE), multiply when bytes are requested
        StorageType units_;
    public:
        GenericSize(const GenericSize<StoredType, StorageType> &other)
                : units_(other.units_) {
        }
        explicit GenericSize(Word n)
                : units_(n) {
        }

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

    // size types for byte and word arrays
    using ByteSize = GenericSize<Uint8>;
    using WordSize = GenericSize<Word>; // Word is same as Term

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
        explicit BoxView(): data_(nullptr), size_(0) {}
        explicit BoxView(const T *data, Count s)
                : data_(data), size_(s) {
        }
        bool is_empty() const { return not data_ || not size_; }
        Count size() const { return size_; }

        // Iterator interface
        const T *cbegin() const { return data_; }
        const T *cend() const { return cbegin() + size_; }
    };

    //
    // A data block, represented by unique_ptr and size (owns its data and
    // destroys the data on death)
    //
    template <class T>
    class UniqueBox {
    private:
        std::unique_ptr<T[]> data_;
        Count size_;
    public:
        explicit UniqueBox(): data_(), size_(0) {}
        explicit UniqueBox(std::unique_ptr<T[]> &&data, Count s)
                : data_(std::move(data)), size_(s) {
        }
        explicit UniqueBox(T *data, Count s)
                : data_(data), size_(s) {
        }
        BoxView<T> view() const { return BoxView<T>(data_.get(), size_); }
        bool is_empty() const { return not data_ || not size_; }
        Count size() const { return size_; }

        // Replaces data_ with a new data, copy of the view
        void copy_from(const BoxView<T> &bv) {
            G_ASSERT(not bv.is_empty()); // hope we've got something
            if (data_) { data_.release(); }
            data_ = gluon::mem::make_uniq_array<T>(bv.size());
            std::copy(bv.cbegin(), bv.cend(), data_.get());
        }

        // Iterator interface
        const T *cbegin() const { return data_.get(); }
        const T *cend() const { return cbegin() + size_; }
    };
} // ns g_erts
