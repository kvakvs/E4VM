/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include <stdint.h>

#include "e4std/vector.h"
#include "e4std/array.h"
#include "e4std/map.h"
#include "e4std/string.h"

#include "e4std/stuff.h"
#include "e4std/sizes.h"

#include "e4platf/mem.h"
#include "e4platf/debug.h"

namespace e4 {

#if E4_BITS_PER_WORD == 64
    using Word       = ::uint64_t;
    using SignedWord = ::int64_t;
    constexpr Word BYTES_PER_WORD = 8;
#elif E4_BITS_PER_WORD == 32
    using Word       = ::uint32_t;
    using SignedWord = ::int32_t;
    constexpr Word BYTES_PER_WORD = 4;
#else
    #error E4_BITS_PER_WORD other than 32/64 not supported yet
#endif
    constexpr Word BITS_PER_WORD = sizeof(Word) * 8;
    static_assert(sizeof(void*) <= sizeof(Word),
                  "A pointer type won't fit the word type");

    // Ensure that casting to word will not lose any bits
    template <typename ContainerType, typename ValueType>
    constexpr bool fits_in(ValueType i) {
        return static_cast<ValueType>(static_cast<ContainerType>(i)) == i;
    }

    using Uint8  = ::uint8_t;
    using Uint16 = ::uint16_t;
    using Uint32 = ::uint32_t;
    using Uint64 = ::uint64_t;

    // count type
    using Count = Word;
    using SignedCount = SignedWord;

    // size types for byte and word arrays
    template <typename T, typename Storage = ::size_t>
    using GenericSize = e4std::GenericSize<T, Storage>;
    using ByteSize = GenericSize<Uint8>;
    using WordSize = GenericSize<Word>; // Word is same as Term

    template <class TType>
    using Vector = e4std::Vector<TType>;

    template <class TType, Word ARRAY_SIZE>
    using Array = e4std::Array<TType, ARRAY_SIZE>;

    template <class TKey, class TValue>
    using Map = e4std::Map<TKey, TValue>;

    using String = e4std::String;
    using CString = e4std::CString;

    template <class Type>
    using UniquePtr = e4std::UniquePtr<Type>;
    template <class Type>
    using UniqueArrayPtr = e4std::UniqueArrayPtr<Type>;

} // ns e4
