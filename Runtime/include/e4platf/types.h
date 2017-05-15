// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/debug.h"
#include "e4platf/mem.h"
#include "e4std/array.h"
#include "e4std/map.h"
#include "e4std/sizes.h"
#include "e4std/string.h"
#include "e4std/stuff.h"
#include "e4std/vector.h"
#include <cstdint>
#include <string>
#include <unordered_map>

namespace e4 {

#if E4_WORD_SIZE == 64
  using Word = uint64_t;
  using SignedWord = int64_t;
  constexpr Word BYTES_PER_WORD = 8;
#elif E4_WORD_SIZE == 32
  using Word = uint32_t;
  using SignedWord = int32_t;
  constexpr Word BYTES_PER_WORD = 4;
#else
  #error E4_WORD_SIZE other than 32/64 not supported yet
#endif
constexpr Word BITS_PER_WORD = sizeof(Word) * 8;


#if E4FEATURE_FLOAT
using Float = double;
#else
using Float = Word;
#endif


// Ensure that casting to word will not lose any bits
// TODO: Make this count bits instead of runtime checking the value
template <typename ContainerType, typename ValueType>
constexpr bool fits_in(ValueType i) {
  return static_cast<ValueType>(static_cast<ContainerType>(i)) == i;
}

// count type
using Count = Word;
using SignedCount = SignedWord;

// size types for byte and word arrays
template <typename GRAN, typename STORAGE = ::size_t>
using GenericSize = T_GenericSize<GRAN, STORAGE>;

using ByteSize = T_GenericSize<uint8_t>;

using WordSize = T_GenericSize<Word>;  // Word is same as Term

template <class VALUE>
using Vector = std::vector<VALUE>;

// Stupid vector which does not bother with constructing/destructing
template <class VALUE>
using PODVector = std::vector<VALUE>;

template <class KEY, class VALUE>
using HashMap = std::unordered_map<KEY, VALUE>;

template <class KEY, class VALUE, class HASHER>
using HashMap2 = std::unordered_map<KEY, VALUE, HASHER>;

template <class KEY, class VALUE, class HASHER, class COMPARATOR>
using HashMap3 = std::unordered_map<KEY, VALUE, HASHER, COMPARATOR>;

using String = std::string;

template <class TYPE>
using UniquePtr = std::unique_ptr<TYPE>;

template <class TYPE>
using UniqueArrayPtr = std::unique_ptr<TYPE[]>;

}  // ns e4
