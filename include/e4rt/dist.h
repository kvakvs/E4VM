/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4rt/term.h"

namespace e4 {

// This class has size of 2 words padded with 7 bytes because creation is a byte
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpadded"

// Erl dist node implementation
class Node {
public:
    Term ml_sysname = NIL;
    dist::Creation m_creation = dist::INTERNAL_CREATION;
};

#pragma clang diagnostic pop

}  // ns e4
