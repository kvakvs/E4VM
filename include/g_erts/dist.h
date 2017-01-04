#pragma once

#include "g_erts/term.h"

namespace gluon {

// Erl dist node implementation
class Node {
   public:
    Term m_sysname = NIL;
    dist::Creation m_creation = dist::INTERNAL_CREATION;
};

}  // ns gluon
