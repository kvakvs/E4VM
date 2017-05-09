// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/byte_stream_reader.h"
#include "e4rt/module.h"
//#include "e4rt/term.h"

namespace e4 { namespace tool {

// Compact term encoding: 3 bit tag
enum class CteTag: uint8_t {
  Literal   = 0b000,
  Integer   = 0b001,
  Atom      = 0b010,
  XReg      = 0b011,
  YReg      = 0b100,
  Label     = 0b101,
  Character = 0b110,
  Extended  = 0b111
};

// Compact term encoding: Extended tags, if 3 bit tag was 0b111=Extended
enum class CteExtendedTag: uint8_t {
  Float     = 0b0001'0111,
  List      = 0b0010'0111,
  FloatReg  = 0b0011'0111,
  AllocList = 0b0100'0111,
  Literal   = 0b0101'0111,
};


e4::Term Reader::read_compact_term(ModuleEnv& env) {
  auto b = read_byte();
  switch (CteTag(b & 7)) {
    case CteTag::Literal:
      return env.get_literal(read_cte_word(b));

    case CteTag::Integer:
      break;

    case CteTag::Atom:
      break;

    case CteTag::XReg:
      break;

    case CteTag::YReg:
      break;

    case CteTag::Label:
      break;

    case CteTag::Character:
      break;

    case CteTag::Extended:
      switch (CteExtendedTag(b)) {
        case CteExtendedTag::Float:
          break;

        case CteExtendedTag::List:
          break;

        case CteExtendedTag::FloatReg:
          break;

        case CteExtendedTag::AllocList:
          break;

        case CteExtendedTag::Literal:
          break;
      }
      break;
  }

  E4FAIL("Can't parse compact term");
}

}} // ns e4::tool
