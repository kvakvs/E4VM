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


Term Reader::read_compact_term(const ModuleEnv& env,
                               const ModuleLoaderState& lstate) {
  auto b = read_byte();
  auto tag = CteTag(b & 7);

  // Pre-read, in an attempt to reduce amount of calls to read_cte_word
  auto bword = tag < CteTag::Extended ? read_cte_word(b) : 0;

  switch (tag) {
    case CteTag::Literal:
      return env.get_literal(bword);

    case CteTag::Atom:
      return lstate.get_atom(bword);

    case CteTag::XReg:
      return Term::make_xreg(bword);

    case CteTag::YReg:
      return Term::make_yreg(bword);

    case CteTag::Label:
      // fallthrough
    case CteTag::Integer:
      // fallthrough
    case CteTag::Character:
      return Term::make_integer(bword);

    case CteTag::Extended:
      switch (CteExtendedTag(b)) {
        case CteExtendedTag::Float:
          return Term::make_float(read_float());

        case CteExtendedTag::List:
          break;

        case CteExtendedTag::FloatReg:
          return Term::make_fpreg(read_cte_word(read_byte()));

        case CteExtendedTag::AllocList:
          break;

        case CteExtendedTag::Literal:
          return env.get_literal(read_cte_word(read_byte()));
      }
      break;
  }

  E4FAIL("Can't parse compact term");
}

}} // ns e4::tool
