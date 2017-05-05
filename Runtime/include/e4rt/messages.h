// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/debug.h"

namespace e4err {
#define DEF_ERR(NAME, VAL) constexpr const char* NAME = VAL;

DEF_ERR(no_feat_bignum, E4CHOICE("Bignum feature is disabled", "F:BIG"))
DEF_ERR(no_feat_maps, E4CHOICE("Maps feature is disabled", "F:MAP"))
DEF_ERR(no_feat_float, E4CHOICE("Float feature is disabled", "F:FLO"))
DEF_ERR(no_feat_erldist,
       E4CHOICE("Erlang Distribution feature is disabled", "F:DIS"))

DEF_ERR(etf_bad_tag,
       E4CHOICE("Bad tag encountered while reading ext term", "E:!TAG"))
DEF_ERR(etf_bad_pid, E4CHOICE("Bad pid while reading ext term", "E:!PID"))
DEF_ERR(etf_atom_expected, E4CHOICE("Atom is expected", "E:ATOM?"))

DEF_ERR(mod_not_exist, E4CHOICE("Module not found", "M:NEX"))
DEF_ERR(proc_not_exist, E4CHOICE("Process pid not found", "P:NEX"))

DEF_ERR(code_undef, E4CHOICE("Function is not found or not exported", "C:NDEF"))

#undef DEF_ERR
}  // ns e4err
