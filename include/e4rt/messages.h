#pragma once

#include "e4platf/debug.h"

namespace e4err {
#define DEFERR(NAME, VAL) constexpr const char *NAME = VAL;

DEFERR(no_feat_bignum,  E4CHOICE("Bignum feature is disabled", "F:BIG"))
DEFERR(no_feat_maps,    E4CHOICE("Maps feature is disabled", "F:MAP"))
DEFERR(no_feat_float,   E4CHOICE("Float feature is disabled", "F:FLO"))
DEFERR(no_feat_erldist, E4CHOICE("Erlang Distribution feature is disabled", "F:DIS"))

DEFERR(etf_bad_tag,     E4CHOICE("Bad tag encountered while reading ext term format", "E:ETF1"))

#undef DEFERR
} // ns e4err
