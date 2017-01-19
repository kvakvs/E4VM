/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4platf/types.h"

#include "e4rt/term_tag.h"

namespace e4 {

namespace box_tag {
typedef enum {
    Tuple,
    PositiveBignum,
    NegativeBignum,
    Float,
    Map,
    FunObject,
    Export,
    Pid,
    Port,
    Ref,
    DestroyedSomething,
    ProcBinary,
    RCBinary,
    MatchContext,
    SubBinary,
} Type;
} // ns box_tag;
using BoxTag = box_tag::Type;

class Term;

//
// First word of every boxed value is a header
//
class BoxHeaderWord {
private:
    PrimaryTag primary_tag_:TAG1_TAG_BITS;
    BoxTag tag_:BOXED_TAG_BITS;   // least-significant goes first
    Word val_:BOXED_VALUE_BITS;
public:
    constexpr BoxHeaderWord(BoxTag t, Word val)
            : primary_tag_(PrimaryTag::Header), tag_(t), val_(val) {}

    void set_tag(BoxTag t) {
        E4ASSERT(primary_tag_ == PrimaryTag::Header);
        tag_ = t;
    }

    BoxTag tag() const {
        E4ASSERT(primary_tag_ == PrimaryTag::Header);
        return tag_;
    }

    void set_val(Word a) {
        E4ASSERT(primary_tag_ == PrimaryTag::Header);
        val_ = a;
    }

    Word val() const {
        E4ASSERT(primary_tag_ == PrimaryTag::Header);
        return val_;
    }

    // Given a pointer to whatever T* setup first word
    template<class T>
    static BoxHeaderWord* setup_a_box(T* memory, BoxTag bt, Word val) {
        auto hword = reinterpret_cast<BoxHeaderWord *>(memory);
        hword->primary_tag_ = PrimaryTag::Header;
        hword->set_tag(bt);
        hword->set_val(val);
        return hword;
    }
};

static_assert(sizeof(BoxHeaderWord) == sizeof(Word),
              "BoxHeaderWord must have 1 word size");

// A tuple header
class TupleBoxHeader {
private:
    BoxHeaderWord header_;
    Word data_[0];
public:
    constexpr TupleBoxHeader(Word val)
            : header_(BoxTag::Tuple, val), data_() {}

    operator Term() const; // impl in term.cpp

    Word element(Word z_index) const { return data_[z_index]; }

    void set_element(Word z_index, Word val) { data_[z_index] = val; }

    BoxTag tag() const { return header_.tag(); }

    Word val() const { return header_.val(); }
};

static_assert(sizeof(TupleBoxHeader) == sizeof(Word),
              "BoxHeader must have 1 word size");


} // ns e4

