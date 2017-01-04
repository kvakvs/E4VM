#pragma once

#include "g_platform/types.h"
//#include "g_erts/term.h"

namespace gluon {

    enum class BoxTag: Word {
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
    };

    class Term;

    // First word of a boxed value is a header, which defines contents and size
    class BoxHeader {
    private:
        static constexpr Word TAG_BITS = 4;
        static constexpr Word VALUE_BITS = BITS_PER_WORD - TAG_BITS;
        struct {
            Word val_:VALUE_BITS;
            BoxTag tag_:TAG_BITS;
        };
        Word data_[0];
    public:
        constexpr BoxHeader(BoxTag t, Word val): tag_(t), val_(val), data_() {}
        operator Term () const;

        void set_tag(BoxTag t) { tag_ = t; }
        BoxTag tag() const { return tag_; }

        void set_arity(Word a) { val_ = a; }
        Word arity() const { return val_; }

        Word element(Word z_index) const { return data_[z_index]; }
        void set_element(Word z_index, Word val) { data_[z_index] = val; }
    };
    static_assert(sizeof(BoxHeader) == sizeof(Word),
                  "Box header must have 1 word size");


} // ns gluon

