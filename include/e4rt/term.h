/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4platf/conf.h"
#include "e4platf/types.h"

#include "e4rt/term_tag.h"
#include "e4rt/box.h"

namespace e4 {

class ConsCell;
class TupleBoxHeader;
class BoxHeaderWord;

namespace dist {
    //
    // Creation in node specific data (pids, ports, refs)
    //
    constexpr Word CREATION_SIZE = 2;
    using Creation = Uint8;
    // MAX value for the creation field in pid, port and reference
    constexpr Creation MAX_CREATION = (1 << CREATION_SIZE);
    constexpr Creation ORIG_CREATION = 0;
    constexpr Creation INTERNAL_CREATION = 255;
}  // ns dist

class Term {
private:
    // Term representation as primary_tag:2 + value in the remaining bits
    struct PrimaryTaggedWord {
        PrimaryTag tag_:TAG1_TAG_BITS;
        Word val_:TAG1_VALUE_BITS;

        PrimaryTaggedWord() {}

        constexpr PrimaryTaggedWord(PrimaryTag pt, Word v)
                : tag_(pt), val_(v) {}
    };

    // Represents term as primary_tag:2 + immediate_tag:4 + value
    struct ImmediateTaggedWord {
        PrimaryTag primary_tag_:TAG1_TAG_BITS;
        ImmediateTag imm_tag_:IMM1_TAG_BITS;
        Word val_:IMM1_VALUE_BITS;

        ImmediateTaggedWord() {}

        constexpr ImmediateTaggedWord(PrimaryTag pt, ImmediateTag it,
                                      Word v)
                : primary_tag_(pt), imm_tag_(it), val_(v) {}
    };

    // Represents term as a small integer
    // primary_tag:2 + immediate_tag:4=0x1 + value which overlays 3 bits of
    // the immediate tag, hence why it is only 1 bit and is always true
    struct SmallTaggedImmediateWord {
        PrimaryTag primary_tag_:TAG1_TAG_BITS;
        bool imm_tag_:1;
        SignedWord val_:(TAG1_VALUE_BITS - 1);

        SmallTaggedImmediateWord() {}

        explicit constexpr SmallTaggedImmediateWord(SignedWord val)
                : primary_tag_(PrimaryTag::Immediate), imm_tag_(true),
                  val_(val) {}
    };

    union {
        Word raw_;
        PrimaryTaggedWord as_primary_;
        ImmediateTaggedWord as_imm_;
        SmallTaggedImmediateWord as_small_;
    };

    // contains zero arity tuple header. Use box_wrap() on address of this
    static TupleBoxHeader empty_tuple_;

public:
    // Construct any term from a raw word
    explicit constexpr Term(Word x) : raw_(x) {}

    Term() : raw_(0) {}

    explicit Term(ConsCell* cell_ptr)
            : as_primary_(PrimaryTag::Cons, ptr_to_val1(cell_ptr)) {}

    explicit constexpr Term(PrimaryTag pt, ImmediateTag itag, Word val2)
            : as_imm_(pt, itag, val2) {}

    explicit constexpr Term(PrimaryTag pt, Word val1)
            : as_primary_(pt, val1) {}

    // Get raw word
    Word get_raw() const { return raw_; }

    // Construct an atom from atom index
    static Term make_atom(Word a) {
        E4ASSERT(a < (1UL << IMM1_VALUE_BITS));
        return Term(PrimaryTag::Immediate, ImmediateTag::Atom, a);
    }

    //
    // Generic Box Utilities
    //

    bool is_immediate() const {
        return as_imm_.primary_tag_ == PrimaryTag::Immediate;
    }

    bool is_boxed() const {
        return as_primary_.tag_ == PrimaryTag::Boxed;
    }

    // A pointer with 2 bits trimmed to fit into val1_ of a term
    template<typename T>
    static Word ptr_to_val1(const T* ptr) {
        Word result = reinterpret_cast<Word>(ptr) & TAG1_VALUE_MASK;
        E4ASSERT(fits_in<Word>(result));
        return reinterpret_cast<Word>(result);
    }

    template<typename T>
    static Term box_wrap(T* ptr) {
        return Term(PrimaryTag::Boxed, ptr_to_val1(ptr));
    }

    BoxHeaderWord* unbox() const {
        E4ASSERT(is_boxed());
        return reinterpret_cast<BoxHeaderWord*>(raw_ & TAG1_VALUE_MASK);
    }

    //
    // Tuple Aspect
    //
    static Term make_zero_tuple() { return box_wrap(&empty_tuple_); }

    static Term make_tuple(TupleBoxHeader* tuple_box);

    //
    // Small Integer Aspect
    //
    static Term make_small(SignedWord s) {
        // TODO: Maybe can be optimized by providing an appropriate ctor
        Term result(PrimaryTag::Immediate, 0);
        result.as_small_.val_ = s;
        return result;
    }

    //
    // PID Aspect
    // Stuff goes into a box, so we only store a boxed header here
    //
#if E4FEATURE_ERLDIST
#error TODO: pids need attention in distributed mode
#endif
    static constexpr Word PID_ID_SIZE = 15;
    static constexpr Word PID_DATA_SIZE = 28; // for 32bit maximum
    static constexpr Word PID_SERIAL_SIZE = (PID_DATA_SIZE - PID_ID_SIZE);
    static_assert(PID_DATA_SIZE + BOXED_TAG_BITS <= BITS_PER_WORD,
                  "Pid does not fit the machine Word");

    static constexpr bool is_valid_pid_id(Word x) {
        return x < (1 << PID_ID_SIZE) - 1;
    }

    static constexpr bool is_valid_pid_serial(Word x) {
        return x < (1 << PID_SERIAL_SIZE) - 1;
    }

    static constexpr Word make_pid_data(Word ser, Word num) {
        return static_cast<Word>(ser << PID_ID_SIZE | num);
    }

    // Data arg is created using Term::make_pid_data
    static Term make_short_pid(Word data) {
        return Term(PrimaryTag::Immediate, ImmediateTag::ShortPid, data);
    }

    constexpr bool is_short_pid() const {
        return is_immediate() && as_imm_.imm_tag_ == ImmediateTag::ShortPid;
    }

    bool is_remote_pid() const {
        return is_boxed() && unbox()->tag() == BoxTag::Pid;
    }

    bool is_pid() const { return is_short_pid() || is_remote_pid(); }

//        constexpr Word short_pid_get_value() const {
//            return imm_val_;
//        }

};

static_assert(sizeof(Term) == sizeof(Word),
              "Term must have size of 1 word");

// TODO: This belongs to Immediate2 namespace
constexpr Term NIL = Term(PrimaryTag::Immediate,
                          ImmediateTag::Special, 0);

// TODO: This belongs to Immediate2 namespace
constexpr Term NON_VALUE = Term(PrimaryTag::Immediate,
                                ImmediateTag::Special, 1);

class ConsCell {
public:
    Term head_;
    Term tail_;
};

} // ns e4