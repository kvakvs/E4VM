#pragma once

#include "g_platform/types.h"
#include "box.h"

namespace gluon {
    class ConsCell;
    class BoxHeader;

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

    // primary tag bits
    constexpr Word TAG1_TAG_BITS = 2;
    constexpr Word TAG1_VALUE_BITS = BITS_PER_WORD - TAG1_TAG_BITS;
    constexpr Word TAG1_VALUE_MASK = (~0ULL) << TAG1_TAG_BITS;

    enum class PrimaryTag: Word {
        Boxed,
        Cons,
        Immediate,
    };
    enum class ImmediateTag: Word {
        Atom = 0,  //(0 << PRIMARY_SIZE) | IMMED1,
        SmallInt = 1,
        ShortPid = 2,
        ShortPort = 4,
        FpRegister = 6,
        Catch = 8,
        XRegister = 10,
        YRegister = 12,
        Special = 14,  // includes nil,noval,rip
    };
    constexpr Word IMM1_TAG_BITS = 4;
    constexpr Word IMM1_VALUE_BITS = BITS_PER_WORD - IMM1_TAG_BITS;

    class Term {
    private:
        union {
            Word raw_;
            // Primary tag and value of the remaining bits
            struct {
                PrimaryTag tag1_:TAG1_TAG_BITS;
                union {
                    struct {
                        ImmediateTag imm_tag_:IMM1_TAG_BITS;
                        Word imm_val_:IMM1_VALUE_BITS;
                    };

                    // To store an immediate1-small integer
                    struct {
                        bool small_imm_tag_:1;
                        SignedWord small_val_:(TAG1_VALUE_BITS-1);
                    };

                    Word val1_:TAG1_VALUE_BITS;
                };
            };
        };
        // contains zero arity tuple header
        static Term empty_tuple_[1];
    public:
        // Construct any term from a raw word
        explicit constexpr Term(Word x): raw_(x) {}
        explicit Term(ConsCell *cell_ptr)
                : tag1_(PrimaryTag::Cons), val1_(ptr_to_val1(cell_ptr)) {
        }

        explicit constexpr Term(PrimaryTag pt, ImmediateTag itag, Word val2)
            : tag1_(pt), imm_tag_(itag), imm_val_(val2) {}

        explicit constexpr Term(PrimaryTag pt, Word val1)
            : val1_(val1), tag1_(pt) {}

        // Get raw word
        Word get_raw() const { return raw_; }

        // Construct an atom from atom index
        static Term make_atom(Word a) {
            G_ASSERT(a < (1UL << IMM1_VALUE_BITS));
            return Term(PrimaryTag::Immediate, ImmediateTag::Atom, a);
        }

        //
        // Generic Box Utilities
        //

        bool is_boxed() const { return tag1_ == PrimaryTag::Boxed; }

        // A pointer with 2 bits trimmed to fit into val1_ of a term
        template <typename T>
        static Word ptr_to_val1(const T *ptr) {
            Word result = (Word)ptr & TAG1_VALUE_MASK;
            G_ASSERT(fits_in<Word>(result));
            return (Word) result;
        }

        template <typename T>
        static Term box_wrap(T *ptr) {
            return Term(PrimaryTag::Boxed, ptr_to_val1(ptr));
        }

        BoxHeader *unbox() const {
            G_ASSERT(is_boxed());
            return (BoxHeader *)(raw_ & TAG1_VALUE_MASK);
        }

        //
        // Tuple Aspect
        //
        static Term make_zero_tuple() { return box_wrap(empty_tuple_); }
        static Term make_tuple(BoxHeader *box_contents);

        //
        // Small Integer Aspect
        //
        static Term make_small(SignedWord s) {
            Term result(PrimaryTag::Immediate, 0);
            result.small_imm_tag_ = true;
            result.small_val_ = s;
            return result;
        }

        //
        // PID Aspect
        //
        static constexpr Word PID_ID_SIZE = 15;
        static constexpr Word PID_DATA_SIZE = 28; // for 32bit maximum
        static constexpr Word PID_SERIAL_SIZE = (PID_DATA_SIZE - PID_ID_SIZE);
        static_assert(PID_DATA_SIZE + 4 <= BITS_PER_WORD,
                     "Pid does not fit the machine Word");

        static constexpr bool is_valid_pid_id(Word x) {
            return x < (1 << PID_ID_SIZE) - 1;
        }
        static constexpr bool is_valid_pid_serial(Word x) {
            return x < (1 << PID_SERIAL_SIZE) - 1;
        }
        static constexpr Word make_pid_data(Word ser, Word num) {
            return (Word) (ser << PID_ID_SIZE | num);
        }
        // Data arg is created using Term::make_pid_data
        static Term make_short_pid(Word data) {
            return Term(PrimaryTag::Immediate, ImmediateTag::ShortPid, data);
        }

        constexpr bool is_short_pid() const {
            return tag1_ == PrimaryTag::Immediate
                   && imm_tag_ == ImmediateTag::ShortPid;
        }

        bool is_remote_pid() const {
            return tag1_ == PrimaryTag::Boxed &&
                    unbox()->tag() == BoxTag::Pid;
        }

        bool is_pid() const { return is_short_pid() || is_remote_pid(); }

//        constexpr Word short_pid_get_value() const {
//            return imm_val_;
//        }

    };

    constexpr Term NIL = Term(PrimaryTag::Immediate,
                              ImmediateTag::Special, 0);
    constexpr Term NON_VALUE = Term(PrimaryTag::Immediate,
                                    ImmediateTag::Special, 1);

    class ConsCell {
    public:
        Term head_;
        Term tail_;
    };

} // ns gluon
