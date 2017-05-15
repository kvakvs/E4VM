// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include "e4platf/conf.h"
#include "e4platf/types.h"
#include "e4rt/box.h"
#include "e4rt/term_tag.h"
#include <functional>

namespace e4 {

class ConsCell;
class TupleBoxHeader;
class BoxHeaderWord;

namespace dist {
//
// Creation in node specific data (pids, ports, refs)
//
constexpr Word CREATION_SIZE = 2;
using Creation = uint8_t;
// MAX value for the creation field in pid, port and reference
constexpr Creation MAX_CREATION = (1 << CREATION_SIZE);
constexpr Creation ORIG_CREATION = 0;
constexpr Creation INTERNAL_CREATION = 255;
}  // ns dist


namespace pid {
static constexpr Word PID_ID_SIZE = 15;
static constexpr Word PID_DATA_SIZE = 28;  // for 32bit maximum
static constexpr Word PID_SERIAL_SIZE = (PID_DATA_SIZE - PID_ID_SIZE);
static_assert(PID_DATA_SIZE + BOXED_TAG_BITS <= BITS_PER_WORD,
              "Pid doesn't fit into the Word");
}  // ns pid


class Term {
 private:
  // Term representation as primary_tag:2 + value in the remaining bits
  class PrimaryTaggedWord {
  private:
    Word tag_ : PRIMARY_TAG_BITS;

    E4_MAYBE_UNUSED Word val_ : PRIMARY_VALUE_BITS;

  public:
    PrimaryTaggedWord() {
    }  //-V730

    constexpr PrimaryTaggedWord(PrimaryTag pt, Word v)
      : tag_(static_cast<Word>(pt)),
        val_(v) {
    }

    constexpr PrimaryTag get_primary_tag() const {
      return static_cast<PrimaryTag>(tag_);
    }
  };
  static_assert(sizeof(PrimaryTaggedWord) == sizeof(Word),
                "PrimaryTaggedWord must have size of 1 word");

  // Represents immed1 term as <<value, immed1_tag:2, IMMED:2>>
  class Immed1TaggedWord {
  private:
    E4_MAYBE_UNUSED Word primary_tag_ : PRIMARY_TAG_BITS;

    Word imm1_tag_ : IMM1_TAG_BITS;

    Word val_ : IMM1_VALUE_BITS;

  public:
    Immed1TaggedWord() {}  //-V730

    constexpr Immed1TaggedWord(Immed1Tag it, Word v)
      : primary_tag_(static_cast<Word>(PrimaryTag::Immediate)),
        imm1_tag_(static_cast<Word>(it)),
        val_(v) {
    }

    constexpr Immed1TaggedWord(Immed1Tag it, SignedWord v)
      : primary_tag_(static_cast<Word>(PrimaryTag::Immediate)),
        imm1_tag_(static_cast<Word>(it)),
        val_(static_cast<Word>(v)) {
    }

    constexpr Immed1Tag get_imm1_tag() const {
      return static_cast<Immed1Tag>(imm1_tag_);
    }

    constexpr Word get_value() const {
      return val_;
    }

    constexpr SignedWord get_signed_val() const {
      return static_cast<SignedWord>(val_);
    }
  };
  static_assert(sizeof(Immed1TaggedWord) == sizeof(Word),
                "Immed1TaggedWord must have size of 1 word");

  // Represents immed2 term as <<value, immed2_tag:2, ?IMM1_IMM2:2, IMMED:2>>
  class Immed2TaggedWord {
  private:
    E4_MAYBE_UNUSED Word primary_tag_ : PRIMARY_TAG_BITS;

    E4_MAYBE_UNUSED Word imm1_tag_ : IMM1_TAG_BITS;

    Word imm2_tag_ : IMM2_TAG_BITS;

    Word val_ : IMM2_VALUE_BITS;

  public:
    Immed2TaggedWord() {}  //-V730

    constexpr Immed2TaggedWord(Immed2Tag it, Word v)
            : primary_tag_(static_cast<Word>(PrimaryTag::Immediate)),
              imm1_tag_(static_cast<Word>(Immed1Tag::Immed2)),
              imm2_tag_(static_cast<Word>(it)),
              val_(v) {
    }

    constexpr Immed2Tag get_imm2_tag() const {
      return static_cast<Immed2Tag>(imm2_tag_);
    }

    constexpr Word get_value() const {
      return val_;
    }
  };
  static_assert(sizeof(Immed2TaggedWord) == sizeof(Word),
                "Immed2TaggedWord must have size of 1 word");

  // Represents (special for E4) immed3 term as
  // <<value, immed3_tag:2, ?IMM2_IMM3:2, ?IMM1_IMM2:2, IMMED:2>>
  class Immed3TaggedWord {
  private:
    E4_MAYBE_UNUSED Word primary_tag_ : PRIMARY_TAG_BITS;

    E4_MAYBE_UNUSED Word imm1_tag_ : IMM1_TAG_BITS;

    E4_MAYBE_UNUSED Word imm2_tag_ : IMM2_TAG_BITS;

    Word imm3_tag_ : IMM3_TAG_BITS;

    Word val_ : IMM3_VALUE_BITS;

  public:
    Immed3TaggedWord() {}  //-V730

    constexpr Immed3TaggedWord(Immed3Tag it, Word v)
      : primary_tag_(static_cast<Word>(PrimaryTag::Immediate)),
        imm1_tag_(static_cast<Word>(Immed1Tag::Immed2)),
        imm2_tag_(static_cast<Word>(Immed2Tag::Immed3)),
        imm3_tag_(static_cast<Word>(it)),
        val_(v) {
    }

    constexpr Immed3Tag get_imm3_tag() const {
      return static_cast<Immed3Tag>(imm3_tag_);
    }

    constexpr Word get_value() const {
      return val_;
    }
  };
  static_assert(sizeof(Immed3TaggedWord) == sizeof(Word),
                "Immed3TaggedWord must have size of 1 word");

  // contains zero arity tuple header. Use box_wrap() on address of this
  static TupleBoxHeader empty_tuple_;

 public:
  union {
    Word raw_;
    PrimaryTaggedWord as_primary_;
    Immed1TaggedWord as_imm1_;
    Immed2TaggedWord as_imm2_;
    Immed3TaggedWord as_imm3_;
  };

 public:
  // Construct any term from a raw word
  explicit constexpr Term(Word x) : raw_(x) {}

  Term() : raw_(0) {}

  bool operator == (const Term& other) const {
    return raw_equal(other);
  }

  bool raw_equal(const Term& other) const {
    return raw_ == other.raw_;
  }

  explicit Term(ConsCell* cell_ptr)
    : as_primary_(PrimaryTag::Cons, ptr_to_val1(cell_ptr)) {}

  explicit constexpr Term(Immed1Tag itag, Word val2)
    : as_imm1_(itag, val2) {}

  explicit constexpr Term(Immed1Tag itag, SignedWord val2)
    : as_imm1_(itag, val2) {}

  explicit constexpr Term(Immed2Tag itag, Word val2)
    : as_imm2_(itag, val2) {}

  explicit constexpr Term(Immed3Tag itag, Word val3)
    : as_imm3_(itag, val3) {}

  explicit constexpr Term(PrimaryTag pt, Word val1)
    : as_primary_(pt, val1) {}

  // Get raw word
  Word get_raw() const { return raw_; }

  // Construct an atom from atom index
  static constexpr Term make_atom(Word a) {
    E4ASSERT(a <= IMM2_MAX_VALUE);
    return Term(Immed2Tag::Atom, a);
  }

  //
  // Generic Box Utilities
  //

  constexpr bool is_immediate() const {
    return as_primary_.get_primary_tag() == PrimaryTag::Immediate;
  }

  // TODO: Check if this produces efficient assembly
  constexpr bool is_immed2() const {
    return is_immediate() && as_imm1_.get_imm1_tag() == Immed1Tag::Immed2;
  }

  // TODO: Check if this produces efficient assembly
  constexpr bool is_atom() const {
    return is_immed2() && as_imm2_.get_imm2_tag() == Immed2Tag::Atom;
  }

  bool is_boxed() const {
    return as_primary_.get_primary_tag() == PrimaryTag::Boxed;
  }

  // A pointer with 2 bits trimmed to fit into val1_ of a term
  template <typename T>
  static Word ptr_to_val1(const T* ptr) {
    Word result = reinterpret_cast<Word>(ptr) & PRIMARY_VALUE_MASK;
    E4ASSERT(fits_in<Word>(result));
    return reinterpret_cast<Word>(result);
  }

  template <typename T>
  static Term box_wrap(T* ptr) {
    return Term(PrimaryTag::Boxed, ptr_to_val1(ptr));
  }

  BoxHeaderWord* unbox() const {
    E4ASSERT(is_boxed());
    return reinterpret_cast<BoxHeaderWord*>(raw_ & PRIMARY_VALUE_MASK);
  }

  //
  // Tuple Aspect
  //
  static Term make_zero_tuple() {
    return box_wrap(&empty_tuple_);
  }

  static Term make_tuple(TupleBoxHeader* tuple_box);

  //
  // Small Integer Aspect
  //
  constexpr static Term make_small(SignedWord s) {
    return Term(Immed1Tag::Small, s);
  }

  static Term make_integer(Word val) {
    if (val < (1UL << IMM1_VALUE_BITS)) {
      return make_small(static_cast<SignedWord>(val));
    }
    E4FAIL("do bigint here");
  }

//
// PID Aspect
// Stuff goes into a box, so we only store a boxed header here
//
#if E4FEATURE_ERLDIST
#error TODO: pids need attention in distributed mode
#endif
  static constexpr bool is_valid_pid_id(Word x) {
    return x < (1 << pid::PID_ID_SIZE) - 1;
  }

  static constexpr bool is_valid_pid_serial(Word x) {
    return x < (1 << pid::PID_SERIAL_SIZE) - 1;
  }

  static constexpr Word make_pid_data(Word ser, Word num) {
    return static_cast<Word>(ser << pid::PID_ID_SIZE | num);
  }

  // Data arg is created using Term::make_pid_data
  static constexpr Term make_short_pid(Word data) {
    return Term(Immed1Tag::Pid, data);
  }

  constexpr bool is_short_pid() const {
    return is_immediate() && as_imm1_.get_imm1_tag() == Immed1Tag::Pid;
  }

  bool is_remote_pid() const {
    return is_boxed() && unbox()->tag() == BoxTag::Pid;
  }

  bool is_pid() const { return is_short_pid() || is_remote_pid(); }

  bool is_value() const;

  //
  // Register Aspect
  //

  static constexpr Term make_xreg(Word i) {
    return Term(Immed3Tag::XReg, i);
  }

  static constexpr Term make_yreg(Word i) {
    return Term(Immed3Tag::YReg, i);
  }

  static constexpr Term make_fpreg(Word i) {
    return Term(Immed3Tag::FloatReg, i);
  }

  //
  // Floating Point Aspect
  //

#if E4FEATURE_FLOAT
  static Term make_float(Float f);
#else
  static Term make_float(Float) {
    return make_nil();
  }
#endif

  //
  // Special values and imm3 stuff
  //

  static constexpr Term make_nil() {
    return Term(Immed2Tag::Special, 0);
  }

  static constexpr Term make_nonvalue() {
    return Term(Immed2Tag::Special, 0);
  }
};


static_assert(sizeof(Term) == sizeof(Word), "Term must have size of 1 word");

constexpr Term NIL = Term::make_nil();

constexpr Term NON_VALUE = Term::make_nonvalue();


class ConsCell {
public:
  Term head_;

  Term tail_;
};


class Arity { // TODO: Use a smaller type but alignment will eat it away
private:
  Word val_;

public:
  explicit Arity(Word x) : val_(x) {}

  bool operator<(const Arity& other) const {
    return val_ < other.val_;
  }

  bool operator==(const Arity& other) const {
    return val_ == other.val_;
  }

  template<typename T = Word>
  T get() const {
    return static_cast<T>(val_);
  }
};


class MFArity {
 public:
  Term mod_;
  Term fun_;
  Arity arity_;

  MFArity() = delete;
  MFArity(Term m, Term f, Arity a) : mod_(m), fun_(f), arity_(a) {}
};


using e4::ArrayRef;
class VM;


class MFArgs {
 public:
  Term mod_;

  Term fun_;

  ArrayRef<Term> args_;

  MFArgs(Term m, Term f, const ArrayRef<Term>& args)
    : mod_(m), fun_(f), args_(args) {
  }

  MFArity as_mfarity() const {
    return MFArity(mod_, fun_, Arity {args_.count()});
  }

#if E4DEBUG
  void print(const VM& vm) const;
#endif
};

}  // ns e4


namespace std {
  template<> struct hash<e4::Term> {
    size_t operator()(const e4::Term &t) const {
      return std::hash<e4::Word>()(t.get_raw());
    }
  };
} // ns std.hash
