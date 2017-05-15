// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4rt/ext_term_format.h"
#include "e4rt/binary.h"
#include "e4rt/messages.h"

namespace e4 {

DECL_IMPL_EXCEPTION(ExternalTerm)

// Reads long atom as string and attempts to create it in atom table.
Term ExtTerm::read_atom_string_i16(tool::Reader& r) {
  Word sz = r.read_big_u16();
  String atom_str = r.read_string(sz);
  return vm()->add_atom(atom_str);
}

// Reads short atom as string and attempts to create it in atom table.
Term ExtTerm::read_atom_string_i8(tool::Reader& r) {
  Word sz = r.read_byte();
  String atom_str = r.read_string(sz);
  return vm()->add_atom(atom_str);
}

// Reads tag byte, then reads long or short atom as string and attempts to
// create it in atom table.
Term ExtTerm::read_tagged_atom_string(tool::Reader& r) {
  Tag tag = static_cast<Tag>(r.read_byte());
  if (tag == Tag::AtomExt) {
    return read_atom_string_i16(r);
  } else if (tag == Tag::SmallAtomExt) {
    return read_atom_string_i8(r);
  }
  E4FAIL(e4err::etf_atom_expected);
}

Node* ExtTerm::get_node(Term /*sysname*/, dist::Creation /*creation*/) {
#if E4FEATURE_ERLDIST
  E4TODO("distribution support pid etf")
#endif
  return vm()->dist_this_node();
}

Term ExtTerm::make_pid(Term sysname,
                       Word id,
                       Word serial,
                       uint8_t creation) {
  if (!Term::is_valid_pid_id(id) || !Term::is_valid_pid_serial(serial)) {
    E4FAIL(e4err::etf_bad_pid);
  }
  // TODO: check valid creation
  Word data = Term::make_pid_data(serial, id);
  auto node = get_node(sysname, creation);

  if (node == vm()->dist_this_node()) {
    return Term::make_short_pid(data);
  }
#if E4FEATURE_ERLDIST
  E4TODO("distribution support pid etf");
#endif
  // distribution disabled, no want remote pids
  E4FAIL(e4err::no_feat_erldist);
}

Term ExtTerm::read_tuple(Heap& heap, tool::Reader& r, Word arity) {
  if (arity == 0) {
    return Term::make_zero_tuple();
  }

  WordSize box_sz(arity);
  auto tuple = heap.allocate_tuple_box(arity);

  // fill elements or die horribly if something does not decode
  for (Word i = 0; i < arity; ++i) {
    Term read_val = ExtTerm::read(heap, r);
    tuple->set_element(i, read_val.get_raw());
  }

  return Term::make_tuple(tuple);
}

Term ExtTerm::read_with_marker(Heap& heap, tool::Reader& r) {
  r.assert_byte(ETF_MARKER);
  return ExtTerm::read(heap, r);
}

#if E4FEATURE_MAPS
Term read_map(Heap* heap, tool::Reader& r) {
  Word arity = r.read_bigendian_i32();

  if (arity == 0) {
    return Term::make_zero_map();
  }

  for (auto i = 0; i < arity; ++i) {
    Term key = read_ext_term2(heap, r);
    if (key.is_non_value()) {
      return key;
    }

    Term val = read_ext_term2(heap, r);
    if (val.is_non_value()) {
      return val;
    }
  }
}
#endif  // FEATURE_MAPS

Term ExtTerm::read_string_ext(Heap& heap, tool::Reader& r) {
  Word length = r.read_big_u16();
  if (length == 0) {
    return NIL;
  }

  Term result = NIL;
  Term* ref = &result;

  for (Word i = 0; i < length; ++i) {
    ConsCell* cons = heap.allocate_cons();
    cons->head_ = Term::make_small(r.read_byte());
    *ref = Term(cons);
    ref = &cons->tail_;
  }

  *ref = NIL;
  return result;
}

Term ExtTerm::read_list_ext(Heap& heap, tool::Reader& r) {
  Word length = r.read_big_u32();

  Term result = NIL;
  Term* ref = &result;

  for (SignedWord i = static_cast<SignedWord>(length) - 1; i >= 0; i--) {
    ConsCell* cons = heap.allocate_cons();
    cons->head_ = ExtTerm::read(heap, r);
    *ref = Term(cons);
    ref = &cons->tail_;
  }

  *ref = ExtTerm::read(heap, r);
  return result;
}

Term ExtTerm::read_binary(Heap& heap, tool::Reader& r) {
  Word length = r.read_big_u32();
  if (length <= PROCBIN_THRESHOLD) {
    auto pbox = make_proc_binary(heap, ByteSize(length));
    uint8_t* data = pbox->data_;
    r.read(data, length);
    return Term::box_wrap(pbox);
  }
  auto rcbox = make_rc_binary(ByteSize(length));
  uint8_t* data = rcbox->bin_->data_;
  r.read(data, length);
  return Term::box_wrap(rcbox);
}

Term ExtTerm::read(Heap& heap, tool::Reader& r) {
  auto t = static_cast<Tag>(r.read_byte());
  switch (t) {
    case Tag::Compressed:
      // =80; 4 bytes size; compressed data
      E4TODO("compressed etf");
      E4IF_NODEBUG(break;)

    case Tag::SmallIntegerExt:
      return Term::make_small(r.read_byte());

    case Tag::IntegerExt: {
      // 32-bit integer
      SignedWord n = r.read_big_s(4);
      if (BITS_PER_WORD > 32) {
        // fits into small_int if platform is x64
        return Term::make_small(n);
      } else {  // hardware bits = 32
#if E4FEATURE_BIGNUM
        if (Term::is_big(n)) {
          E4TODO("constr bignum etf");
        } else {
          return Term::make_small(n);
        }
#else
        // no bignum, and hardware bits not enough: much fail here
        E4FAIL(e4err::no_feat_bignum);
#endif
      }  // hardware bits = 32
    }    // integer_ext

#if E4FEATURE_FLOAT
    case OLD_FLOAT_STRING_EXT: {
      E4TODO("parse float string etf");
    }  // old string float_ext
    case IEEE_FLOAT_EXT: {
      E4TODO("make ieee 8byte double etf");
    }  // new 8byte double float_ext
#else
    case Tag::OldFloatStringExt:
    case Tag::IeeeFloatExt:
      E4FAIL(e4err::no_feat_float);
#endif

    case Tag::AtomUtf8Ext:  // fall through
    case Tag::AtomExt:
      return read_atom_string_i16(r);

    case Tag::SmallAtomUtf8Ext:  // fall through
    case Tag::SmallAtomExt:
      return read_atom_string_i8(r);

    case Tag::ReferenceExt: {
      // format: N atom string, 4byte id, 1byte creation
      //      Term node = read_atom_string(r);
      //      Word id = r.read_bigendian_i32();
      //      u8_t creation = r.read_byte();
      E4TODO("ref etf");
      E4IF_NODEBUG(break;)
    }  // end reference_ext

    case Tag::PortExt: {
      // format: N atom string, 4byte id, 1byte creation
      //      Term node = read_atom_string(r);
      //      Word id = r.read_bigendian_i32();
      //      u8_t creation = r.read_byte();
      E4TODO("port etf");
      E4IF_NODEBUG(break;)
    }  // end reference_ext

    case Tag::PidExt: {
      // format: N atom string, 4byte id, 4byte serial, 1byte cre
      Term node = read_tagged_atom_string(r);
      Word id = r.read_big_u32();
      Word serial = r.read_big_u32();
      uint8_t creation = r.read_byte();
      return make_pid(node, id, serial, creation);
    }  // end reference_ext

    case Tag::SmallTupleExt:
      return read_tuple(heap, r, r.read_byte());
    case Tag::LargeTupleExt:
      return read_tuple(heap, r, r.read_big_u32());

    case Tag::MapExt:
#if E4FEATURE_MAPS
      // return read_map(heap, r);
      throw err::TODO("etf MAPS");
#else
      E4FAIL(e4err::no_feat_maps);
#endif

    case Tag::NilExt:
      return NIL;
    case Tag::StringExt:
      return read_string_ext(heap, r);
    case Tag::ListExt:
      return read_list_ext(heap, r);

    case Tag::BinaryExt:
      return read_binary(heap, r);
    case Tag::BitBinaryExt:
      E4TODO("read bit-binary etf");

    case Tag::SmallBigExt:
    case Tag::LargeBigExt:
#if E4FEATURE_BIGNUM
      throw err::FeatureMissing("BIGNUM");
#else
      E4FAIL(e4err::no_feat_bignum);
#endif

    case Tag::DistHeader:
    case Tag::AtomCacheRef:
      // Std::fmt("invalid ETF value tag %d\n", t);
      E4FAIL(e4err::etf_bad_tag);
  }  // switch tag

  E4LOG1("read etf: tag %d\n", static_cast<int>(t));
  failf("Something is not right");
  //    return NON_VALUE;
}  // parse function

}  // ns e4
