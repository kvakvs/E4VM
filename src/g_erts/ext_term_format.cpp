#include "g_erts/binary.h"
#include "g_erts/ext_term_format.h"
#include "g_erts/term_layout.h"

namespace gluon {

namespace err {
DECL_IMPL_EXCEPTION(ExternalTerm)
}  // ns err

// Reads long atom as string and attempts to create it in atom table.
Term ExtTerm::read_atom_string_i16(VM& vm, tool::Reader& r) {
    Word sz = r.read_big_u16();
    String atom_str = r.read_string(sz);
    return vm.add_atom(atom_str);
}

// Reads short atom as string and attempts to create it in atom table.
Term ExtTerm::read_atom_string_i8(VM& vm, tool::Reader& r) {
    Word sz = r.read_byte();
    String atom_str = r.read_string(sz);
    return vm.add_atom(atom_str);
}

// Reads tag byte, then reads long or short atom as string and attempts to
// create it in atom table.
Term ExtTerm::read_tagged_atom_string(VM& vm, tool::Reader& r) {
    Tag tag = (Tag)r.read_byte();
    if (tag == Tag::AtomExt) {
        return read_atom_string_i16(vm, r);
    } else if (tag == Tag::SmallAtomExt) {
        return read_atom_string_i8(vm, r);
    }
    throw err::ExternalTerm("atom expected");
}

Node* ExtTerm::get_node(VM& vm, Term /*sysname*/, dist::Creation /*creation*/) {
#if FEATURE_ERL_DIST
    G_TODO("distribution support pid etf")
#endif
    return vm.dist_this_node();
}

Term ExtTerm::make_pid(VM& vm, Term sysname, Word id, Word serial,
                       Uint8 creation) {
    if (!Term::is_valid_pid_id(id) || !Term::is_valid_pid_serial(serial)) {
        throw err::ExternalTerm("bad pid");
    }
    // TODO: check valid creation
    Word data = Term::make_pid_data(serial, id);
    auto node = get_node(vm, sysname, creation);

    if (node == vm.dist_this_node()) {
        return Term::make_short_pid(data);
    }
#if FEATURE_ERL_DIST
    G_TODO("distribution support pid etf");
#endif
    // distribution disabled, no want remote pids
    throw err::FeatureMissing("ERL_DIST");
}

Term ExtTerm::read_tuple(VM& vm, Heap& heap, tool::Reader& r, Word arity) {
    if (arity == 0) {
        return Term::make_zero_tuple();
    }

    WordSize box_sz(layout::Tuple::box_size(arity));
    auto box = heap.allocate_box(box_sz);
    box->set_arity(arity);

    // fill elements or die horribly if something does not decode
    for (Word i = 0; i < arity; ++i) {
        layout::Tuple::set_element(box, i, ExtTerm::read(vm, heap, r));
    }

    return Term::make_tuple(box);
}

Term ExtTerm::read_with_marker(VM& vm, Heap& heap, tool::Reader &r) {
    r.assert_byte(ETF_MARKER);
    return ExtTerm::read(vm, heap, r);
}

#if FEATURE_MAPS
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

Term ExtTerm::read_string_ext(Heap &heap, tool::Reader &r) {
    Word length = r.read_big_u16();
    if (length == 0) {
        return NIL;
    }

    Term result = NIL;
    Term *ref = &result;

    for (Word i = 0; i < length; ++i) {
        ConsCell *cons = heap.allocate_cons();
        cons->head_ = Term::make_small(r.read_byte());
        *ref = Term(cons);
        ref = &cons->tail_;
    }

    *ref = NIL;
    return result;
}

Term ExtTerm::read_list_ext(VM &vm, Heap &heap, tool::Reader &r) {
    Word length = r.read_big_u32();

    Term result = NIL;
    Term *ref = &result;

    for (SignedWord i = (SignedWord) length - 1; i >= 0; i--) {
        ConsCell *cons = heap.allocate_cons();
        cons->head_ = ExtTerm::read(vm, heap, r);
        *ref = Term(cons);
        ref = &cons->tail_;
    }

    *ref = ExtTerm::read(vm, heap, r);
    return result;
}

Term ExtTerm::read_binary(VM &vm, Heap &heap, tool::Reader &r) {
    Word length = r.read_big_u32();
    if (length <= PROCBIN_THRESHOLD) {
        auto pbox = make_proc_binary(vm, heap, ByteSize(length));
        Uint8 *data = pbox->data_;
        r.read(data, length);
        return Term::box_wrap(pbox);
    }
    auto rcbox = make_rc_binary(vm, heap, ByteSize(length));
    Uint8 *data = rcbox->bin_->data_;
    r.read(data, length);
    return Term::box_wrap(rcbox);
}

Term ExtTerm::read(VM &vm, Heap &heap, tool::Reader &r) {
    auto t = (Tag) r.read_byte();
    switch (t) {
        case Tag::Compressed:
            // =80; 4 bytes size; compressed data
            G_TODO("compressed etf");
            G_IF_NODEBUG(break;)

        case Tag::SmallIntegerExt:
            return Term::make_small(r.read_byte());

        case Tag::IntegerExt: {
            // 32-bit integer
            SignedWord n = r.read_big_s(4);
            if (BITS_PER_WORD > 32) {
                // fits into small_int if platform is x64
                return Term::make_small(n);
            } else {  // hardware bits = 32
#if GLUON_FEATURE_BIGNUM
                if (Term::is_big(n)) {
                    G_TODO("construct bignum etf");
                } else {
                    return Term::make_small(n);
                }
#else
                // no bignum, and hardware bits not enough: much fail here
                throw err::FeatureMissing("BIGNUM");
#endif
            }  // hardware bits = 32
        }      // integer_ext

#if GLUON_FEATURE_FLOAT
        case OLD_FLOAT_STRING_EXT: {
            G_TODO("parse float string etf");
        }  // old string float_ext
        case IEEE_FLOAT_EXT: {
            G_TODO("make ieee 8byte double etf");
        }  // new 8byte double float_ext
#else
        case Tag::OldFloatStringExt:
        case Tag::IeeeFloatExt:
            throw err::FeatureMissing("FLOAT");
#endif

        case Tag::AtomUtf8Ext:  // fall through
        case Tag::AtomExt:
            return read_atom_string_i16(vm, r);

        case Tag::SmallAtomUtf8Ext:  // fall through
        case Tag::SmallAtomExt:
            return read_atom_string_i8(vm, r);

        case Tag::ReferenceExt: {
            // format: N atom string, 4byte id, 1byte creation
            //      Term node = read_atom_string(r);
            //      Word id = r.read_bigendian_i32();
            //      u8_t creation = r.read_byte();
            G_TODO("ref etf");
            G_IF_NODEBUG(break;)
        }  // end reference_ext

        case Tag::PortExt: {
            // format: N atom string, 4byte id, 1byte creation
            //      Term node = read_atom_string(r);
            //      Word id = r.read_bigendian_i32();
            //      u8_t creation = r.read_byte();
            G_TODO("port etf");
            G_IF_NODEBUG(break;)
        }  // end reference_ext

        case Tag::PidExt: {
            // format: N atom string, 4byte id, 4byte serial, 1byte cre
            Term node = read_tagged_atom_string(vm, r);
            Word id = r.read_big_u32();
            Word serial = r.read_big_u32();
            Uint8 creation = r.read_byte();
            return make_pid(vm, node, id, serial, creation);
        }  // end reference_ext

        case Tag::SmallTupleExt:
            return read_tuple(vm, heap, r, r.read_byte());
        case Tag::LargeTupleExt:
            return read_tuple(vm, heap, r, r.read_big_u32());

        case Tag::MapExt:
#if GLUON_FEATURE_MAPS
                // return read_map(heap, r);
                throw err::TODO("etf MAPS");
#else
                throw err::FeatureMissing("MAPS");
#endif

        case Tag::NilExt:
            return NIL;
        case Tag::StringExt:
            return read_string_ext(heap, r);
        case Tag::ListExt:
            return read_list_ext(vm, heap, r);

        case Tag::BinaryExt:
            return read_binary(vm, heap, r);
        case Tag::BitBinaryExt:
            throw err::TODO("read bit-binary etf");

        case Tag::SmallBigExt:
        case Tag::LargeBigExt:
#if GLUON_FEATURE_BIGNUM
                throw err::FeatureMissing("BIGNUM");
#else
                throw err::TODO("BIGNUM");
#endif

        case Tag::DistHeader:
        case Tag::AtomCacheRef:
            // Std::fmt("invalid ETF value tag %d\n", t);
            throw err::ExternalTerm("bad etf tag");
        default:
            dprintf("read etf: tag %d\n", (int)t);
            failf("Something is not right");
    }  // switch tag
    return NON_VALUE;
}  // parse function

} // ns gluon
