//
// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include "e4platf/byte_stream_reader.h"

#include "e4rt/module.h"
#include "e4rt/vm.h"
#include "e4rt/ext_term_format.h"

namespace e4 {

constexpr Word SIG_SIZE = 4; // module and section signature length
constexpr const char *SIG_MODULE = "E4J1"; // Erl-Forth J1Forth Flavour

constexpr const char *SIG_ATOMS = "ATOM";   // atoms section tag
//constexpr const char *SIG_ATOMS_GZ = "atom"; // gzipped
constexpr const char *SIG_CODE = "CODE";    // code section tag
//constexpr const char *SIG_CODE_GZ = "code"; // gzipped
constexpr const char *SIG_LTRL = "LTRL";    // literals section tag
//constexpr const char *SIG_LTRL_GZ = "ltrl"; // gzipped
constexpr const char *SIG_EXPT = "EXPT";    // exports section tag
//constexpr const char *SIG_EXPT_GZ = "expt"; // gzipped

void Module::load(const ByteView& data) {
    tool::Reader bsr(data);

    bsr.assert_and_advance(SIG_MODULE, ByteSize(4));
    ByteSize all_sz(bsr.read_varint_u<Word>());
    bsr.assert_have(all_sz);

    char section_sig[5] = {0, 0, 0, 0, 0};
    Vector<Term> atoms_t; // after loaded, will be used in exports

    while (bsr.have(ByteSize(SIG_SIZE))) {
        bsr.read<char>(section_sig, SIG_SIZE);
        ByteSize section_sz(bsr.read_varint_u<Word>());
        auto section_view = ByteView(bsr.pos(), section_sz.bytes());

        if (not ::memcmp(section_sig, SIG_ATOMS, SIG_SIZE)) {
            //
            // Atoms table
            //
            Vector<String> atoms;
            load_atoms(section_view, /*out*/ atoms);
            name_ = vm_.add_atom(atoms.front());
            for (const auto &a: atoms) {
                atoms_t.push_back(vm_.add_atom(a));
            }
        } else if (not ::memcmp(section_sig, SIG_CODE, SIG_SIZE)) {
            //
            // Code
            //
            code_.resize(section_sz.bytes());
            ::memcpy(code_.data(), bsr.pos(), section_sz.bytes());

            // TODO: set up literal refs in code
        } else if (not ::memcmp(section_sig, SIG_LTRL, SIG_SIZE)) {
            //
            // Literals table
            //
            E4ASSERT(literals_.empty());
            E4ASSERT(literal_heap_.empty());
            load_literals(section_view);
        } else if (not ::memcmp(section_sig, SIG_EXPT, SIG_SIZE)) {
            load_exports(section_view, atoms_t);
        } else {
            E4FAIL("Unknown section");
        }

        bsr.advance(section_sz);
    }
    // TODO: set up atom refs in code
}

void Module::load_literals(const ByteView& adata) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    literals_.reserve(n);

    for (Word i = 0; i < n; ++i) {
        const auto lit = ExtTerm::read_with_marker(vm_, literal_heap_, bsr);
        literals_.push_back(lit);
    }
}

// Reads atom table and populates a string vector. Does not populate
// the global atom table.
void Module::load_atoms(const ByteView& adata,
                        Vector<String> &result) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    result.clear();
    result.reserve(n);

    for (Word i = 0; i < n; ++i) {
        result.push_back(bsr.read_varlength_string());
    }
}

void Module::load_exports(const ByteView& adata,
                          const Vector<Term>& atoms_lookup) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    exports_.reserve(n);

    for (Word i = 0; i < n; ++i) {
        auto fn_atom_index = bsr.read_varint_u<Word>();
        E4ASSERT(atoms_lookup.size() > fn_atom_index);

        Export ex(atoms_lookup[fn_atom_index],
                  bsr.read_varint_u<Word>(),
                  bsr.read_varint_u<Word>());
        exports_.push_back(ex);
    }
    // We then use binary search so better this be sorted
    exports_.sort(Export::compare_pvoid);
}

Export* Module::find_export(const MFArity& mfa) const {
    Export exp(mfa.fun_, mfa.arity_, 0);
    return exports_.binary_search(&exp, Export::compare_pvoid);
}

int Export::compare_pvoid(const void* a, const void* b) {
    auto pa = static_cast<const Export*>(a);
    auto pb = static_cast<const Export*>(b);

    if (e4std::compare_less(pa->fun_, pb->fun_)) {
        return -1;
    } else if (e4std::compare_equal(pa->fun_, pb->fun_)) {
        if (pa->arity_ < pb->arity_) {
            return -1;
        } else if (pa->arity_ == pb->arity_) {
            return 0;
        }
    }
    return 1;
}

} // ns e4
