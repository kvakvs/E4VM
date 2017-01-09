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

void Module::load(const e4std::BoxView<Uint8> &data) {
    tool::Reader bsr(data);

    bsr.assert_and_advance(SIG_MODULE, ByteSize(4));
    ByteSize all_sz(bsr.read_varint_u<Word>());
    bsr.assert_have(all_sz);

    char section_sig[5] = {0, 0, 0, 0, 0};

    while (bsr.have(ByteSize(SIG_SIZE))) {
        bsr.read<char>(section_sig, SIG_SIZE);
        ByteSize section_sz(bsr.read_varint_u<Word>());
        auto section_view = e4std::BoxView<Uint8>(bsr.pos(), section_sz.bytes());

        if (not ::memcmp(section_sig, SIG_ATOMS, SIG_SIZE)) {
            // Atoms table
            Vector<String> atoms;
            load_atoms(section_view, /*out*/ atoms);
            name_ = vm_.add_atom(atoms.front());
            for (const auto &a: atoms) {
                vm_.add_atom(a);
            }
        } else if (not ::memcmp(section_sig, SIG_CODE, SIG_SIZE)) {
            // Code
            code_.resize(section_sz.bytes());
            ::memcpy(code_.data(), bsr.pos(), section_sz.bytes());
        } else if (not ::memcmp(section_sig, SIG_LTRL, SIG_SIZE)) {
            // Literals table
            Vector<Term> literals;
            load_literals(section_view, literals);
            // TODO: GC on module unload or drop heap completely
        }

        bsr.advance(section_sz);
    }
    // TODO: set up atom refs in code
    // TODO: set up literal refs in code
}

void Module::load_literals(const e4std::BoxView<Uint8> &adata,
                           Vector<Term>& result) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    result.clear();
    result.reserve(n);

    for (Word i = 0; i < n; ++i) {
        result.push_back(ExtTerm::read_with_marker(vm_, literals_, bsr));
    }
}

// Reads atom table and populates a string vector. Does not populate
// the global atom table.
void Module::load_atoms(const e4std::BoxView<Uint8> &adata,
                        Vector<String> &result) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    result.clear();
    result.reserve(n);

    for (Word i = 0; i < n; ++i) {
        result.push_back(bsr.read_varlength_string());
    }
}

} // ns e4
