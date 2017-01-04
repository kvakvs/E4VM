#include "g_platform/byte_stream_reader.h"

#include "g_erts/module.h"
#include "g_erts/vm.h"
#include "g_erts/ext_term_format.h"

namespace gluon {

constexpr Word SIG_SIZE = 4; // module and section signature length
constexpr const char *SIG_MODULE = "E4J1"; // Erl-Forth J1Forth Flavour

constexpr const char *SIG_ATOMS = "ATOM";   // atoms section tag
constexpr const char *SIG_ATOMS_GZ = "atom"; // gzipped
constexpr const char *SIG_CODE = "CODE";    // code section tag
constexpr const char *SIG_CODE_GZ = "code"; // gzipped
constexpr const char *SIG_LTRL = "LTRL";    // literals section tag
constexpr const char *SIG_LTRL_GZ = "ltrl"; // gzipped

BoxView<Uint8> Module::find_section(const char *want_sig,
                                    const BoxView<Uint8> &data) {
    tool::Reader bsr(data);

    bsr.assert_and_advance(SIG_MODULE, ByteSize(4));
    ByteSize all_sz(bsr.read_varint_u<Word>());
    bsr.assert_have(all_sz);

    const ByteSize sig_size(4);
    char section_sig[5] = {0, 0, 0, 0, 0};

    while (bsr.have(sig_size)) {
        bsr.read<char>(section_sig, sig_size.bytes());
        ByteSize section_sz(bsr.read_varint_u<Word>());

        if (0 == std::memcmp(section_sig, want_sig, sig_size.bytes())) {
            return BoxView<Uint8>(bsr.pos(), section_sz.bytes());
        }

        bsr.advance(section_sz);
    }

    return BoxView<Uint8>(nullptr, 0);
}

void Module::load(const BoxView<Uint8> &data) {
    tool::Reader bsr(data);

    bsr.assert_and_advance(SIG_MODULE, ByteSize(4));
    ByteSize all_sz(bsr.read_varint_u<Word>());
    bsr.assert_have(all_sz);

    char section_sig[5] = {0, 0, 0, 0, 0};

    while (bsr.have(ByteSize(SIG_SIZE))) {
        bsr.read<char>(section_sig, SIG_SIZE);
        ByteSize section_sz(bsr.read_varint_u<Word>());
        auto section_view = BoxView<Uint8>(bsr.pos(), section_sz.bytes());

        if (not std::memcmp(section_sig, SIG_ATOMS, SIG_SIZE)) {
            // Atoms table
            for (const auto &a: load_atoms(section_view)) {
                vm_.add_atom(a);
            }
        } else if (not std::memcmp(section_sig, SIG_CODE, SIG_SIZE)) {
            // Code
            code_.copy_from(find_section(SIG_CODE, data));
        } else if (not std::memcmp(section_sig, SIG_LTRL, SIG_SIZE)) {
            // Literals table
            load_literals(section_view);
            // TODO: store literals on separate heap or something.
            // TODO: GC on module unload or drop heap completely
        }

        bsr.advance(section_sz);
    }
    // TODO: set up atom refs in code
    // TODO: set up literal refs in code
}

Vector<Term> Module::load_literals(const BoxView<Uint8> &adata) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    Vector<Term> result;
    result.reserve(n);
    for (Word i = 0; i < n; ++i) {
        result.push_back(ExtTerm::read_with_marker(vm_, literals_, bsr));
    }
    return result;
}

// Reads atom table and populates a string vector. Does not populate
// the global atom table.
Vector<String> Module::load_atoms(const BoxView<Uint8> &adata) {
    tool::Reader bsr(adata);
    Word n = bsr.read_varint_u<Word>();
    Vector<String> result;
    result.reserve(n);

    for (Word i = 0; i < n; ++i) {
        result.push_back(bsr.read_varlength_string());
    }

    return result;
}

} // ns g_erts
