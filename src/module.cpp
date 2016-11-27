#include "gluon/module.h"
#include "gluon/byte_stream_reader.h"
#include "gluon/vm.h"

namespace gluon {

const char *SIGNATURE = "3EAM";
const char *SIG_EXPORTS = "ExpT";
const char *SIG_ATOMS = "Atom";
const char *SIG_CODE = "Code";

BoxView<Uint8> Module::find_section(const char *sig,
                                    const BoxView<Uint8> &data) {
    ByteStreamReader reader(data);

    reader.assert_and_skip(SIGNATURE, Bytes(4));

    Bytes sig_size(4);
    while (reader.have(sig_size)) {
        if (reader.see_ahead(sig, sig_size)) {
            // skip 4 bytes signature
            reader.advance(sig_size);

            // Get section size
            Bytes section_sz(reader.varint_u<Word>());
            dprintf("mod:find_sec %s size %d\n", sig, section_sz.bytes());
            return BoxView<Uint8>(reader.pos(), section_sz.bytes());
        }
        reader.advance(sig_size);
        reader.advance(Bytes(reader.varint_u<Word>()));
    }

    return BoxView<Uint8>(nullptr, 0);
}

void Module::load(VM &vm, const BoxView<Uint8> &data) {
    // Atom table
    //
    auto atoms = find_section(SIG_ATOMS, data);
    auto tmp_atom_tab = load_atoms(atoms);
    for (const auto &a: tmp_atom_tab) {
        vm.add_atom(a);
    }

    //
    // Code, prepare to run
    //
    code_.copy_from(find_section(SIG_CODE, data));
    // TODO: set up atom refs in code
    // TODO: set up literal refs in code
}

// Reads atom table and populates a string vector. Does not populate
// the global atom table.
Vector<String> Module::load_atoms(const BoxView<Uint8> &adata) {
    ByteStreamReader reader(adata);
    Word atoms_count = reader.varint_u<Word>();

    Vector<String> result;
    result.reserve(atoms_count);
    dprintf("mod:load_a atoms count %d\n", atoms_count);

    for (auto i = 0; i < atoms_count; ++i) {
        result.push_back(reader.string());
    }

    return result;
}

} // ns gluon
