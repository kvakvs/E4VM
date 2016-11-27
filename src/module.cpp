#include "gluon/module.h"
#include "gluon/byte_stream_reader.h"

namespace gluon {

const char *SIGNATURE = "3EAM";
const char *SIG_EXPORTS = "ExpT";
const char *SIG_ATOMS = "Atom";
const char *SIG_CODE = "Code";


BoxView<Uint8> Module::find_code_section(const UniqueBox<Uint8> &d) {
    ByteStreamReader reader(d.view());

    reader.assert_and_skip(SIGNATURE, Bytes(4));

    Bytes sig_size(4);
    while (reader.have(sig_size)) {
        if (reader.see_ahead(SIG_CODE, sig_size)) {
            reader.advance(sig_size);
            Bytes code_size(reader.varint_u<Word>());
            return BoxView<Uint8>(reader.pos(), code_size.bytes());
        }
    }

    return BoxView<Uint8>(nullptr, 0);
}

} // ns gluon
