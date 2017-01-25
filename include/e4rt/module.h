/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */
#pragma once

#include "e4platf/debug.h"
#include "e4platf/types.h"

#include "e4rt/term.h"
#include "e4rt/heap.h"
#include "e4rt/module.h"
#include "e4rt/bytecode.h"

#include "e4std/view.h"

namespace e4 {

class VM;
using e4std::ByteView;

// Element in exports table. Used to find functions referred by {M,F,Arity}
// from the outside
class Export {
public:
    Term fun_;
    Word arity_;
    Word offset_; // how far from the module code start, in 16bit words

    explicit Export(): Export(NON_VALUE, 0, 0) {}

    Export(Term f, Word a, Word offs) : fun_(f), arity_(a), offset_(offs) {}

    // Compares two exports as void* vs void*, returns -1 if a<b, 1 if a>b, or 0
    static int compare_pvoid(const void* a, const void* b);

#if E4DEBUG
    void print(const VM& vm) const;
#endif // DEBUG
};

class Module {
private:
    Term name_ = NON_VALUE; // atom name
    // TODO: group Vector<> members on the process heap, have them preallocated
    PODVector<J1Opcode> code_;
    PODVector<Word> labels_; // labels table TODO: merge with code maybe?

    PODVector<Term> literals_;
    Heap literal_heap_;
    Vector<Export> exports_;

    VM& vm_;

public:
    explicit Module(VM& vm) : literal_heap_(64), vm_(vm) {}

    void load(const ByteView& data);

    Term name() const { return name_; }

    Export* find_export(const MFArity& mfa) const;

    // Adds code start to export offset
    CodeAddress get_export_address(const Export& exp) const;

private:
    void load_atoms(const ByteView& adata, Vector<String>& out);
    void load_literals(const ByteView& adata);
    void load_exports(const ByteView& adata,
                      const Vector<Term>& atoms_lookup);
    void load_labels(const ByteView& adata);
};

} // ns e4
