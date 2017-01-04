#pragma once

#include "g_erts/box.h"
#include "g_erts/heap.h"
#include "g_erts/vm.h"

namespace gluon {

    constexpr Word PROCBIN_THRESHOLD = 50;

    // A heap object representing an onheap binary (< threshold)
    class ProcBinaryBox {
    public:
        BoxHeader header_;
        Uint8 data_[0];
    };

    // Refcounted binary on the separate heap
    class RCBinary {
    public:
        Word size_;
        Word rc_; // ref count
        Uint8 data_[0];
    };

    // A process heap object holding refcount to an external RCBinary
    class RCBinaryBox {
    public:
        BoxHeader header_;
        RCBinary *bin_;
    };

    template <typename T>
    ProcBinaryBox *make_proc_binary(VM &vm, Heap &heap, GenericSize<T> size) {
        G_ASSERT(size.bytes() <= PROCBIN_THRESHOLD);
        auto w_size = Heap::word_size(size);
        auto pbin = (ProcBinaryBox *)heap.allocate_box(w_size);
        pbin->header_.set_tag(BoxTag::ProcBinary);
        pbin->header_.set_arity(size.bytes());
        return pbin;
    }

    template <typename T>
    RCBinaryBox *make_rc_binary(VM &vm, Heap &heap, GenericSize<T> size) {
        // Too large, we allocate an RCBinaryBox instead
        auto w_size = Heap::word_size(GenericSize<RCBinaryBox>(1));
        auto rcbin = (RCBinaryBox *)heap.allocate_box(w_size);
        rcbin->header_.set_tag(BoxTag::RCBinary);
        rcbin->header_.set_arity(0);
        return rcbin;
    }

} // ns gluon
