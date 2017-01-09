/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4rt/box.h"
#include "e4rt/heap.h"
#include "e4rt/vm.h"

namespace e4 {

    constexpr Word PROCBIN_THRESHOLD = 50;

    // A heap object representing an onheap binary (< threshold)
    class ProcBinaryBox {
    public:
        BoxHeaderWord header_;
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
        BoxHeaderWord header_;
        RCBinary *bin_;
    };

    template <typename T>
    ProcBinaryBox *make_proc_binary(Heap &heap, GenericSize<T> size) {
        dprintf("bin: new procbin %zu b\n", size.bytes());
        G_ASSERT(size.bytes() <= PROCBIN_THRESHOLD);
        auto wsz = Heap::word_size(size);
        auto newbox = heap.allocate_box(wsz, BoxTag::ProcBinary, size.bytes());
        auto pbin = reinterpret_cast<ProcBinaryBox *>(newbox);
        pbin->header_.set_tag(BoxTag::ProcBinary);
        pbin->header_.set_val(size.bytes());
        return pbin;
    }

    template <typename T>
    RCBinaryBox *make_rc_binary(VM &vm, GenericSize<T> size) {
        dprintf("bin: new rcbin %zu b\n", size.bytes());
        // Too large, we allocate an RCBinaryBox instead
        auto w_size = Heap::word_size(GenericSize<RCBinaryBox>(1));
        auto newbox = vm.binary_heap_.allocate_box(
                            w_size, BoxTag::RCBinary, size.bytes());
        auto rcbin = reinterpret_cast<RCBinaryBox *>(newbox);
        rcbin->header_.set_tag(BoxTag::RCBinary);
        rcbin->header_.set_val(0);
        return rcbin;
    }

} // ns e4
