/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include <bits/unique_ptr.h>

#include "g_erts/term.h"
#include "g_erts/box.h"

namespace gluon {

// A growable heap to hold Erlang terms
class Heap {
private:
    Word capacity_;
    Word htop_;
    std::unique_ptr<Word []> heap_;

public:
    explicit Heap(Word init_size): capacity_(0), htop_(0) {
        this->alloc_heap(init_size);
    }

    bool have(Word want_size) const {
        return (capacity_ - htop_) >= want_size;
    }

    ConsCell *allocate_cons() {
        dprintf("heap: alloc cons\n");
        return reinterpret_cast<ConsCell *>(allocate_raw_words(2));
    }

    // A helper to find any size in words
    template <typename T>
    static WordSize word_size(GenericSize<T> sz) {
        return WordSize(sz.template as_units<Word>());
    }

    BoxHeader *allocate_box(WordSize want_size) {
        dprintf("heap: alloc box %zu w\n", want_size.units());
        Word *box = allocate_raw_words(want_size.units() + 1);
        return reinterpret_cast<BoxHeader *>(box);
    }

private:
    Word *allocate_raw_words(Word want_size) {
        if (not have(want_size)) {
            // Grow or something
            G_TODO("Grow or something");
            return nullptr;
        }
        auto result = heap_.get() + htop_;
        htop_ += want_size;
        dprintf("h::alloc(%zu) htop %zu size %zu\n",
                want_size, htop_, capacity_);
        return result;
    }
    void alloc_heap(Word size) {
        heap_ = mem::make_uniq_array<Word>(size);
        capacity_ = size;
    }
};

} // ns gluon
