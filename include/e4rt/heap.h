/* * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#pragma once

#include "e4std/ptr.h"

#include "e4rt/term.h"
#include "e4rt/box.h"

namespace e4 {

// A growable heap to hold Erlang terms
class Heap {
private:
    Word capacity_;
    Word htop_;
    UniqueArrayPtr<Word> heap_;

public:
    explicit Heap(Word init_size) : capacity_(0), htop_(0) {
        this->alloc_heap(init_size);
    }

    bool have(Word want_size) const {
        return (capacity_ - htop_) >= want_size;
    }

    ConsCell* allocate_cons() {
        E4LOG("heap: alloc cons\n");
        return reinterpret_cast<ConsCell*>(allocate_raw_words(2));
    }

    // A helper to find any size in words
    template <typename T>
    static WordSize word_size(GenericSize<T> sz) {
        return WordSize(sz.template as_units<Word>());
    }

    // Create a box in memory, set up its first word with boxtag and value bits
    BoxHeaderWord* allocate_box(WordSize want_size, BoxTag bt, Word val) {
        E4LOG1("heap: alloc box %zu w\n", want_size.units());
        Word* box = allocate_raw_words(want_size.units() + 1);
        return BoxHeaderWord::setup_a_box(box, bt, val);
    }

    TupleBoxHeader* allocate_tuple_box(Word arity) {
        return reinterpret_cast<TupleBoxHeader*>(
                allocate_box(WordSize(arity), BoxTag::Tuple, arity));
    }

private:
    Word* allocate_raw_words(Word want_size) {
        if (not have(want_size)) {
            // Grow or something
            E4TODO("Grow or something");
            return nullptr;
        }
        auto result = heap_.get() + htop_;
        htop_ += want_size;
        E4LOG3("h::alloc(%zu) htop %zu size %zu\n",
               want_size, htop_, capacity_);
        return result;
    }

    void alloc_heap(Word size) {
        heap_ = e4std::make_array<Word>(size);
        capacity_ = size;
    }
};

} // ns e4
