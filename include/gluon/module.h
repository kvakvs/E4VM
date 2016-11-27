#pragma once

#include "gluon/types.h"
#include "gluon/module.h"

namespace gluon {

class Module {
private:
    UniqueBox<Uint8> data_;
    BoxView<Uint8> code_;
public:
    Module(UniqueBox<Uint8> && data)
            : data_(std::move(data)), code_(find_code_section(data_))
    {
    }

private:
    static BoxView<Uint8> find_code_section(const UniqueBox<Uint8> &d);
};

} // ns gluon
