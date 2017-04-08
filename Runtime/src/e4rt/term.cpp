// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#include <stdio.h>

#include "e4rt/box.h"
#include "e4rt/term.h"
#include "e4rt/vm.h"

namespace e4 {

TupleBoxHeader Term::empty_tuple_ = TupleBoxHeader(0);

Term Term::make_tuple(TupleBoxHeader* tuple_box) {
  // Assumption: boxheader already has tuple tag and arity set
  E4ASSERT(tuple_box->tag() == BoxTag::Tuple);
  E4ASSERT(tuple_box->val() > 0);
  return box_wrap(tuple_box);
}

bool Term::is_value() const {
  return raw_ != NON_VALUE.raw_;
}

TupleBoxHeader::operator Term() const {
  return Term::box_wrap(this);
}

#if E4DEBUG
void MFArgs::print(const VM& vm) const {
  vm.print(mod_);
  ::printf(":");
  vm.print(fun_);
  ::printf("/%zu", args_.count());
}
#endif  // DEBUG

}  // ns e4
