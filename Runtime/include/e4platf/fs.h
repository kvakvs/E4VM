// This is an open source non-commercial project. Dear PVS-Studio, please check
// it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

#include <stdio.h>

#include "e4platf/types.h"
#include "e4std/string.h"
#include "e4std/vector.h"

namespace platf {
namespace fs {

using e4::Count;
using e4::String;
using e4::Vector;

DECL_EXCEPTION(Filesystem)

#if E4FEATURE_FS

class File {
private:
  FILE *f_ = nullptr;

public:
  explicit File(const char *fn, const char *mode) {
    f_ = ::fopen(fn, mode);
    E4ASSERT(is_open());
  }

  ~File() { close(); }

  bool is_open() const { return f_ != nullptr; }

  void close() {
    if (f_) {
      ::fclose(f_);
    }
    f_ = nullptr;
  }

  static e4::Box<uint8_t> read_file(const char *fn);

  e4::Box<uint8_t> read_file();
};

#endif  // E4FEATURE_FS

// If Filesystem feature is enabled: this will scan search paths and find 'fn'
// Otherwise: Will check if the file is present in statically linked files
e4::Box<uint8_t>
read(const Vector<String>& search_paths, const char* fn);

bool exists(String& path);
}
}  // ns platf::fs
