#include "naming.h"
#include <cstdlib>

namespace naming { // is hard

std::string function_name(const std::string& name, int arity) {
  // Build "/N" for arity suffix
  char arity_s[32];
  std::snprintf(arity_s, sizeof(arity_s) - 1, "/%d", arity);
  arity_s[sizeof(arity_s) - 1] = '\0';

  return name + arity_s;
}

} // ns naming
