#pragma once

namespace gluon {

#ifndef GLUON_FEATURE_FS
#   define GLUON_FEATURE_FS 0
#endif
constexpr bool FEATURE_FS = (GLUON_FEATURE_FS != 0);

#ifndef GLUON_FEATURE_ERLDIST
#   define GLUON_FEATURE_ERLDIST 0
#endif
constexpr bool FEATURE_ERLDIST = (GLUON_FEATURE_ERLDIST != 0);

#ifndef GLUON_FEATURE_FLOAT
#   define GLUON_FEATURE_FLOAT 0
#endif
constexpr bool FEATURE_FLOAT = (GLUON_FEATURE_FLOAT != 0);

#ifndef GLUON_FEATURE_BIGNUM
#   define GLUON_FEATURE_BIGNUM 0
#endif
constexpr bool FEATURE_BIGNUM = (GLUON_FEATURE_BIGNUM != 0);

#ifndef GLUON_FEATURE_MAPS
#   define GLUON_FEATURE_MAPS 0
#endif
constexpr bool FEATURE_MAPS = (GLUON_FEATURE_MAPS != 0);

constexpr bool DEBUG_MODE = (GLUON_DEBUG != 0);

#undef BIG_ENDIAN
#undef LITTLE_ENDIAN
#if __BYTE_ORDER == __LITTLE_ENDIAN
#   define G_BIGENDIAN 0
    constexpr bool BIG_ENDIAN = false;
#else
#   define G_BIGENDIAN 1
    constexpr bool BIG_ENDIAN = true;
#endif
    constexpr bool LITTLE_ENDIAN = not BIG_ENDIAN;

#define DECL_EXCEPTION(NAME)                           \
    class NAME : public std::runtime_error {           \
       public:                                         \
        NAME(const char* e) : std::runtime_error(e) {} \
        virtual const char* what() const noexcept;     \
    };
#define IMPL_EXCEPTION(NAME)                  \
    const char* NAME::what() const noexcept { \
        return std::runtime_error::what();    \
    }
#define DECL_IMPL_EXCEPTION(NAME) DECL_EXCEPTION(NAME) IMPL_EXCEPTION(NAME)

} // ns g_erts
