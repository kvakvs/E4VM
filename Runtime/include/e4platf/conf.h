// This is an open source non-commercial project. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
//

#pragma once

namespace e4 {

//
//====== BEGIN FEATURES ======
//

#ifndef E4FEATURE_FS
#   define E4FEATURE_FS 0
#endif
constexpr bool FEATURE_FS = (E4FEATURE_FS != 0);

#ifndef E4FEATURE_ERLDIST
#   define E4FEATURE_ERLDIST 0
#endif
constexpr bool FEATURE_ERLDIST = (E4FEATURE_ERLDIST != 0);

#ifndef E4FEATURE_FLOAT
#   define E4FEATURE_FLOAT 0
#endif
constexpr bool FEATURE_FLOAT = (E4FEATURE_FLOAT != 0);

#ifndef E4FEATURE_BIGNUM
#   define E4FEATURE_BIGNUM 0
#endif
constexpr bool FEATURE_BIGNUM = (E4FEATURE_BIGNUM != 0);

#ifndef E4FEATURE_MAPS
#   define E4FEATURE_MAPS 0
#endif
constexpr bool FEATURE_MAPS = (E4FEATURE_MAPS != 0);

#ifndef E4FEATURE_HOTCODELOAD
#   define E4FEATURE_HOTCODELOAD 0
#endif
constexpr bool FEATURE_HOTCODELOAD = (E4FEATURE_HOTCODELOAD != 0);

//
//====== END FEATURES ======
//

constexpr bool DEBUG_MODE = (E4DEBUG != 0);

#undef BIG_ENDIAN
#undef LITTLE_ENDIAN
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#   define G_BIGENDIAN 0
    constexpr bool BIG_ENDIAN = false;
#else
#   define G_BIGENDIAN 1
    constexpr bool BIG_ENDIAN = true;
#endif

#define DECL_EXCEPTION(NAME)                                    \
    class NAME##Error: public e4std::RuntimeError {             \
    public:                                                     \
        NAME##Error(const char* e): e4std::RuntimeError(e) {}   \
        virtual const char* what() const noexcept;              \
    };
#define IMPL_EXCEPTION(NAME)                            \
    const char* NAME##Error::what() const noexcept {    \
        return e4std::RuntimeError::what();             \
    }
#define DECL_IMPL_EXCEPTION(NAME) DECL_EXCEPTION(NAME) IMPL_EXCEPTION(NAME)

#define E4_NORETURN __attribute__((noreturn))

#if __has_cpp_attribute(nodiscard)
    #define E4_NODISCARD [[nodiscard]]
#elif __has_cpp_attribute(gnu::warn_unused_result)
    #define E4_NODISCARD [[gnu::warn_unused_result]]
#else
    #define E4_NODISCARD
#endif

#if __has_cpp_attribute(maybe_unused)
    #define E4_MAYBE_UNUSED [[maybe_unused]]
#elif __has_cpp_attribute(gnu::unused)
    #define E4_MAYBE_UNUSED [[gnu::unused]]
#else
    #define E4_MAYBE_UNUSED(X)
#endif

#if __has_cpp_attribute(fallthrough)
    #define E4_FALLTHROUGH [[fallthrough]]
#elif __has_cpp_attribute(clang::fallthrough)
    #define E4_FALLTHROUGH [[clang::fallthrough]]
#else
    #define E4_FALLTHROUGH
#endif

} // ns e4
