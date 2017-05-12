CMAKE_MINIMUM_REQUIRED(VERSION 3.5)
PROJECT(E4VM CXX)

SET(CMAKE_CXX_STANDARD 14)

SET(E4_INVOKE_PVS 0)

IF("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    SET(E4_GCC 0)
ELSE()
    SET(E4_GCC 1)
ENDIF()

SET(COMPILATION_INTERMEDIATE_RESULTS    0)

## We do not use standard C++ library here, but we DO use C++ startup/exit code
#add_definitions(-nostdinc++)

##
##
INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/include)

## Erlang Runtime system for E4VM: types, bifs, memory etc
## Minus the VM itself this later can become a runtime library for compiled Erl
SET(E4_RUNTIME_SOURCES
    include/e4.h
    include/e4rt/atom_store.h
    include/e4rt/binary.h
    include/e4rt/box.h
    include/e4rt/bytecode.h
    include/e4rt/code_mgr.h
    include/e4rt/dist.h
    include/e4rt/ext_term_format.h
    include/e4rt/heap.h
    include/e4rt/messages.h
    include/e4rt/module.h
    include/e4rt/process.h
    include/e4rt/range_checker.h
    include/e4rt/scheduler.h
    include/e4rt/term.h
    include/e4rt/term_as_map_key.h
    include/e4rt/term_tag.h
    include/e4rt/vm.h
    src/e4rt/atom_store.cpp
    #src/e4rt/binary.cpp
    src/e4rt/bytecode.cpp
    src/e4rt/code_mgr.cpp
    src/e4rt/ext_term_format.cpp
    src/e4rt/module.cpp
    src/e4rt/process.cpp
    src/e4rt/scheduler.cpp
    src/e4rt/term.cpp
    src/e4rt/vm.cpp
    src/e4rt/vm_loop.cpp
    )

## Helpers and OS abstraction library
SET(E4_PLATF_SOURCES
    include/e4platf/byte_stream_reader.h
    include/e4platf/conf.h
    include/e4platf/debug.h
    include/e4platf/fs.h
    include/e4platf/mem.h
    include/e4platf/types.h
    src/e4platf/byte_stream_reader.cpp
    src/e4platf/debug.cpp
    src/e4platf/fs.cpp
    src/e4platf/mem.cpp
    )

# Minimal badly written replacements for C++ stuff optimized for size
SET(E4_STD_SOURCES
    include/e4std/array.h
    include/e4std/complicated.h
    include/e4std/free_fun.h
    include/e4std/map.h
    include/e4std/ptr.h
    include/e4std/sizes.h
    include/e4std/string.h
    include/e4std/stuff.h
    include/e4std/vector.h
    include/e4std/vector_impl.h
    include/e4std/view.h
    src/e4std/free_fun.cpp
    src/e4std/stuff.cpp
    src/e4std/vector.cpp
    )

SET(EMU_SOURCES
    src/main.cpp
    )

IF(CMAKE_BUILD_TYPE MATCHES Debug)
    ADD_DEFINITIONS(
            -Os
            -ggdb
            -DE4DEBUG=1
            -fno-unroll-loops
    )
ELSE()
    ADD_DEFINITIONS(
            -Os
            -g0
            -DE4DEBUG=0
    )
ENDIF()

ADD_DEFINITIONS(
    -DE4FEATURE_FS=1
    -DE4_WORD_SIZE=64
)
ADD_DEFINITIONS(
    -std=c++14
    -fno-rtti
    #-fno-exceptions
)
ADD_DEFINITIONS(
    -Wall -Werror
)

IF(CMAKE_CXX_COMPILER MATCHES clang)
    ADD_DEFINITIONS(
        -Weverything
        -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-gnu-anonymous-struct
        -Wno-zero-length-array -Wno-nested-anon-types
        -Wno-format-nonliteral -Wno-global-constructors
    )
ELSE()
    IF(E4_GCC)
        ADD_DEFINITIONS(
                -ffunction-sections
                -Wl,--gc-sections
        )
    ENDIF()
ENDIF()

ADD_LIBRARY(e4_runtime  ${E4_RUNTIME_SOURCES})
ADD_LIBRARY(e4_platform ${E4_PLATF_SOURCES})
ADD_LIBRARY(e4_std      ${E4_STD_SOURCES})

ADD_EXECUTABLE(e4emu ${EMU_SOURCES})
TARGET_LINK_LIBRARIES(e4emu
    e4_runtime e4_platform e4_std)

IF(COMPILATION_INTERMEDIATE_RESULTS)
    TARGET_COMPILE_OPTIONS(e4_std       PUBLIC -save-temps)
    TARGET_COMPILE_OPTIONS(e4_platform  PUBLIC -save-temps)
    TARGET_COMPILE_OPTIONS(e4_runtime   PUBLIC -save-temps)

    TARGET_COMPILE_OPTIONS(e4emu-debug  PUBLIC -save-temps)
ENDIF()

#IF(E4_GCC)
#    IF(CMAKE_BUILD_TYPE MATCHES Debug)
#        SET_TARGET_PROPERTIES(e4emu PROPERTIES LINK_FLAGS
#                "-flto -static -static-libgcc -static-libstdc++")
#    ELSE()
#        SET_TARGET_PROPERTIES(e4emu PROPERTIES LINK_FLAGS
#                "-flto -static -static-libgcc -static-libstdc++")
#    ENDIF()
#ELSE()
#ENDIF(E4_GCC)

#SET_TARGET_PROPERTIES(e4emu PROPERTIES LINK_FLAGS
#        "-static -static-libgcc -static-libstdc++")

##
## PVS-Studio Static Checker
##
IF(E4_INVOKE_PVS)
    INCLUDE(PVS-Studio.cmake)
    pvs_studio_add_target(
        TARGET analyze ALL
        OUTPUT FORMAT tasklist
        PREPROCESSOR gcc
        LOG "PVS-report.tasks"
        ANALYZE e4_runtime e4_platform e4_std e4emu
        CXX_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
        C_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
        CONFIG "${CMAKE_SOURCE_DIR}/PVS-Studio.cfg"
    )
ENDIF()