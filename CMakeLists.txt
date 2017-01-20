cmake_minimum_required(VERSION 3.5)
project(E4VM)

set(CMAKE_CXX_STANDARD 14)

set(CMAKE_C_COMPILER /usr/bin/clang)
set(CMAKE_CXX_COMPILER /usr/bin/clang++)

set(COMPILATION_INTERMEDIATE_RESULTS    0)
set(BUILD_RELEASE                       0)

## We do not use standard C++ library here, but we DO use C++ startup/exit code
add_definitions(-nostdinc++)

##
##
include_directories(${CMAKE_SOURCE_DIR}/include)
add_definitions(
    -DE4FEATURE_FS=1
    -DE4_WORD_SIZE=64
)
add_definitions(
    -std=c++17
    -fno-rtti -fno-unroll-loops
    -Weverything -Wall -Werror
    -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-gnu-anonymous-struct
    -Wno-zero-length-array -Wno-nested-anon-types
    -Wno-format-nonliteral -Wno-global-constructors
)

## Erlang Runtime system for E4VM: types, bifs, memory etc
## Minus the VM itself this later can become a runtime library for compiled Erl
set(E4_RUNTIME_SOURCES
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
    include/e4rt/term.h
    include/e4rt/term_as_map_key.h
    include/e4rt/term_tag.h
    include/e4rt/vm.h
    src/e4rt/atom_store.cpp
    src/e4rt/binary.cpp
    src/e4rt/code_mgr.cpp
    src/e4rt/ext_term_format.cpp
    src/e4rt/module.cpp
    src/e4rt/process.cpp
    src/e4rt/term.cpp
    src/e4rt/vm.cpp
    )

## Helpers and OS abstraction library
set(E4_PLATF_SOURCES
    include/e4platf/byte_stream_reader.h
    include/e4platf/conf.h
    include/e4platf/debug.h
    include/e4platf/fs.h
    include/e4platf/mem.h
    include/e4platf/types.h
    src/e4platf/debug.cpp
    src/e4platf/fs.cpp
    src/e4platf/mem.cpp
    )

# Minimal badly written replacements for C++ stuff optimized for size
set(E4_STD_SOURCES
    include/e4std/array.h
    include/e4std/complicated.h
    include/e4std/free_fun.h
    include/e4std/map.h
    include/e4std/ptr.h
    include/e4std/sizes.h
    include/e4std/string.h
    include/e4std/stuff.h
    include/e4std/vector.h
    include/e4std/view.h
    src/e4std/stuff.cpp
    src/e4std/vector.cpp
    src/e4std/free_fun.cpp)

set(EMU_SOURCES
    src/main.cpp
    )

set(COMPILER_OPTIONS_REL
    -Os -g0 -DE4DEBUG=0)

set(COMPILER_OPTIONS_DEBUG
    -O0 -g -ggdb -DE4DEBUG=1 -fno-inline)

if (BUILD_RELEASE)
    add_library(e4_runtime ${E4_RUNTIME_SOURCES})
    add_library(e4_platform ${E4_PLATF_SOURCES})
    add_library(e4_std ${E4_STD_SOURCES})
    target_compile_options(e4_runtime PUBLIC ${COMPILER_OPTIONS_REL})
    target_compile_options(e4_platform PUBLIC ${COMPILER_OPTIONS_REL})
    target_compile_options(e4_std PUBLIC ${COMPILER_OPTIONS_REL})
endif (BUILD_RELEASE)

add_library(e4_runtime-debug ${E4_RUNTIME_SOURCES})
add_library(e4_platform-debug ${E4_PLATF_SOURCES})
add_library(e4_std-debug ${E4_STD_SOURCES})
target_compile_options(e4_runtime-debug PUBLIC ${COMPILER_OPTIONS_DEBUG})
target_compile_options(e4_platform-debug PUBLIC ${COMPILER_OPTIONS_DEBUG})
target_compile_options(e4_std-debug PUBLIC ${COMPILER_OPTIONS_DEBUG})

##
## Static Debug
##
add_executable(e4emu-debug ${EMU_SOURCES})
target_link_libraries(e4emu-debug
    e4_runtime-debug e4_platform-debug e4_std-debug ${MUSL_LIBS})
target_compile_options(e4emu-debug PUBLIC ${COMPILER_OPTIONS_DEBUG})

##
## Static Optimized Release
##
if (BUILD_RELEASE)
    add_executable(e4emu-x64 ${EMU_SOURCES})
    target_link_libraries(e4emu-x64 e4_runtime e4_platform e4_std ${MUSL_LIBS})
    target_compile_options(e4emu-x64 PUBLIC ${COMPILER_OPTIONS_REL})
endif (BUILD_RELEASE)

if (COMPILATION_INTERMEDIATE_RESULTS)
    if (BUILD_RELEASE)
        target_compile_options(e4_std PUBLIC -save-temps)
        target_compile_options(e4_platform PUBLIC -save-temps)
        target_compile_options(e4_runtime PUBLIC -save-temps)
    endif (BUILD_RELEASE)

    target_compile_options(e4_std-debug PUBLIC -save-temps)
    target_compile_options(e4_platform-debug PUBLIC -save-temps)
    target_compile_options(e4_runtime-debug PUBLIC -save-temps)

    target_compile_options(e4emu-debug PUBLIC -save-temps)
endif ()

##
## Link Flags
## -static-libgcc -static-libstdc++ -static -flto
##
set_target_properties(e4emu-debug
    PROPERTIES LINK_FLAGS "-flto -static-libgcc -static-libstdc++ -static")
if (BUILD_RELEASE)
    set_target_properties(e4emu-x64
        PROPERTIES LINK_FLAGS "-s -flto -static-libgcc -static-libstdc++ -static")
endif (BUILD_RELEASE)

##
## PVS-Studio Static Checker
##
include(PVS-Studio.cmake)
pvs_studio_add_target(
    TARGET analyze ALL
    OUTPUT FORMAT tasklist
    PREPROCESSOR gcc
    LOG "PVS-report.tasks"
    ANALYZE e4_runtime-debug e4_platform-debug e4emu-debug
    CXX_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
    C_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
    CONFIG "${CMAKE_SOURCE_DIR}/PVS-Studio.cfg"
)
