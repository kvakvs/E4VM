cmake_minimum_required(VERSION 3.5)
project(E4VM)

set(CMAKE_CXX_STANDARD 14)

set(CMAKE_C_COMPILER    /usr/bin/clang)
set(CMAKE_CXX_COMPILER  /usr/bin/clang++)

set(COMPILATION_INTERMEDIATE_RESULTS    0)

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
    src/e4rt/scheduler.cpp include/e4rt/scheduler.h)

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
    src/e4std/free_fun.cpp
    src/e4std/stuff.cpp
    src/e4std/vector.cpp
    )

set(EMU_SOURCES
    src/main.cpp
    )

if(CMAKE_BUILD_TYPE MATCHES Debug)
    add_definitions(-O0 -g -ggdb -DE4DEBUG=1 -fno-inline)
else()
    add_definitions(-Os -g0 -DE4DEBUG=0)
endif()


add_library(e4_runtime  ${E4_RUNTIME_SOURCES})
add_library(e4_platform ${E4_PLATF_SOURCES})
add_library(e4_std      ${E4_STD_SOURCES})
#target_compile_options(e4_runtime   PUBLIC ${COMPILER_OPTIONS_DEBUG})
#target_compile_options(e4_platform  PUBLIC ${COMPILER_OPTIONS_DEBUG})
#target_compile_options(e4_std       PUBLIC ${COMPILER_OPTIONS_DEBUG})

add_executable(e4emu ${EMU_SOURCES})
target_link_libraries(e4emu
    e4_runtime e4_platform e4_std)
#target_compile_options(e4emu PUBLIC ${COMPILER_OPTIONS_DEBUG})

if (COMPILATION_INTERMEDIATE_RESULTS)
    target_compile_options(e4_std       PUBLIC -save-temps)
    target_compile_options(e4_platform  PUBLIC -save-temps)
    target_compile_options(e4_runtime   PUBLIC -save-temps)

    target_compile_options(e4emu-debug  PUBLIC -save-temps)
endif()

## -static-libgcc -static-libstdc++ -static -flto
if(CMAKE_BUILD_TYPE MATCHES Debug)
    set_target_properties(e4emu
        PROPERTIES LINK_FLAGS "-flto -static")
else()
    set_target_properties(e4emu
        PROPERTIES LINK_FLAGS "-flto -static")
endif()

##
## PVS-Studio Static Checker
##
#include(PVS-Studio.cmake)
#pvs_studio_add_target(
#    TARGET analyze ALL
#    OUTPUT FORMAT tasklist
#    PREPROCESSOR gcc
#    LOG "PVS-report.tasks"
#    ANALYZE e4_runtime-debug e4_platform-debug e4emu-debug
#    CXX_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
#    C_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
#    CONFIG "${CMAKE_SOURCE_DIR}/PVS-Studio.cfg"
#)
