cmake_minimum_required(VERSION 3.5)
project(gluonvm2)

set(CMAKE_CXX_STANDARD 14)

include_directories(${CMAKE_SOURCE_DIR}/include)
add_definitions(
        -DGLUON_FEATURE_FS=1
        -DGLUON_DEBUG=1
        -DGLUON_WORD_SIZE=64
        -std=c++17
        -fno-rtti -fno-unroll-loops
        -Weverything -Wall -Werror
        -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-gnu-anonymous-struct
        -Wno-zero-length-array -Wno-nested-anon-types
        -Wno-format-nonliteral -Wno-global-constructors
)

## Erlang Runtime system for GluonVM: types, bifs, memory etc
set(GLUON_ERTS_SOURCES
        include/g_erts/code_mgr.h
        include/g_erts/dist.h
        include/g_erts/ext_term_format.h
        include/g_erts/heap.h
        include/g_erts/module.h
        include/g_erts/term.h
        include/g_erts/term_layout.h
        include/g_erts/vm.h
        include/gluon.h
        src/g_erts/code_mgr.cpp
        src/g_erts/ext_term_format.cpp
        src/g_erts/module.cpp
        src/g_erts/vm.cpp
        src/g_erts/term.cpp include/g_erts/box.h src/g_erts/binary.cpp include/g_erts/binary.h)

## Helpers and OS abstraction library
set(GLUON_PLATF_SOURCES
        include/g_platform/byte_stream_reader.h
        include/g_platform/conf.h
        include/g_platform/debug.h
        include/g_platform/fs.h
        include/g_platform/interned_string.h
        include/g_platform/mem.h
        include/g_platform/types.h
        src/g_platform/debug.cpp
        src/g_platform/interned_string.cpp
        src/g_platform/mem.cpp
        )

set(GLUON_SOURCES
        src/main.cpp
        )

add_library(gluon_erts ${GLUON_ERTS_SOURCES})
add_library(gluon_platform ${GLUON_PLATF_SOURCES})

add_executable(gleam_d ${GLUON_SOURCES})
target_link_libraries(gleam_d gluon_erts gluon_platform)
target_compile_options(gleam_d
        PUBLIC -O0 -g -ggdb
        )

add_executable(gleam_opt ${GLUON_SOURCES})
target_link_libraries(gleam_opt gluon_erts gluon_platform)
target_compile_options(gleam_opt
        PUBLIC -Oz -g0
        )
# Static linking + lto (link time opt) -static-libgcc -static-libstdc++ -static -flto
# llvm-ld  -strip-all
set_target_properties(gleam_opt gleam_d
        PROPERTIES LINK_FLAGS "-flto")
set_target_properties(gleam_opt
        PROPERTIES LINK_FLAGS "-s -flto")

include(PVS-Studio.cmake)
pvs_studio_add_target(
        TARGET analyze ALL
        OUTPUT FORMAT tasklist
        PREPROCESSOR gcc
        LOG "PVS-report.tasks"
        ANALYZE gluon_erts gluon_platform gleam_d
        CXX_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
        C_FLAGS ${PREPROCESSOR_ADDITIONAL_FLAGS}
        CONFIG "${CMAKE_SOURCE_DIR}/PVS-Studio.cfg"
        )
