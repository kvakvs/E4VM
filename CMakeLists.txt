cmake_minimum_required(VERSION 3.5)
project(GluonVM)

set(CMAKE_CXX_STANDARD 14)

include_directories(${CMAKE_SOURCE_DIR}/include)
add_definitions(
    # enables filesystem feature: files, directories, code loading from disk etc
    -DGLUON_FEATURE_FS=1
    -DGLUON_DEBUG=1
    -std=c++14
)

set(GLUON_SOURCES
    include/gluon.h
    include/gluon/byte_stream_reader.h
    include/gluon/code_mgr.h
    include/gluon/conf.h
    include/gluon/debug.h
    include/gluon/fs.h
    include/gluon/module.h
    include/gluon/types.h
    include/gluon/vm.h
    src/code_mgr.cpp
    src/debug.cpp
    src/main.cpp
    src/module.cpp
        )

add_executable(gleam_d ${GLUON_SOURCES})
target_compile_options(gleam_d
        PUBLIC -O0 -ggdb
        )
add_executable(gleam_opt_sz ${GLUON_SOURCES})
target_compile_options(gleam_opt_sz
        PUBLIC -Os
        )