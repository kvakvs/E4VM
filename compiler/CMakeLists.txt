cmake_minimum_required(VERSION 3.5)
project(compiler CXX)

add_subdirectory(antlr4-runtime)
set(CMAKE_CXX_STANDARD 14)

#set(LLVM_CMAKE_DIR "/usr/local/lib/cmake/llvm")
#set(LLVM_INCLUDE_DIRS "/usr/local/include")
#set(LLVM_LIBRARY_DIRS "/usr/local/lib")
#set(LLVM_TOOLS_BINARY_DIR "/usr/local/bin")
#set(LLVM_TARGETS_TO_BUILD "x86")
#
#include(${LLVM_CMAKE_DIR}/LLVMExports.cmake)
#include(${LLVM_CMAKE_DIR}/LLVM-Config.cmake)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
add_definitions(-fPIC -fvisibility-inlines-hidden -Wall -W
    -Wno-unused-parameter -Wwrite-strings -Wcast-qual
    -Wmissing-field-initializers -pedantic -Wno-long-long
    -Wcovered-switch-default -Wnon-virtual-dtor -Wdelete-non-virtual-dtor
    -Wstring-conversion -Werror=date-time
    -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS
    -D__STDC_LIMIT_MACROS
    -std=c++14
    -g -ggdb
    )
#    -fno-rtti
#    -fno-exceptions

include_directories(antlr4-runtime/runtime/src)
set(ANTLR4_OUTPUTS
    src/antlr4/ErlangBaseListener.cpp
    src/antlr4/ErlangBaseListener.h
    src/antlr4/ErlangBaseVisitor.cpp
    src/antlr4/ErlangBaseVisitor.h
    src/antlr4/ErlangLexer.cpp
    src/antlr4/ErlangLexer.h
    src/antlr4/ErlangListener.cpp
    src/antlr4/ErlangListener.h
    src/antlr4/ErlangParser.cpp
    src/antlr4/ErlangParser.h
    src/antlr4/ErlangVisitor.cpp
    src/antlr4/ErlangVisitor.h
    )

# to update Antlr4 code, run make in antlr4-erlang
set(SOURCE_FILES
    ${ANTLR4_OUTPUTS}
    antlr4-erlang/Erlang.g4
    src/ast.h
    src/compiler.cpp
    src/compiler.h
    src/main.cpp
    src/ast.cpp
    src/ast_builder.h src/ast_builder.cpp src/codegen.h src/codegen.cpp src/compile_error.h)

add_executable(compiler
    ${SOURCE_FILES}
    )

set(MY_LLVM_LIBS
    #LLVMMCJIT
    LLVMAnalysis
    LLVMAsmPrinter
    #LLVMBitReader
    #LLVMBitWriter
    LLVMCodeGen
    LLVMCore
    #LLVMDebugInfoCodeView
    #LLVMDebugInfoMSF
    LLVMDemangle
    LLVMExecutionEngine
    #LLVMGlobalISel
    #LLVMInstCombine
    #LLVMMC
    #LLVMMCDisassembler
    #LLVMMCParser
    #LLVMObject
    #LLVMProfileData
    #LLVMRuntimeDyld
    #LLVMScalarOpts
    #LLVMSelectionDAG
    LLVMSupport
    LLVMTarget
    LLVMTransformUtils
    LLVMX86AsmParser
    LLVMX86AsmPrinter
    LLVMX86CodeGen
    LLVMX86Desc
    LLVMX86Disassembler
    LLVMX86Info
    LLVMX86Utils
    )

# Link against LLVM libraries
target_link_libraries(compiler
    ${MY_LLVM_LIBS}
    )
target_link_libraries(compiler antlr4_static)
