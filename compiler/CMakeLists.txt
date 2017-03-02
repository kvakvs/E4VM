cmake_minimum_required(VERSION 3.5)
project(compiler)

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
    -std=c++14 -g
    -fno-exceptions -fno-rtti
    -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS
    -D__STDC_LIMIT_MACROS)

set(SOURCE_FILES
    src/erl_ast.h
    src/erl_codegen.cpp
    src/erl_codegen.h
    src/erl_jit.h
    src/erl_lexer.cpp
    src/erl_lexer.h
    src/erl_parse.cpp
    src/erl_parse.h
    src/main.cpp
    )
add_executable(compiler ${SOURCE_FILES})

set(MY_LLVM_LIBS
    LLVMX86Disassembler LLVMX86AsmParser LLVMX86CodeGen LLVMGlobalISel
    LLVMSelectionDAG LLVMAsmPrinter LLVMDebugInfoCodeView LLVMDebugInfoMSF
    LLVMCodeGen LLVMScalarOpts LLVMInstCombine LLVMTransformUtils
    LLVMBitWriter LLVMX86Desc LLVMMCDisassembler LLVMX86Info LLVMX86AsmPrinter
    LLVMX86Utils LLVMMCJIT LLVMExecutionEngine LLVMTarget LLVMAnalysis
    LLVMProfileData LLVMRuntimeDyld LLVMObject LLVMMCParser LLVMBitReader
    LLVMMC LLVMCore LLVMSupport LLVMDemangle
    )

# Link against LLVM libraries
target_link_libraries(compiler ${MY_LLVM_LIBS})
