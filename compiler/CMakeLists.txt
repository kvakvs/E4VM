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
    -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS
    -D__STDC_LIMIT_MACROS
    )
#    -fno-rtti
#    -fno-exceptions

set(YY_DIR ${CMAKE_SOURCE_DIR}/src/yy/)

set(SOURCE_FILES
    bison/erlang.ll
    bison/erlang.yy
    ${YY_DIR}/erlang_lexer.cpp
    ${YY_DIR}/erlang_parser.cpp
    ${YY_DIR}/erlang_parser.hpp
#    ${YY_DIR}/location.hh
#    ${YY_DIR}/position.hh
    src/erl_ast.h
    src/erl_codegen.cpp
    src/erl_codegen.h
#    src/erl_jit.h
    src/erl_scanner.cpp
    src/erl_scanner.cpp
    src/erl_parse.cpp
    src/erl_parse.h
    src/main.cpp
    src/erl_yy_driver.h src/erl_yy_driver.cpp)

##
## Flex/Bison definitions
##
find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
BISON_TARGET(MyErlangParser bison/erlang.yy ${YY_DIR}/erlang_parser.cpp
    COMPILE_FLAGS "-L c++")
flex_target(MyErlangLexer bison/erlang.ll ${YY_DIR}/erlang_lexer.cpp)
ADD_FLEX_BISON_DEPENDENCY(MyErlangLexer MyErlangParser)
#include_directories(${YY_DIR})
##
## End Flex/Bison
##

add_executable(compiler
    ${SOURCE_FILES}
    ${BISON_MyErlangParser_OUTPUTS}
    ${FLEX_MyErlangLexer_OUTPUTS}
    )

set(MY_LLVM_LIBS
    LLVMX86Disassembler LLVMX86AsmParser LLVMX86CodeGen LLVMGlobalISel
    LLVMSelectionDAG LLVMAsmPrinter LLVMDebugInfoCodeView LLVMDebugInfoMSF
    LLVMCodeGen LLVMScalarOpts LLVMInstCombine LLVMTransformUtils
    LLVMBitWriter LLVMX86Desc LLVMMCDisassembler LLVMX86Info LLVMX86AsmPrinter
    LLVMX86Utils
#    LLVMMCJIT
    LLVMExecutionEngine LLVMTarget LLVMAnalysis
    LLVMProfileData LLVMRuntimeDyld LLVMObject LLVMMCParser LLVMBitReader
    LLVMMC LLVMCore LLVMSupport LLVMDemangle
    )

# Link against LLVM libraries
target_link_libraries(compiler ${MY_LLVM_LIBS})
