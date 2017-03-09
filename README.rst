E4VM Project
============

This is another attempt to approach the problem of running Erlang with smaller
resource requirements, targeting small devices. There have been several attempts:

-   Gluonvm1 project was the first attempt to read and execute regular BEAM file
    format (which sort of worked)
-   E4VM was named after Erlang-Forth, and the idea was to transpile Kernel Erlang
    to Forth and then to Forth bytecode (which almost worked, so is a valid way
    to get this done)
-   Recent idea then appeared to transpile Kernel Erlang to C++ (started shaping up
    but then was dropped).

Current and latest idea is to parse Erlang sources directly and produce LLVM IR which
then is linked with custom Erlang Runtime (part of this project too) and then LLVM
will produce either firmware blob for an embedded system or a Linux executable.

Compiler
--------

Includes a simple Erlang compiler, which parses your Erlang files (preprocessed by 
``erlc`` first) and produces an LLVM module.

Building on PC
--------------

The project parts are built using CMake (both runtime library and the compiler).

- Create a build directory, for example ``mkdir build_``
- Enter build directory: ``cd build_``
- Run CMake on it: ``cmake ..`` to create ``Makefile``
- Run ``make``.
