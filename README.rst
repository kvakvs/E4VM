E4 (Erlang-Forth) VM
====================

This is a virtual machine written in C++ to run custom flavour of Forth (J1).
Forth is modified to support Erlang data types, exceptions, stack frames,
processes and whatnot.

Cross-compiler
--------------

Includes a simple cross-compiler, which converts your Erlang source to Kernel
Erlang and then produces a Forth program internally which is then compiled into
J1 Forth bytecode and written to disk as ``filename.e4b``.

See also: ``Compiler/README.rst``

The ultimate goal is to land on embedded devices (something like RTEMS?)
but development naturally happens on x64 Linux.

Building on PC
--------------

Create a build directory, for example `mkdir build_`. Enter build directory:
`cd build_`. Run CMake on it: `cmake ..` and then `make`.
