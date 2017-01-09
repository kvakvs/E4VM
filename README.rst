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

See also: ``e4compiler/README.rst``

The ultimate goal is to land on embedded devices (something like RTEMS?)
but development naturally happens on x64 Linux.

Building
--------

**Optional step** (controlled by CMakeLists option CUSTOM_STDLIB, default off)

Build and install musl: enter musl directory: `cd musl`, do `./configure`
and then `sudo make install` this will build the library and output all the
include and lib files into `/usr/local/musl/*` where the CMakeLists script
expects them.

Create a build directory, for example `mkdir build_`. Enter build directory:
`cd build_`. Run CMake on it: `cmake ..` and then `make`.
