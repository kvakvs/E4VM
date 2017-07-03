MicroErlang
===========

This project is a cross-compiler from Erlang to a handcrafted compressed
bytecode format.
Also includes the Runtime, a virtual machine written in C++ which will run
this bytecode.
It is designed and built with embedded hardware in mind.

Building on PC
--------------

The project uses CMake and I recommend out-of-source build (see steps below).
Makefiles have extra targets to run demo scenarios or debugger so have a
look there too.

* Create a build directory, for example ``mkdir build_``.
* Enter build directory: ``cd build_``.
* Run CMake on it: ``cmake ..``
* `make`

Contributions
-------------

At this time contributions are **not** welcome.
The project is highly experimental and nobody knows what may change tomorrow.
Sorry.
