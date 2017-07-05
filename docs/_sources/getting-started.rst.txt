Getting Started
===============

Compiler
--------

The **Compiler** project has some test files that you can run to see what
happens. Try ``make run1``, ``make run2``, and ``make run3``. The output goes
to the ``priv/`` directory.

Runtime
-------

The **Runtime** project uses CMake and out-of-source build (see steps below).
Makefiles may have extra targets to run demo scenarios or debugger so have a
look there too.

* Create a build directory with any name, for example ``mkdir _build``.
* ``cd _build``.
* Run CMake to generate the Makefile ``cmake ..``
* ``make``
