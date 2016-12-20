Gluon VM
========

Virtual machine written in C++ to run custom flavour of Forth. Your program
is compiled from Erlang via Kernel Erlang to Forth.

Cross-compiler
--------------

Includes a simple cross-compiler, which converts your Erlang source to Kernel Erlang
and then produces a Forth program internally which is then compiled into Forth
bytecode and written to disk as ``filename.4bin``.

See also: ``e4compiler/README.rst``

The ultimate goal is to land on embedded devices (something like RTEMS?)
but development naturally happens on x64 Linux.
