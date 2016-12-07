Gluon VM
========

Virtual machine written in C++ to run custom flavour of Forth. Your program
is compiled from Core Erlang to Forth.

Includes a simple cross-compiler, which converts your Erlang source to Core Erlang
and then produces a Forth program internally which is then compiled into Forth
bytecode and written to disk.

See also: 3eamc/README.rst

The project is intended to land on embedded devices or something like RTEMS
(to be decided) but development naturally happens on x64 Linux.
