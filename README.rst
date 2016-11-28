Gluon VM
========

Virtual machine written in C++ to run custom flavour of Erlang byte-code.
Includes a cross-compiler which produces said bytecode from BEAM assembly (``+S`).
The project is intended to land on embedded devices or something like RTEMS
(to be decided) but development naturally happens on x64 Linux.

New 3EAM bytecode format description is here: doc/3eam-format.rst.

This format is somewhat similar but also different from the original
Ericsson's Erlang/OTP BEAM format.
Emphasis is to preprocess and simplify the loading as much as possible.
