Gluon VM
========

Cross-compiler tool which produces compact bytecode from BEAM assembly and C++
virtual machine to load and run this code. The project is intended to land on
embedded devices or something like RTEMS (to be decided) but development naturally
happens on x64 Linux.

New 3EAM bytecode format description is here: doc/3eam-format.rst.

Emphasis is made to preprocess and
precalculate as much as possible and simplify further loading and
format interpretation, and to be able to execute straight from the loaded
file image. This should allow to reduce loader and interpreter/VM loop code.
