Erlang-to-Forth (E4) compiler
=============================

A simple cross-compiler, converts your Erlang source to Kernel Erlang and then
produces a Forth program internally which is then compiled into Forth bytecode.

Forth dialect is based on J1Forth but with added Erlang types and extra opcodes.

Trying it out
-------------

    $ make run
