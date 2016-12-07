3eamc BEAM to Forth compiler
============================

A simple cross-compiler, converts your Erlang source to Core Erlang and then
produces a Forth program which is then compiled into Forth bytecode.

Build
-----

    $ make test

Forth words standard and custom
-------------------------------

*   `R:A RET` - pops `A` from return stack and jumps to `A`
*   `R:A n RETN` - trims `N` items from value stack, pops `A` from return stack
    and jumps to `A`
*   `NIL` - pushes NIL literal onto value stack
