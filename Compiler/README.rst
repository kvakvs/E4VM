Erlang BEAM post-processor
==========================

A simple post-compiler, which hooks after BEAM compiler has finished working and
takes produced BEAM assembly. It converts then BEAM assembly into simplified
bytecode for E4 virtual machine and writes it to disk.

Trying it out
-------------

    $ make run1; make run2; make run3

will run included example modules from priv/ dir through the compiler
