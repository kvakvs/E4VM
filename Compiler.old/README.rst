MicroErlang Transpiler
======================

A simple transpiler, which picks up output from one late BEAM compiler stage
(namely: the BEAM assembly output).
It is then converted into simplified bytecode for the MicroErlang
virtual machine.
The code is compressed with a mix of Huffman codes and bit field magic
and is designed to be executed without decompression.

Trying it out
-------------

    $ make run1
    $ make run2
    $ make run3

This will compile included example modules from the `priv/` dir producing
`*.uerl` output files.
