Modified J1 Forth
=================

Erlang-Forth (E4) is a J1-like Forth modified to support Erlang code semantics,
literals and other features.

Stack Frames
------------

Stack frame support was added to simplify variable allocation on stack, similar
to how x86 assembly does it. Upon entering a function a base pointer BP is
stored in a special register and is used to address both function arguments
(BP+i) and variables (BP-i with negative offsets).

Creating and extending a frame is done with .ENTER Forth word, leaving a frame
or reducing its size is done with .LEAVE word.

.. todo:
    Entering a function frame is different from extending it. Same with
    leaving vs shrinking. Ensure that this works correctly.

Forth Words
-----------

``:`` (colon) defines a function. Current address in code is recorded along with
the name in the word dictionary.

Special Keywords
----------------

``:NIF Name -Index`` registers an internal function with negative index and the
name. Does not do any other action than registering the name with the compiler.
The implementation is trusted to the virtual machine.

``:MODULE Name`` defines current module name.

.. todo:
    During compilation it is now not allowed to switch module names to define
    multiple modules.
