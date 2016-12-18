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
