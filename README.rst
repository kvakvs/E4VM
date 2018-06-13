Note
====

This project is not moving forward (as you probably noticed) and ends are
not yet properly connected. I.e. the precompiler generates compressed bitcode
but the runtime does not handle it yet.

The general idea was to create a portable bit-compressed bytecode format and a
tiny runtime for embedded systems.

Here's my talk on EUC 2017 (Stockholm) explaining the history of this project
and why it is here https://www.youtube.com/watch?v=6NUPormxgw8

MicroErlang
===========

For documentation and getting started please refer to http://uerlang.org

Building Documentation
----------------------

Having Sphinx installed run ``make docs`` in the root directory of the project.
It will populate ``docs/`` with the new copy of the documentation and website
content.
