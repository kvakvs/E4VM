Compiler
========

Cross-compiler tool which takes .S beam assembly files and writes out
compressed binary MicroErlang modules.

Compiling
---------

*   ``sudo apt install ghc haskell-stack``
*   ``make compile`` or ``make run``

For comfortable editing:

*   IntelliJ IDEA + IntelliJ-Haskell plugin and configure path to stack in
    the IDEA SDK options
*   ``stack install hindent`` and ``stack install stylish-haskell`` and
    configure them in IDEA Options

With these features configured, IDEA becomes quite Haskell-friendly.
