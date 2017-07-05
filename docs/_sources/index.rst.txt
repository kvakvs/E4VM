μErlang — MicroErlang
=====================

This project contains the ``Compiler``, an cross-compiler application which
converts your Erlang source to compressed bytecode format which runs on the
target hardware without decompression (``.uerl`` files).

``Runtime`` is a virtual machine written in C++ that loads the generated
bytecode on the target hardware and executes it.

Github Link: https://github.com/kvakvs/E4VM
(will be renamed to ``MicroErlang`` later).

.. toctree::
    :maxdepth: 1
    :caption: Documentation Topics:

    getting-started
    contributing

.. toctree::
    :maxdepth: 1
    :caption: Extra Topics:

    compact-encoding

..  Indices and tables
    ==================
    * :ref:`genindex`
    * :ref:`modindex`
    * :ref:`search`
