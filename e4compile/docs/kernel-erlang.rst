Kernel Erlang Intro
===================

Kernel Erlang is a final stage of Erlang compilation before BEAM assembly is
produced. Kernel Erlang is made from Core Erlang as a compilation stage,
and the code is located in the ``compiler`` app in ``v3_kernel.erl``.

Program structure
-----------------

You get ``#k_mdef{}`` as a root, which is a module definition with name,
exports, attributes and body (list of function definitions ``#k_fdef{}``).

Each function def contains a match ``#k_match{}``. These are compiled pattern
matches for incoming arguments.
A match contains some ``#k_alt{}`` inside, with nested ``#k_select{}`` one per
function clause. Select contains variable name and choices for each possible
expected type.

Select has type checks inside ``#k_type_clause{}`` with nested value checks
``#k_val_clause{}``.
