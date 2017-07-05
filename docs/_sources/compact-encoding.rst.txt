=============================
Compact Term Encoding in BEAM
=============================

Source:
http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html#beam-compact-term-encoding

This encoding is used by E4VM bytecode writer to encode things in the module
file. Also this is the same encoding, that is used in BEAM file format.

The idea is to stick as many type and value data in the 1st byte as possible::

    7 6 5 4 3 | 2 1 0
    ----------+------
             | 0 0 0 — Literal
             | 0 0 1 — Integer
             | 0 1 0 — Atom
             | 0 1 1 — X Register
             | 1 0 0 — Y Register
             | 1 0 1 — Label
             | 1 1 0 — Character
    0 0 0 1 0 | 1 1 1 — Extended — Float
    0 0 1 0 0 | 1 1 1 — Extended — List
    0 0 1 1 0 | 1 1 1 — Extended — Floating point register
    0 1 0 0 0 | 1 1 1 — Extended — Allocation list
    0 1 0 1 0 | 1 1 1 — Extended — Literal

It uses first 3 bits of a first byte as a tag to specify the type of the
following value. If the bits were all 1 (special value 7), then few more
bits are used.

For values under 16 the value is placed entirely into bits 4-5-6-7 having
bit 3 set to 0::

    7 6 5 4 | 3 | 2 1 0
    --------+---+------
    Value>> | 0 | Tag>>

For values under 16#800 (2048) bit 3 is set to 1, marks that 1 continuation
byte will be used and 3 most significant bits of the value will extend into
this byte’s bits 5-6-7::

    7 6 5 | 4 3 | 2 1 0
    ------+-----+------
    Value | 0 1 | Tag>>

Larger and negative values are first converted to bytes. Then if the value
takes 2..8 bytes, bits 3-4 will be set to 1, and bits 5-6-7 will contain
the (Bytes-2) size for the value, which follows::

    7  6  5 | 4 3 | 2 1 0
    --------+-----+------
    Bytes-2 | 1 1 | Tag>>

If the following value is greater than 8 bytes, then all bits 3-4-5-6-7
will be set to 1, followed by a nested encoded unsigned ?tag_u value of
(Bytes-9):8, and then the data::

    7 6 5 4 3 | 2 1 0
    ----------+------ Followed by nested encoded int (Size-9)
    1 1 1 1 1 | Tag>>

Refer to beam_asm:encode/2 in the compiler application for details about
how this is encoded. Tag values are presented in this section, but also can
be found in compiler/src/beam_opcodes.hrl.
