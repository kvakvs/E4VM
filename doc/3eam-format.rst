File Format for 3EAM files
==========================

This format is somewhat similar but also different from the original
Ericsson's Erlang/OTP BEAM format.
Emphasis is made to preprocess and precalculate as much as possible and
simplify further loading and format interpretation, and to be able to
execute straight from the loaded file image.

3EAM are rewritten BEAM files with some processing done on them.
They have reduced instruction set.
Encoding of data is simplified to varint (UTF-8 algorithm where eldest bit).

File header
-----------

File begins with initial marker "3EAM".
It is followed by section markers in no particular order. They can be:

*   "Expt", exports table;
*   "Atom", atom table;
*   "Code", code section.

Section marker is followed by varint byte size.

Exports section
---------------

Section contains a list of pairs [{varint function_name, varint arity}].
Function name is encoded as an index in the atom table.

Atom section
------------

Atom table contains all atoms collected from the module in form of strings.

After the section header "Atom" and varint section_size goes varint atom_count.
Each string is encoded as [varint size, utf8 characters] for each atom,
beginning with the index 0.
Atom at position 0 is always the module name.

Code section
------------

Code is encoded directly from BEAM assembly output by the Erlang compiler
with some simplifications compared to original BEAM files.
Some data is omitted.
Numbers are simplified.
This is done to reduce the loader and decoder size.

Code contains list of functions. Each function begins with ?MARK_FUNCTION=255,
followed by varint atom_name, varint arity, varint operator_count and
then list of operators.

Code end is marked by ?MARK_CODE_END=254.

See gcompile module for opcodes. Arguments are encoded as tagged values, followed
by the value itself:

*   ?VAL_X=1 denotes an X register, followed by varint x_register_index
*   ?VAL_Y=2 denotes an Y stack slot, followed by varint y_index
*   ?VAL_ATOM=3 followed by varint atom index