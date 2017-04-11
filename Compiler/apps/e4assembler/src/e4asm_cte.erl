%%% @doc BEAM Compact Term encoding, also used in Ericsson's BEAM files
%%% @end

-module(e4asm_cte).

%% API
-export([encode/2]).

-include_lib("compiler/src/beam_opcodes.hrl").

%% The idea is to stick as many type and value data in the 1st byte as possible
%% 7 6 5 4 3 | 2 1 0
%% ----------+------
%%           | 0 0 0 — Literal
%%           | 0 0 1 — Integer
%%           | 0 1 0 — Atom
%%           | 0 1 1 — X Register
%%           | 1 0 0 — Y Register
%%           | 1 0 1 — Label
%%           | 1 1 0 — Character
%% 0 0 0 1 0 | 1 1 1 — Extended — Float
%% 0 0 1 0 0 | 1 1 1 — Extended — List
%% 0 0 1 1 0 | 1 1 1 — Extended — Floating point register
%% 0 1 0 0 0 | 1 1 1 — Extended — Allocation list
%% 0 1 0 1 0 | 1 1 1 — Extended — Literal

%% Bit 3 is set to mark a longer continuation (beyond 8 bits) and bits 5 6 7
%% will contain the 3 most significant bits of the value, rest will be in the
%% following byte.

encode(_Mod, {x, X}) -> beam_asm:encode(?tag_x, X);
encode(_Mod, {y, Y}) -> beam_asm:encode(?tag_y, Y);
encode(#{'$' := e4mod} = Mod, Atom) ->
  e4c:varint(atom_index(Mod, Atom)).

atom_index(#{'$' := e4mod, atoms := Atoms}, A) when is_atom(A) ->
  orddict:fetch(A, Atoms).
