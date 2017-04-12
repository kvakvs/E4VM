%%% @doc BEAM Compact Term encoding, also used in Ericsson's BEAM files
%%% @end

-module(e4asm_cte).

%% API
-export([encode/2]).

-include_lib("compiler/src/beam_opcodes.hrl").

%% @doc See docs/compact-encoding.rst
%% The logic is based on beam_asm:encode_arg in the compiler app
encode(_Mod, {x, X}) -> beam_asm:encode(?tag_x, X);
encode(_Mod, {y, Y}) -> beam_asm:encode(?tag_y, Y);
encode(_Mod, nil) -> beam_asm:encode(?tag_a, 0);
encode(_Mod, []) -> beam_asm:encode(?tag_a, 0);
encode(Mod = #{'$' := e4mod}, {atom, Atom}) ->
  %% Assume atom already exists, will crash if it doesn't
  beam_asm:encode(?tag_a, atom_index(Mod, Atom) + 1);
encode(_Mod, X) when is_integer(X) -> beam_asm:encode(?tag_u, X).

atom_index(#{'$' := e4mod, atoms := Atoms}, A) when is_atom(A) ->
  orddict:fetch(A, Atoms).
