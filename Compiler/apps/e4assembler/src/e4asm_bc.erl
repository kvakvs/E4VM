%%% @doc Bytecode generation library for E4 VM
-module(e4asm_bc).

%% API
-export([func_info/3]).

-define(E4BC_FUNC_INFO, 1).

bc_op(X) -> <<X:8>>.

atom_index(Atoms, A) when is_atom(A) ->
  orddict:fetch(A, Atoms).

func_info(#{'$' := e4mod, atoms := Atoms}, F, Arity) ->
  [bc_op(?E4BC_FUNC_INFO),
   e4c:varint(atom_index(Atoms, F)),
   e4c:varint(Arity)].

