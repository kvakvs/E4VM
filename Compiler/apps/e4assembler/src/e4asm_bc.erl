%%% @doc Bytecode generation library for E4 VM
-module(e4asm_bc).

%% API
-export([func_info/3, call/2]).

-define(E4BC_FUNC_INFO, 1).
-define(E4BC_CALL_LOCAL, 2).  % 4 local call flavours
-define(E4BC_CALL_EXT, 6).    % 4 ext call flavours

bc_op(X) -> <<X:8>>.
%%bc_op(X, F1) -> <<(X bor bit_if(F1, 128)):8>>.
bc_op(X, F1, F2) -> <<(X bor bit_if(F1, 128) bor bit_if(F2, 64)):8>>.


bit_if(true, X) -> X;
bit_if(false, _X) -> 0.


atom_index(Atoms, A) when is_atom(A) ->
  orddict:fetch(A, Atoms).


func_info(#{'$' := e4mod, atoms := Atoms}, F, Arity) ->
  [bc_op(?E4BC_FUNC_INFO),
   e4c:varint(atom_index(Atoms, F)),
   e4c:varint(Arity)].


call(#{'$' := e4mod, atoms := Atoms},
     #{'$' := e4call, arity := A, tailcall := Tail,
       target := Target, dealloc := Dealloc}) ->
  case Target of
    {f, TargetLabel} ->
      [bc_op(?E4BC_CALL_LOCAL, Tail, Dealloc =/= 0), e4c:varint(TargetLabel)];
    {extfunc, M, F, Arity} when Arity =:= A ->
      [bc_op(?E4BC_CALL_EXT, Tail, Dealloc =/= 0),
       e4c:varint(atom_index(Atoms, M)),
       e4c:varint(atom_index(Atoms, F))]
  end.
