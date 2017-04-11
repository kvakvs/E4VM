%%% @doc Bytecode generation library for E4 VM
-module(e4asm_bc).

%% API
-export([func_info/3, call/2, bif/2, allocate/3, get_element/3]).

-define(E4BC_FUNC_INFO, 0).
-define(E4BC_CALL_LOCAL, 1).  % 4 local call flavours defined by bits 6 and 7
-define(E4BC_CALL_EXT, 2).    % 4 ext call flavours defined by bits 6 and 7
-define(E4BC_BIF, 3).
-define(E4BC_ALLOC_S, 4).
-define(E4BC_ALLOC_S_H, 5).
-define(E4BC_GET_ELEMENT, 6).

bc_op(X) -> <<X:8>>.
%%bc_op(X, F1) -> <<(X bor bit_if(F1, 128)):8>>.
bc_op(X, F1, F2) -> <<(X bor bit_if(F1, 128)
                         bor bit_if(F2, 64)):8>>.
bc_op(X, F1, F2, F3) -> <<(X bor bit_if(F1, 128)
                             bor bit_if(F2, 64)
                             bor bit_if(F3, 32)):8>>.


bit_if(true, X) -> X;
bit_if(false, _X) -> 0.


func_info(#{'$' := e4mod, atoms := Atoms}, F, Arity) ->
  [bc_op(?E4BC_FUNC_INFO),
   e4c:varint(atom_index(Atoms, F)),
   e4c:varint(Arity)].


call(#{'$' := e4mod} = Module,
     #{'$' := e4call, arity := A, tailcall := Tail,
       target := Target, dealloc := Dealloc}) ->
  %% TODO: Dealloc
  case Target of
    {f, TargetLabel} ->
      [bc_op(?E4BC_CALL_LOCAL, Tail, Dealloc =/= 0), e4c:varint(TargetLabel)];
    {extfunc, M, F, Arity} when Arity =:= A ->
      [bc_op(?E4BC_CALL_EXT, Tail, Dealloc =/= 0),
       encode(Module, M),
       e4c:varint(atom_index(Atoms, F))]
  end.

bif(#{'$' := e4mod, atoms := Atoms},
    Bif = #{'$' := e4bif, args := Args, name := Name}) ->
  %% TODO: Args
  %% TODO: Gc
  Gc = maps:get(gc, Bif, 0),
  %% TODO: Fail
  Fail = maps:get(fail, Bif, ignore),
  %% TODO: Result
  Result = maps:get(result, Bif, ignore),
  [bc_op(?E4BC_BIF, Fail =/= ignore, Gc =/= 0, Result =/= 0),
   e4c:varint(atom_index(Atoms, Name))].

allocate(StackNeed, 0, Live) ->
  [bc_op(?E4BC_ALLOC_S),
   e4c:varint(StackNeed),
   e4c:varint(Live)];
allocate(StackNeed, HeapNeed, Live) ->
  [bc_op(?E4BC_ALLOC_S_H),
   e4c:varint(StackNeed),
   e4c:varint(HeapNeed),
   e4c:varint(Live)].

get_element(Tuple, Index, Result) ->
  %% TODO: Result
  [bc_op(?E4BC_GET_ELEMENT),
   encode(Tuple),
   e4c:varint(Index),
   encode(Result)].
