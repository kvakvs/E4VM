%%% @doc Bytecode generation library for E4 VM
-module(e4asm_bc).

%% API
-export([func_info/3, call/2, bif/2, allocate/3, get_element/3, move/3,
         call_fun/2, set_nil/2, test_heap/3, put_tuple/3, put/2]).

-define(E4BC_FUNC_INFO,   0).
-define(E4BC_CALL_LOCAL,  1). % 4 local call flavours defined by bits 6 and 7
-define(E4BC_CALL_EXT,    2). % 4   ext call flavours defined by bits 6 and 7
-define(E4BC_BIF,         3).
-define(E4BC_ALLOC_S,     4).
-define(E4BC_ALLOC_S_H,   5).
-define(E4BC_GET_ELEMENT, 6).
-define(E4BC_MOVE,        7).
-define(E4BC_CALL_FUN,    8).
-define(E4BC_SET_NIL,     9).
-define(E4BC_TEST_HEAP,   10).
-define(E4BC_PUT_TUPLE,   11).
-define(E4BC_PUT,         12).

bc_op(X) -> <<X:8>>.
%%bc_op(X, F1) -> <<(X bor bit_if(F1, 128)):8>>.
bc_op(X, F1, F2) -> <<(X bor bit_if(F1, 128)
                         bor bit_if(F2, 64)):8>>.
bc_op(X, F1, F2, F3) -> <<(X bor bit_if(F1, 128)
                             bor bit_if(F2, 64)
                             bor bit_if(F3, 32)):8>>.


bit_if(true, X) -> X;
bit_if(false, _X) -> 0.


func_info(Mod = #{'$' := e4mod}, F, Arity) ->
  [bc_op(?E4BC_FUNC_INFO),
   e4asm_cte:encode(Mod, {atom, F}),
   e4asm_cte:encode(Mod, Arity)].


test_heap(Mod = #{'$' := e4mod}, Need, Live) ->
  [bc_op(?E4BC_TEST_HEAP),
   e4asm_cte:encode(Mod, Need),
   e4asm_cte:encode(Mod, Live)].


put_tuple(Mod = #{'$' := e4mod}, Size, Dst) ->
  [bc_op(?E4BC_PUT_TUPLE),
   e4asm_cte:encode(Mod, Size),
   e4asm_cte:encode(Mod, Dst)].


put(Mod = #{'$' := e4mod}, Val) ->
  [bc_op(?E4BC_PUT),
   e4asm_cte:encode(Mod, Val)].


move(Mod = #{'$' := e4mod}, Src, Dst) ->
  [bc_op(?E4BC_MOVE),
   e4asm_cte:encode(Mod, Src),
   e4asm_cte:encode(Mod, Dst)].


call_fun(Mod = #{'$' := e4mod}, Arity) ->
  [bc_op(?E4BC_CALL_FUN),
   e4asm_cte:encode(Mod, Arity)].


set_nil(Mod = #{'$' := e4mod}, Dst) ->
  [bc_op(?E4BC_SET_NIL),
   e4asm_cte:encode(Mod, Dst)].


call(Mod = #{'$' := e4mod},
     Call = #{'$' := e4call, arity := A, target := Target}) ->
  Tail = maps:get(tailcall, Call, false),
  Dealloc = maps:get(dealloc, Call, 0),
  %% TODO: Dealloc
  case Target of
    {f, TargetLabel} ->
      [bc_op(?E4BC_CALL_LOCAL, Tail, Dealloc =/= 0), e4c:varint(TargetLabel)];
    {extfunc, _, _, _} = MFA ->
      [bc_op(?E4BC_CALL_EXT, Tail, Dealloc =/= 0),
       e4asm_cte:encode(Mod, MFA)]
  end.

bif(Mod = #{'$' := e4mod},
    Bif = #{'$' := e4bif, args := Args, name := Name}) ->
  %% TODO: Args
  %% TODO: Gc
  Gc = maps:get(gc, Bif, 0),
  %% TODO: Fail
  Fail = maps:get(fail, Bif, ignore),
  %% TODO: Result
  Result = maps:get(result, Bif, ignore),
  [bc_op(?E4BC_BIF, Fail =/= ignore, Gc =/= 0, Result =/= 0),
   e4asm_cte:encode(Mod, {atom, Name})].

allocate(StackNeed, 0, Live) ->
  [bc_op(?E4BC_ALLOC_S),
   e4asm_cte:encode(#{}, StackNeed),
   e4asm_cte:encode(#{}, Live)];
allocate(StackNeed, HeapNeed, Live) ->
  [bc_op(?E4BC_ALLOC_S_H),
   e4asm_cte:encode(#{}, StackNeed),
   e4asm_cte:encode(#{}, HeapNeed),
   e4asm_cte:encode(#{}, Live)].

get_element(Tuple, Index, Result) ->
  %% TODO: Result
  [bc_op(?E4BC_GET_ELEMENT),
   e4asm_cte:encode(#{}, Tuple),
   e4asm_cte:encode(#{}, Index),
   e4asm_cte:encode(#{}, Result)].
