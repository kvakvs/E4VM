%%% @doc Bytecode generation library for E4 VM
-module(e4asm_bc).

%% API
-export([
  allocate/3,
  bif/2,
  call/2,
  call_fun/2,
  clear_stack/1,
  func_info/3,
  get_element/3,
  jump/1,
  make_fun/3,
  move/3,
  put/2,
  cons/3,
  put_tuple/3,
  ret/1,
  select_val/4,
  set_element/4,
  set_nil/2,
  test_heap/3,
  trim/1
]).

-define(E4BC_FUNC_INFO,   0).
-define(E4BC_CALL_LOCAL,  1). % 4 local call flavours defined by bits 6 and 7
-define(E4BC_CALL_EXT,    2). % 4   ext call flavours defined by bits 6 and 7
-define(E4BC_BIF,         3).
-define(E4BC_ALLOC_STACK,       4).
-define(E4BC_ALLOC_STACK_HEAP,  5).
-define(E4BC_GET_ELEMENT, 6).
-define(E4BC_MOVE,        7).
-define(E4BC_CALL_FUN,    8).
-define(E4BC_SET_NIL,     9).
-define(E4BC_TEST_HEAP,   10).
-define(E4BC_PUT_TUPLE,   11).
-define(E4BC_PUT,         12).
-define(E4BC_RET0,        13).
-define(E4BC_RETN,        14).
-define(E4BC_SELECT_VAL,  15).
-define(E4BC_CONS,        16).
-define(E4BC_JUMP,        17).
-define(E4BC_TRIM,        18).
-define(E4BC_MAKE_FUN,    19).
-define(E4BC_SET_ELEMENT, 20).
-define(E4BC_CLEAR_STACK, 21). % use Set_nil instead?


bc_op(X) -> X.
%%bc_op(X, F1) -> <<(X bor bit_if(F1, 128)):8>>.
bc_op(X, F1, F2) -> (X bor bit_if(F1, 128)
                       bor bit_if(F2, 64)).
bc_op(X, F1, F2, F3) -> (X bor bit_if(F1, 128)
                           bor bit_if(F2, 64)
                           bor bit_if(F3, 32)).


bit_if(true, X) -> X;
bit_if(false, _X) -> 0.


func_info(Mod = #{'$' := e4mod}, F, Arity) ->
  [bc_op(?E4BC_FUNC_INFO),
   e4asm_cte:encode(F    , Mod),
   e4asm_cte:encode(Arity, Mod)].


test_heap(Mod = #{'$' := e4mod}, Need, Live) ->
  [bc_op(?E4BC_TEST_HEAP),
   e4asm_cte:encode(Need, Mod),
   e4asm_cte:encode(Live, Mod)].


put_tuple(Mod = #{'$' := e4mod}, Size, Dst) ->
  [bc_op(?E4BC_PUT_TUPLE),
   e4asm_cte:encode(Size, Mod),
   e4asm_cte:encode(Dst, Mod)].


put(Mod = #{'$' := e4mod}, Val) ->
  [bc_op(?E4BC_PUT),
   e4asm_cte:encode(Val, Mod)].


move(Mod = #{'$' := e4mod}, Src, Dst) ->
  [bc_op(?E4BC_MOVE),
   e4asm_cte:encode(Src, Mod),
   e4asm_cte:encode(Dst, Mod)].


select_val(Src, Fail, Select, Mod = #{'$' := e4mod}) ->
  [bc_op(?E4BC_SELECT_VAL),
   e4asm_cte:encode(Src, Mod),
   e4asm_cte:encode(Fail, Mod),
   e4asm_cte:encode({jumptab, Select}, Mod)].


call_fun(Mod = #{'$' := e4mod}, Arity) ->
  [bc_op(?E4BC_CALL_FUN),
   e4asm_cte:encode(Arity, Mod)].


set_nil(Mod = #{'$' := e4mod}, Dst) ->
  [bc_op(?E4BC_SET_NIL),
   e4asm_cte:encode(Dst, Mod)].



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
       e4asm_cte:encode(MFA, Mod)]
  end.


bif(Mod = #{'$' := e4mod},
    Bif = #{'$' := e4bif, args := _Args, name := Name}) ->
  %% TODO: Args
  %% TODO: Gc
  Gc = maps:get(gc, Bif, 0),
  %% TODO: Fail
  Fail = maps:get(fail, Bif, ignore),
  %% TODO: Result
  Result = maps:get(result, Bif, ignore),
  [bc_op(?E4BC_BIF, Fail =/= ignore, Gc =/= 0, Result =/= 0),
   e4asm_cte:encode({atom, Name}, Mod)].


ret(_Dealloc = 0) -> [bc_op(?E4BC_RET0)];

ret(Dealloc) -> [bc_op(?E4BC_RETN), e4asm_cte:encode(Dealloc, #{})].


allocate(StackNeed, 0, Live) ->
  [bc_op(?E4BC_ALLOC_STACK),
   e4c:varint(StackNeed),
   e4c:varint(Live)];

allocate(StackNeed, HeapNeed, Live) ->
  [bc_op(?E4BC_ALLOC_STACK_HEAP),
   e4c:varint(StackNeed),
   e4c:varint(HeapNeed),
   e4c:varint(Live)].


get_element(Tuple, Index, Result) ->
  %% TODO: Result
  [bc_op(?E4BC_GET_ELEMENT),
   e4asm_cte:encode(Tuple, #{}),
   e4asm_cte:encode(Index, #{}),
   e4asm_cte:encode(Result, #{})].


cons(H, T, Dst) ->
  [bc_op(?E4BC_CONS),
   e4asm_cte:encode(H, #{}),
   e4asm_cte:encode(T, #{}),
   e4asm_cte:encode(Dst, #{})].


jump(Dst) ->
  [bc_op(?E4BC_JUMP),
   e4asm_cte:encode(Dst, #{})].


trim(N) ->
  [bc_op(?E4BC_TRIM),
   e4asm_cte:encode(N, #{})].


make_fun({f, _} = L, NumFree, Mod) ->
  [bc_op(?E4BC_MAKE_FUN),
   e4asm_cte:encode({lambda, L, NumFree}, Mod)].


set_element(Value, Tuple, Pos, Mod) ->
  [bc_op(?E4BC_SET_ELEMENT),
   e4asm_cte:encode(Value, Mod),
   e4asm_cte:encode(Tuple, Mod),
   e4asm_cte:encode(Pos, Mod)].


clear_stack({y, Y}) ->
  [bc_op(?E4BC_CLEAR_STACK),
    e4c:varint(Y)].
