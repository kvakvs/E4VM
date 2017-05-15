%%% @doc Bytecode generation library for E4 VM
-module(e4asm_bc).

%% API
-export([
    allocate/3,
    bif/2,
    call/2,
    call_fun/2,
    clear_stack/1,
    cons/3,
    func_info/3,
    get_element/3,
    jump/1,
    label/1,
    make_fun/3,
    move/3,
    put/2,
    put_tuple/3,
    ret/1,
    select_val/4,
    set_element/4,
    set_nil/2,
    test_heap/3,
    trim/1
]).

-include_lib("e4compiler/include/e4c.hrl").

%% Highest 2 or 3 bits of opcode byte may be used for flags

%% Special commands (parsed at load time, errors etc)
%%
-define(E4BC_FUNC_INFO,         16#01).
-define(E4BC_LABEL,             16#02). % removed at load time
-define(E4BC_LINE_INFO,         16#03). % removed at load time % UNUSED

%%
%% Flow control
%%
-define(E4BC_CALL_LOCAL,        16#10).
-define(E4BC_CALL_LOCAL_TAIL,   16#11).
-define(E4BC_CALL_EXT,          16#12).
-define(E4BC_CALL_EXT_TAIL,     16#13).
-define(E4BC_BIF,               16#14).
-define(E4BC_BIF_GC,            16#15).
-define(E4BC_RET0,              16#16).
-define(E4BC_RETN,              16#17).
-define(E4BC_JUMP,              16#18).
-define(E4BC_SELECT_VAL,        16#19).
-define(E4BC_CALL_FUN,          16#1A).

%%
%% Data control
%%
-define(E4BC_ALLOC,             16#20).
-define(E4BC_GET_ELEMENT,       16#21).
-define(E4BC_MOVE,              16#22).
-define(E4BC_SET_NIL,           16#23).
-define(E4BC_PUT_TUPLE,         16#24).
-define(E4BC_PUT,               16#25).
-define(E4BC_CONS,              16#26).
-define(E4BC_TRIM,              16#27).
-define(E4BC_MAKE_FUN,          16#28).
-define(E4BC_SET_ELEMENT,       16#29).
-define(E4BC_CLEAR_STACK,       16#2A). % use Set_nil instead?
-define(E4BC_TEST_HEAP,         16#2B).



bc_op(X) -> X.
%%%%bc_op(X, F1) -> <<(X bor bit_if(F1, 128)):8>>.
%%bc_op(X, F1, F2) -> (X bor bit_if(F1, 128)
%%                       bor bit_if(F2, 64)).
%%bc_op(X, F1, F2, F3) -> (X bor bit_if(F1, 128)
%%                           bor bit_if(F2, 64)
%%                           bor bit_if(F3, 32)).


%%bit_if(true, X) -> X;
%%bit_if(false, _X) -> 0.


label(F) ->
  [bc_op(?E4BC_LABEL),
   e4asm_cte:encode(F, #{})].


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
     Call = #{'$' := e4call, arity := _A, target := Target}) ->
  Tail = maps:get(tailcall, Call, false),
  Dealloc = maps:get(dealloc, Call, 0),
  %% TODO: Dealloc
  case Target of
    {f, TargetLabel} ->
      [bc_op(case Tail of
               true -> ?E4BC_CALL_LOCAL_TAIL;
               false -> ?E4BC_CALL_LOCAL
             end),
       e4c:varint(TargetLabel),
       e4c:varint(Dealloc)];
    {extfunc, _, _, _} = MFA ->
      [bc_op(case Tail of
               true -> ?E4BC_CALL_EXT_TAIL;
               false -> ?E4BC_CALL_EXT
             end),
       e4asm_cte:encode(MFA, Mod),
       e4c:varint(Dealloc)]
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
  [case Gc of
     ignore ->
       bc_op(?E4BC_BIF);
     N ->
       [bc_op(?E4BC_BIF_GC), e4asm_cte:encode(N, Mod)]
   end,
   e4asm_cte:encode(Fail, Mod),
   e4asm_cte:encode({atom, Name}, Mod),
   e4asm_cte:encode(Result, Mod)].


ret(_Dealloc = 0) ->
  [bc_op(?E4BC_RET0)];

ret(Dealloc) ->
  [bc_op(?E4BC_RETN),
   e4asm_cte:encode(Dealloc, #{})].


allocate(StackNeed, HeapNeed, Live) ->
  assert_integer_fits("allocate.StackNeed", StackNeed, 10),
  assert_integer_fits("allocate.HeapNeed", HeapNeed, 10),
  assert_integer_fits("allocate.Live", Live, 10),
  [bc_op(?E4BC_ALLOC),
   e4asm_cte:encode(StackNeed, #{}),
   e4asm_cte:encode(HeapNeed, #{}),
   e4asm_cte:encode(Live, #{})
   ].


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


clear_stack({y, _} = Y) ->
  [bc_op(?E4BC_CLEAR_STACK),
   e4asm_cte:encode(Y, #{})].


assert_integer_fits(Name, N, Bits) ->
  Bin = <<N:Bits>>,
  <<N1:Bits>> = Bin,
  case N == N1 of
    true -> ok;
    false -> ?COMPILE_ERROR("Value ~s does not fit into ~B bits", [Name, Bits])
  end.
