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


encode_value(X, Mod) -> 
  e4asm_simple_encoding:encode(X, Mod).


bc_op(X) -> X.


label(F) ->
  [bc_op(?E4BC_LABEL),
   encode_value(F, #{})].


func_info(Mod = #{'$' := e4mod}, F, Arity) ->
  [bc_op(?E4BC_FUNC_INFO),
   encode_value(F    , Mod),
   encode_value(Arity, Mod)].


test_heap(Mod = #{'$' := e4mod}, Need, Live) ->
  [bc_op(?E4BC_TEST_HEAP),
   encode_value(Need, Mod),
   encode_value(Live, Mod)].


put_tuple(Mod = #{'$' := e4mod}, Size, Dst) ->
  [bc_op(?E4BC_PUT_TUPLE),
   encode_value(Size, Mod),
   encode_value(Dst, Mod)].


put(Mod = #{'$' := e4mod}, Val) ->
  [bc_op(?E4BC_PUT),
   encode_value(Val, Mod)].


move(Mod = #{'$' := e4mod}, Src, Dst) ->
  [bc_op(?E4BC_MOVE),
   encode_value(Src, Mod),
   encode_value(Dst, Mod)].


select_val(Src, Fail, Select, Mod = #{'$' := e4mod}) ->
  [bc_op(?E4BC_SELECT_VAL),
   encode_value(Src, Mod),
   encode_value(Fail, Mod),
   encode_value({jumptab, Select}, Mod)].


call_fun(Mod = #{'$' := e4mod}, Arity) ->
  [bc_op(?E4BC_CALL_FUN),
   encode_value(Arity, Mod)].


set_nil(Mod = #{'$' := e4mod}, Dst) ->
  [bc_op(?E4BC_SET_NIL),
   encode_value(Dst, Mod)].


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
       encode_value(TargetLabel, Mod),
       encode_value(Dealloc, Mod)];

    {extfunc, _, _, _} = MFA ->
      [bc_op(case Tail of
               true -> ?E4BC_CALL_EXT_TAIL;
               false -> ?E4BC_CALL_EXT
             end),
       encode_value(MFA, Mod),
       encode_value(Dealloc, Mod)]
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
       [bc_op(?E4BC_BIF_GC), encode_value(N, Mod)]
   end,
   encode_value(Fail, Mod),
   encode_value({atom, Name}, Mod),
   encode_value(Result, Mod)].


ret(_Dealloc = 0) ->
  [bc_op(?E4BC_RET0)];

ret(Dealloc) ->
  [bc_op(?E4BC_RETN),
   encode_value(Dealloc, #{})].


allocate(StackNeed, HeapNeed, Live) ->
  assert_integer_fits("allocate.StackNeed", StackNeed, 10),
  assert_integer_fits("allocate.HeapNeed", HeapNeed, 10),
  assert_integer_fits("allocate.Live", Live, 10),
  [bc_op(?E4BC_ALLOC),
   encode_value(StackNeed, #{}),
   encode_value(HeapNeed, #{}),
   encode_value(Live, #{})
   ].


get_element(Tuple, Index, Result) ->
  %% TODO: Result
  [bc_op(?E4BC_GET_ELEMENT),
   encode_value(Tuple, #{}),
   encode_value(Index, #{}),
   encode_value(Result, #{})].


cons(H, T, Dst) ->
  [bc_op(?E4BC_CONS),
   encode_value(H, #{}),
   encode_value(T, #{}),
   encode_value(Dst, #{})].


jump(Dst) ->
  [bc_op(?E4BC_JUMP),
   encode_value(Dst, #{})].


trim(N) ->
  [bc_op(?E4BC_TRIM),
   encode_value(N, #{})].


make_fun({f, _} = L, NumFree, Mod) ->
  [bc_op(?E4BC_MAKE_FUN),
   encode_value({lambda, L, NumFree}, Mod)].


set_element(Value, Tuple, Pos, Mod) ->
  [bc_op(?E4BC_SET_ELEMENT),
   encode_value(Value, Mod),
   encode_value(Tuple, Mod),
   encode_value(Pos, Mod)].


clear_stack({y, _} = Y) ->
  [bc_op(?E4BC_CLEAR_STACK),
   encode_value(Y, #{})].


assert_integer_fits(Name, N, Bits) ->
  Bin = <<N:Bits>>,
  <<N1:Bits>> = Bin,
  case N == N1 of
    true -> ok;
    false -> ?COMPILE_ERROR("Value ~s does not fit into ~B bits", [Name, Bits])
  end.
