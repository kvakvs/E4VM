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


encode_value(X) ->
  encode_value(X, #{}).


encode_value(X, Mod) ->
  e4asm_simple_encoding:encode(X, Mod).


bc_op(X) -> X.


label(F) ->
  Args = [encode_value(F)],
  {bc_op(?E4BC_LABEL), Args}.


func_info(Mod = #{'$' := e4mod}, F, Arity) ->
  Args = [encode_value(F, Mod),
          encode_value(Arity, Mod)],
  {bc_op(?E4BC_FUNC_INFO), Args}.


test_heap(Mod = #{'$' := e4mod}, Need, Live) ->
  Args = [encode_value(Need, Mod),
          encode_value(Live, Mod)],
  {bc_op(?E4BC_TEST_HEAP), Args}.


put_tuple(Mod = #{'$' := e4mod}, Size, Dst) ->
  Args = [encode_value(Size, Mod),
          encode_value(Dst, Mod)],
  {bc_op(?E4BC_PUT_TUPLE), Args}.


put(Mod = #{'$' := e4mod}, Val) ->
  Args = [encode_value(Val, Mod)],
  {bc_op(?E4BC_PUT), Args}.


move(Mod = #{'$' := e4mod}, Src, Dst) ->
  Args = [encode_value(Src, Mod),
          encode_value(Dst, Mod)],
  {bc_op(?E4BC_MOVE), Args}.


select_val(Src, Fail, Select, Mod = #{'$' := e4mod}) ->
  Args = [encode_value(Src, Mod),
          encode_value(Fail, Mod),
          encode_value({jumptab, Select}, Mod)],
  {bc_op(?E4BC_SELECT_VAL), Args}.


call_fun(Mod = #{'$' := e4mod}, Arity) ->
  Args = [encode_value(Arity, Mod)],
  {bc_op(?E4BC_CALL_FUN), Args}.


set_nil(Mod = #{'$' := e4mod}, Dst) ->
  Args = [encode_value(Dst, Mod)],
  {bc_op(?E4BC_SET_NIL), Args}.


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
  Opcode = case Gc of
             ignore ->
               bc_op(?E4BC_BIF);
             N ->
               [bc_op(?E4BC_BIF_GC), encode_value(N, Mod)]
           end,
  Args = [encode_value(Fail, Mod),
          encode_value({atom, Name}, Mod),
          encode_value(Result, Mod)],
  {Opcode, Args}.


ret(_Dealloc = 0) ->
  {bc_op(?E4BC_RET0), []};

ret(Dealloc) ->
  Args = [encode_value(Dealloc)],
  {bc_op(?E4BC_RETN), Args}.


allocate(StackNeed, HeapNeed, Live) ->
  assert_integer_fits("allocate.StackNeed", StackNeed, 10),
  assert_integer_fits("allocate.HeapNeed", HeapNeed, 10),
  assert_integer_fits("allocate.Live", Live, 10),
  Args = [encode_value(StackNeed),
          encode_value(HeapNeed),
          encode_value(Live)],
  {bc_op(?E4BC_ALLOC), Args}.


get_element(Tuple, Index, Result) ->
  %% TODO: Result
  Args = [encode_value(Tuple),
          encode_value(Index),
          encode_value(Result)],
  {bc_op(?E4BC_GET_ELEMENT), Args}.


cons(H, T, Dst) ->
  Args = [encode_value(H),
          encode_value(T),
          encode_value(Dst)],
  {bc_op(?E4BC_CONS), Args}.


jump(Dst) ->
  Args = [encode_value(Dst, #{})],
  {bc_op(?E4BC_JUMP), Args}.


trim(N) ->
  Args = [encode_value(N, #{})],
  {bc_op(?E4BC_TRIM), Args}.


make_fun({f, _} = L, NumFree, Mod) ->
  Args = [encode_value({lambda, L, NumFree}, Mod)],
  {bc_op(?E4BC_MAKE_FUN), Args}.


set_element(Value, Tuple, Pos, Mod) ->
  Args = [encode_value(Value, Mod),
          encode_value(Tuple, Mod),
          encode_value(Pos, Mod)],
  {bc_op(?E4BC_SET_ELEMENT), Args}.


clear_stack({y, _} = Y) ->
  Args = [encode_value(Y)],
  {bc_op(?E4BC_CLEAR_STACK), Args}.


assert_integer_fits(Name, N, Bits) ->
  Bin = <<N:Bits>>,
  <<N1:Bits>> = Bin,
  case N == N1 of
    true -> ok;
    false -> ?COMPILE_ERROR("Value ~s does not fit into ~B bits", [Name, Bits])
  end.
