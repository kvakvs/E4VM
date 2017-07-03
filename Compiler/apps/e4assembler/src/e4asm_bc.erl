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
-define(E4BC_LABEL,             16#02).
-define(E4BC_LINE_INFO,         16#03). % UNUSED

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

%%encode_value(X, Bits) ->
%%  e4asm_encode_int:encode(X, Bits, #{}).
%%
%%
%%encode_value(X, Bits, Mod) ->
%%  e4asm_encode_int:encode(X, Bits, Mod).


bc_op(X) -> X.


label(F) ->
  e4asm_stats:count_opcode(?E4BC_LABEL),
  Args = [e4asm_encode_int:encode(F, auto_bits)],
  {bc_op(?E4BC_LABEL), Args}.


func_info(Mod = #{'$' := e4mod}, F, Arity) ->
  e4asm_stats:count_opcode(?E4BC_FUNC_INFO),
  ?ASSERT(Arity < ?LIMIT_MAXARITY, "Arity for func_info is too big"),
  Args = [e4asm_encode_int:encode(F, auto_bits, Mod),
          e4asm_encode_int:varlength_unsigned(Arity)],
  {bc_op(?E4BC_FUNC_INFO), Args}.


test_heap(Mod = #{'$' := e4mod}, Need, Live) ->
  e4asm_stats:count_opcode(?E4BC_TEST_HEAP),
  Args = [e4asm_encode_int:encode(Need, auto_bits, Mod),
          e4asm_encode_int:encode(Live, auto_bits, Mod)],
  {bc_op(?E4BC_TEST_HEAP), Args}.


put_tuple(Mod = #{'$' := e4mod}, Size, Dst) ->
  e4asm_stats:count_opcode(?E4BC_PUT_TUPLE),
  Args = [e4asm_encode_int:encode(Size, auto_bits, Mod),
          e4asm_encode_int:encode(Dst, auto_bits, Mod)],
  {bc_op(?E4BC_PUT_TUPLE), Args}.


put(Mod = #{'$' := e4mod}, Val) ->
  e4asm_stats:count_opcode(?E4BC_PUT),
  Args = [e4asm_encode_int:encode(Val, auto_bits, Mod)],
  {bc_op(?E4BC_PUT), Args}.


move(Mod = #{'$' := e4mod}, Src, Dst) ->
  e4asm_stats:count_opcode(?E4BC_MOVE),
  Args = [e4asm_encode_int:encode(Src, auto_bits, Mod),
          e4asm_encode_int:encode(Dst, auto_bits, Mod)],
  {bc_op(?E4BC_MOVE), Args}.


select_val(Src, Fail, Select, Mod = #{'$' := e4mod}) ->
  e4asm_stats:count_opcode(?E4BC_SELECT_VAL),
  Args = [e4asm_encode_int:encode(Src, auto_bits, Mod),
          e4asm_encode_int:encode(Fail, auto_bits, Mod),
          e4asm_encode_int:encode({jumptab, Select}, auto_bits, Mod)],
  {bc_op(?E4BC_SELECT_VAL), Args}.


call_fun(Mod = #{'$' := e4mod}, Arity) ->
  e4asm_stats:count_opcode(?E4BC_CALL_FUN),
  Args = [e4asm_encode_int:encode(Arity, auto_bits, Mod)],
  {bc_op(?E4BC_CALL_FUN), Args}.


set_nil(Mod = #{'$' := e4mod}, Dst) ->
  e4asm_stats:count_opcode(?E4BC_SET_NIL),
  Args = [e4asm_encode_int:encode(Dst, auto_bits, Mod)],
  {bc_op(?E4BC_SET_NIL), Args}.


call(Mod = #{'$' := e4mod},
     Call = #{'$' := e4call, arity := _A, target := Target}) ->
  Tail = maps:get(tailcall, Call, false),
  Dealloc = maps:get(dealloc, Call, 0),

  %% TODO: Dealloc
  case Target of
    {f, TargetLabel} ->
      Opcode0 = bc_op(case Tail of
                        true ->
                          e4asm_stats:count_opcode(?E4BC_CALL_LOCAL_TAIL),
                          ?E4BC_CALL_LOCAL_TAIL;
                        false ->
                          e4asm_stats:count_opcode(?E4BC_CALL_LOCAL),
                          ?E4BC_CALL_LOCAL
                      end),
      {Opcode0,
       [e4asm_encode_int:encode(TargetLabel, auto_bits, Mod),
        e4asm_encode_int:encode(Dealloc, auto_bits, Mod)]};

    {extfunc, _, _, _} = MFA ->
      Opcode1 = bc_op(case Tail of
                     true ->
                       e4asm_stats:count_opcode(?E4BC_CALL_EXT_TAIL),
                       ?E4BC_CALL_EXT_TAIL;
                     false ->
                       e4asm_stats:count_opcode(?E4BC_CALL_EXT),
                       ?E4BC_CALL_EXT
                   end),
      {Opcode1,
       [e4asm_encode_int:encode(MFA, auto_bits, Mod),
        e4asm_encode_int:encode(Dealloc, auto_bits, Mod)]}
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
  Args = [e4asm_encode_int:encode(Fail, auto_bits, Mod),
          e4asm_encode_int:encode({atom, Name}, auto_bits, Mod),
          e4asm_encode_int:encode(Result, auto_bits, Mod)],
  case Gc of
    ignore ->
      e4asm_stats:count_opcode(?E4BC_BIF),
      {bc_op(?E4BC_BIF), Args};
    N ->
      e4asm_stats:count_opcode(?E4BC_BIF_GC),
      {bc_op(?E4BC_BIF_GC),
       Args ++ [e4asm_encode_int:encode(N, auto_bits, Mod)]}
  end.


ret(_Dealloc = 0) ->
  e4asm_stats:count_opcode(?E4BC_RET0),
  {bc_op(?E4BC_RET0), []};

ret(Dealloc) ->
  e4asm_stats:count_opcode(?E4BC_RETN),
  Args = [e4asm_encode_int:varlength_unsigned(Dealloc)],
  {bc_op(?E4BC_RETN), Args}.


allocate(StackNeed, HeapNeed, Live) ->
  e4asm_stats:count_opcode(?E4BC_ALLOC),
  e4asm_util:assert_unsigned_fits("allocate.StackNeed", StackNeed, 10),
  e4asm_util:assert_unsigned_fits("allocate.HeapNeed", HeapNeed, 10),
  e4asm_util:assert_unsigned_fits("allocate.Live", Live, 10),
  Args = [e4asm_encode_int:varlength_unsigned(StackNeed),
          e4asm_encode_int:varlength_unsigned(HeapNeed),
          e4asm_encode_int:varlength_unsigned(Live)],
  {bc_op(?E4BC_ALLOC), Args}.


get_element(Tuple, Index, Result) ->
  %% TODO: Result
  e4asm_stats:count_opcode(?E4BC_GET_ELEMENT),
  Args = [e4asm_encode_int:encode(Tuple, auto_bits),
          e4asm_encode_int:encode(Index, auto_bits),
          e4asm_encode_int:encode(Result, auto_bits)],
  {bc_op(?E4BC_GET_ELEMENT), Args}.


cons(H, T, Dst) ->
  e4asm_stats:count_opcode(?E4BC_CONS),
  Args = [e4asm_encode_int:encode(H, auto_bits),
          e4asm_encode_int:encode(T, auto_bits),
          e4asm_encode_int:encode(Dst, auto_bits)],
  {bc_op(?E4BC_CONS), Args}.


jump(Dst) ->
  e4asm_stats:count_opcode(?E4BC_JUMP),
  Args = [e4asm_encode_int:encode(Dst, auto_bits, #{})],
  {bc_op(?E4BC_JUMP), Args}.


trim(N) ->
  e4asm_stats:count_opcode(?E4BC_TRIM),
  Args = [e4asm_encode_int:encode(N, auto_bits, #{})],
  {bc_op(?E4BC_TRIM), Args}.


make_fun({f, _} = L, NumFree, Mod) ->
  e4asm_stats:count_opcode(?E4BC_MAKE_FUN),
  Args = [e4asm_encode_int:encode({lambda, L, NumFree}, auto_bits, Mod)],
  {bc_op(?E4BC_MAKE_FUN), Args}.


set_element(Value, Tuple, Pos, Mod) ->
  e4asm_stats:count_opcode(?E4BC_SET_ELEMENT),
  Args = [e4asm_encode_int:encode(Value, auto_bits, Mod),
          e4asm_encode_int:encode(Tuple, auto_bits, Mod),
          e4asm_encode_int:encode(Pos, auto_bits, Mod)],
  {bc_op(?E4BC_SET_ELEMENT), Args}.


clear_stack({y, _} = Y) ->
  e4asm_stats:count_opcode(?E4BC_CLEAR_STACK),
  Args = [e4asm_encode_int:encode(Y, auto_bits)],
  {bc_op(?E4BC_CLEAR_STACK), Args}.
