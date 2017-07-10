%%% @doc Bytecode generation library for MicroErlang VM.
%%% This module does NOT handle final binary encoding or compression.
%%% @end

-module(uasm_bytecode).

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

-include_lib("uerlc/include/uerlc.hrl").

%% Highest 2 or 3 bits of opcode byte may be used for flags

%% Special commands (parsed at load time, errors etc)
%%
-define(BYTECODE_FUNC_INFO,         16#01).
-define(BYTECODE_LABEL,             16#02).
-define(BYTECODE_LINE_INFO,         16#03). % UNUSED

%%
%% Flow control
%%
-define(BYTECODE_CALL_LOCAL,        16#10).
-define(BYTECODE_CALL_LOCAL_TAIL,   16#11).
-define(BYTECODE_CALL_EXT,          16#12).
-define(BYTECODE_CALL_EXT_TAIL,     16#13).
-define(BYTECODE_BIF,               16#14).
-define(BYTECODE_BIF_GC,            16#15).
-define(BYTECODE_RET0,              16#16).
-define(BYTECODE_RETN,              16#17).
-define(BYTECODE_JUMP,              16#18).
-define(BYTECODE_SELECT_VAL,        16#19).
-define(BYTECODE_CALL_FUN,          16#1A).

%%
%% Data control
%%
-define(BYTECODE_ALLOC,             16#20).
-define(BYTECODE_GET_ELEMENT,       16#21).
-define(BYTECODE_MOVE,              16#22).
-define(BYTECODE_SET_NIL,           16#23).
-define(BYTECODE_PUT_TUPLE,         16#24).
-define(BYTECODE_PUT,               16#25).
-define(BYTECODE_CONS,              16#26).
-define(BYTECODE_TRIM,              16#27).
-define(BYTECODE_MAKE_FUN,          16#28).
-define(BYTECODE_SET_ELEMENT,       16#29).
-define(BYTECODE_CLEAR_STACK,       16#2A). % use Set_nil instead?
-define(BYTECODE_TEST_HEAP,         16#2B).

%%encode_value(X, Bits) ->
%%  uasm_encode_int:encode(X, Bits, #{}).
%%
%%
%%encode_value(X, Bits, Mod) ->
%%  uasm_encode_int:encode(X, Bits, Mod).


bc_op(X) -> X.


label(F) ->
  uasm_stats:count_opcode(?BYTECODE_LABEL),
  %% not binary, this will be processed and removed during compression
  {label, F}.


func_info(Mod = #{'$' := module}, F, Arity) ->
  uasm_stats:count_opcode(?BYTECODE_FUNC_INFO),
  ?ASSERT(Arity < ?LIMIT_MAXARITY, "Arity for func_info is too big"),
  Args = [uasm_encode_int:encode(F, auto_bits, Mod),
          uasm_encode_int:varlength_unsigned(Arity)],
  {bc_op(?BYTECODE_FUNC_INFO), Args}.


test_heap(Mod = #{'$' := module}, Need, Live) ->
  uasm_stats:count_opcode(?BYTECODE_TEST_HEAP),
  Args = [uasm_encode_int:encode(Need, auto_bits, Mod),
          uasm_encode_int:encode(Live, auto_bits, Mod)],
  {bc_op(?BYTECODE_TEST_HEAP), Args}.


put_tuple(Mod = #{'$' := module}, Size, Dst) ->
  uasm_stats:count_opcode(?BYTECODE_PUT_TUPLE),
  Args = [uasm_encode_int:encode(Size, auto_bits, Mod),
          uasm_encode_int:encode(Dst, auto_bits, Mod)],
  {bc_op(?BYTECODE_PUT_TUPLE), Args}.


put(Mod = #{'$' := module}, Val) ->
  uasm_stats:count_opcode(?BYTECODE_PUT),
  Args = [uasm_encode_int:encode(Val, auto_bits, Mod)],
  {bc_op(?BYTECODE_PUT), Args}.


move(Mod = #{'$' := module}, Src, Dst) ->
  uasm_stats:count_opcode(?BYTECODE_MOVE),
  Args = [uasm_encode_int:encode(Src, auto_bits, Mod),
          uasm_encode_int:encode(Dst, auto_bits, Mod)],
  {bc_op(?BYTECODE_MOVE), Args}.


select_val(Src, Fail, Select, Mod = #{'$' := module}) ->
  uasm_stats:count_opcode(?BYTECODE_SELECT_VAL),
  Args = [uasm_encode_int:encode(Src, auto_bits, Mod),
          uasm_encode_int:encode(Fail, auto_bits, Mod),
          uasm_encode_int:encode({jumptab, Select}, auto_bits, Mod)],
  {bc_op(?BYTECODE_SELECT_VAL), Args}.


call_fun(Mod = #{'$' := module}, Arity) ->
  uasm_stats:count_opcode(?BYTECODE_CALL_FUN),
  Args = [uasm_encode_int:encode(Arity, auto_bits, Mod)],
  {bc_op(?BYTECODE_CALL_FUN), Args}.


set_nil(Mod = #{'$' := module}, Dst) ->
  uasm_stats:count_opcode(?BYTECODE_SET_NIL),
  Args = [uasm_encode_int:encode(Dst, auto_bits, Mod)],
  {bc_op(?BYTECODE_SET_NIL), Args}.


call(Mod = #{'$' := module},
     Call = #{'$' := e4call, arity := _A, target := Target}) ->
  Tail = maps:get(tailcall, Call, false),
  Dealloc = maps:get(dealloc, Call, 0),

  %% TODO: Dealloc
  case Target of
    {f, TargetLabel} ->
      Opcode0 = bc_op(case Tail of
                        true ->
                          uasm_stats:count_opcode(?BYTECODE_CALL_LOCAL_TAIL),
                          ?BYTECODE_CALL_LOCAL_TAIL;
                        false ->
                          uasm_stats:count_opcode(?BYTECODE_CALL_LOCAL),
                          ?BYTECODE_CALL_LOCAL
                      end),
      {Opcode0,
       [uasm_encode_int:encode(TargetLabel, auto_bits, Mod),
        uasm_encode_int:encode(Dealloc, auto_bits, Mod)]};

    {extfunc, _, _, _} = MFA ->
      Opcode1 = bc_op(case Tail of
                     true ->
                       uasm_stats:count_opcode(?BYTECODE_CALL_EXT_TAIL),
                       ?BYTECODE_CALL_EXT_TAIL;
                     false ->
                       uasm_stats:count_opcode(?BYTECODE_CALL_EXT),
                       ?BYTECODE_CALL_EXT
                   end),
      {Opcode1,
       [uasm_encode_int:encode(MFA, auto_bits, Mod),
        uasm_encode_int:encode(Dealloc, auto_bits, Mod)]}
  end.


bif(Mod = #{'$' := module},
    Bif = #{'$' := e4bif, args := _Args, name := Name}) ->
  %% TODO: Args
  %% TODO: Gc
  Gc = maps:get(gc, Bif, 0),
  %% TODO: Fail
  Fail = maps:get(fail, Bif, ignore),
  %% TODO: Result
  Result = maps:get(result, Bif, ignore),
  Args = [uasm_encode_int:encode(Fail, auto_bits, Mod),
          uasm_encode_int:encode({atom, Name}, auto_bits, Mod),
          uasm_encode_int:encode(Result, auto_bits, Mod)],
  case Gc of
    ignore ->
      uasm_stats:count_opcode(?BYTECODE_BIF),
      {bc_op(?BYTECODE_BIF), Args};
    N ->
      uasm_stats:count_opcode(?BYTECODE_BIF_GC),
      {bc_op(?BYTECODE_BIF_GC),
       Args ++ [uasm_encode_int:encode(N, auto_bits, Mod)]}
  end.


ret(_Dealloc = 0) ->
  uasm_stats:count_opcode(?BYTECODE_RET0),
  {bc_op(?BYTECODE_RET0), []};

ret(Dealloc) ->
  uasm_stats:count_opcode(?BYTECODE_RETN),
  Args = [uasm_encode_int:varlength_unsigned(Dealloc)],
  {bc_op(?BYTECODE_RETN), Args}.


allocate(StackNeed, HeapNeed, Live) ->
  uasm_stats:count_opcode(?BYTECODE_ALLOC),
  uasm_util:assert_unsigned_fits("allocate.StackNeed", StackNeed, 10),
  uasm_util:assert_unsigned_fits("allocate.HeapNeed", HeapNeed, 10),
  uasm_util:assert_unsigned_fits("allocate.Live", Live, 10),
  Args = [uasm_encode_int:varlength_unsigned(StackNeed),
          uasm_encode_int:varlength_unsigned(HeapNeed),
          uasm_encode_int:varlength_unsigned(Live)],
  {bc_op(?BYTECODE_ALLOC), Args}.


get_element(Tuple, Index, Result) ->
  %% TODO: Result
  uasm_stats:count_opcode(?BYTECODE_GET_ELEMENT),
  Args = [uasm_encode_int:encode(Tuple, auto_bits),
          uasm_encode_int:encode(Index, auto_bits),
          uasm_encode_int:encode(Result, auto_bits)],
  {bc_op(?BYTECODE_GET_ELEMENT), Args}.


cons(H, T, Dst) ->
  uasm_stats:count_opcode(?BYTECODE_CONS),
  Args = [uasm_encode_int:encode(H, auto_bits),
          uasm_encode_int:encode(T, auto_bits),
          uasm_encode_int:encode(Dst, auto_bits)],
  {bc_op(?BYTECODE_CONS), Args}.


jump(Dst) ->
  uasm_stats:count_opcode(?BYTECODE_JUMP),
  Args = [uasm_encode_int:encode(Dst, auto_bits, #{})],
  {bc_op(?BYTECODE_JUMP), Args}.


trim(N) ->
  uasm_stats:count_opcode(?BYTECODE_TRIM),
  Args = [uasm_encode_int:encode(N, auto_bits, #{})],
  {bc_op(?BYTECODE_TRIM), Args}.


make_fun({f, _} = L, NumFree, Mod) ->
  uasm_stats:count_opcode(?BYTECODE_MAKE_FUN),
  Args = [uasm_encode_int:encode({lambda, L, NumFree}, auto_bits, Mod)],
  {bc_op(?BYTECODE_MAKE_FUN), Args}.


set_element(Value, Tuple, Pos, Mod) ->
  uasm_stats:count_opcode(?BYTECODE_SET_ELEMENT),
  Args = [uasm_encode_int:encode(Value, auto_bits, Mod),
          uasm_encode_int:encode(Tuple, auto_bits, Mod),
          uasm_encode_int:encode(Pos, auto_bits, Mod)],
  {bc_op(?BYTECODE_SET_ELEMENT), Args}.


clear_stack({y, _} = Y) ->
  uasm_stats:count_opcode(?BYTECODE_CLEAR_STACK),
  Args = [uasm_encode_int:encode(Y, auto_bits)],
  {bc_op(?BYTECODE_CLEAR_STACK), Args}.
