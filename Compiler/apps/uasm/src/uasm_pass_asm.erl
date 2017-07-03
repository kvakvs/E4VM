%%% @doc Given simplified assembly AST produce binary MicroErlang VM bytecode.
%%% The bytecode is compressed before it is written to disk and will be ready
%%% to run on the target device without decompression.
%%% @end

-module(uasm_pass_asm).

%% API
-export([compile/1]).

-include_lib("uerlc/include/uerlc.hrl").
-include_lib("uasm/include/uasm.hrl").


compile(#{'$' := module, funs := Funs0, mod := ModName} = Mod0) ->
  %% For each fun, process it
  Mod1 = Mod0#{
    atoms => orddict:from_list([{ModName, 0}]),
    labels => orddict:new(),
    imports => orddict:new(),
    jumptabs => orddict:new(),
    lambdas => orddict:new(),
    literals => orddict:new()
  },

  Mod3 = lists:foldl(fun compile_fold_helper/2, Mod1, Funs0),
  %Mod3 = Mod2#{'$' := uasmmod, funs => Funs1},

  %e4c:debug_write_term("uasm_pass_asm.txt", Mod3),
  %io:format("~s~n~p~n", [color:redb("uASM PASS ASM"), Mod3]),
  Mod3.

%% Folding over orddict will give us pairs {Key, Value}
-spec compile_fold_helper(e4fun(), e4mod()) -> e4mod().
compile_fold_helper({FunArity, F0 = #{'$' := e4fun}},
                    Mod0 = #{'$' := module}) ->
  {F1, Mod1} = process_fun(F0, Mod0),

  NewFuns = orddict:store(FunArity, F1, maps:get(funs, Mod1, orddict:new())),
  Mod1#{funs := NewFuns}.

%%%-----------------------------------------------------------------------------

process_fun(Fun = #{'$' := e4fun, code := Code0}, M0) ->
  X = lists:foldl(
    fun(Op, State) -> process_fun_fold_helper(Fun, Op, State) end,
    #{'$' => fun_state,
      output => [],
      labels => orddict:new(),
      program => M0},
    Code0
  ),

  #{'$' := fun_state,
    output := Code1,
    labels := Labels,
    program := M1} = X,

  M2 = merge_labels(M1, Labels),
  {Fun#{ output => lists:reverse(Code1) }, M2}.


process_fun_fold_helper(Fun = #{'$' := e4fun},
                        Op0,
                        #{'$' := fun_state,
                          output := Accum,
                          program := Mod0} = FState) ->
  #{'$' := processop_result,
    output := Output1,
    program := Mod1} = process_op(Mod0, Fun, Op0),
  FState#{output => [Output1 | Accum],
          program => Mod1}.


%% Takes offset as the size of all existing binary code in the module
%% Merges NewLabels with Offset into the module
merge_labels(Mod = #{'$' := module,
                     funs := Funs,
                     labels := Labels0}, NewLabels) ->
  %% Count sizes for code in all existing functions
  Offset = lists:foldl(
    fun({_Key, #{'$' := e4fun, binary := B}}, Sum) -> Sum + iolist_size(B);
       ({_Key, _}, Sum) -> Sum
    end, 0, Funs),

  Labels1 = lists:foldl(
    fun ({L, LOffs}, Acc) ->
      orddict:store(L, LOffs + Offset, Acc)
    end,
    Labels0, NewLabels
  ),

  Mod#{labels => Labels1}.


-spec process_op(e4mod(), e4fun(), tuple() | atom()) -> e4mod().
process_op(Mod0, _Fun,
           {func_info, _Mod, FunName, Arity}) ->
  Mod1 = register_value(FunName, Mod0),
  make_result(Mod1, uasm_bytecode:func_info(Mod1, FunName, Arity));

process_op(Mod0, _Fun, #{'$' := e4call, target := Target} = CallOp) ->
  Mod1 = register_call_target(Target, Mod0),
  make_result(Mod1, uasm_bytecode:call(Mod1, CallOp));

process_op(Mod0, _Fun, #{'$' := e4bif, args := Args, name := Name} = BifOp) ->
  Mod1 = register_value(Name, Mod0),
  Mod2 = register_valuelist(Args, Mod1),
  make_result(Mod2, uasm_bytecode:bif(Mod2, BifOp));

process_op(Mod0, _Fun, #{'$' := e4ret, dealloc := Dealloc}) ->
  make_result(Mod0, uasm_bytecode:ret(Dealloc));

process_op(Mod0, Fun, {test, TestName, Fail, _MaybeLive, Args, Result}) ->
  Call = #{
    '$' => e4bif,
    name => TestName,
    args => Args,
    fail => Fail,
    result => Result
  },
  process_op(Mod0, Fun, Call);

process_op(Mod0, _Fun, {allocate, StackNeed, Live}) ->
  make_result(Mod0, uasm_bytecode:allocate(StackNeed, 0, Live));

process_op(Mod0, _Fun, {allocate_zero, StackNeed, Live}) ->
  make_result(Mod0, uasm_bytecode:allocate(StackNeed, 0, Live));

process_op(Mod0, _Fun, {allocate_heap, StackNeed, HeapNeed, Live}) ->
  make_result(Mod0, uasm_bytecode:allocate(StackNeed, HeapNeed, Live));

process_op(Mod0, _Fun, {allocate_heap_zero, StackNeed, HeapNeed, Live}) ->
  make_result(Mod0, uasm_bytecode:allocate(StackNeed, HeapNeed, Live));

process_op(Mod0, _Fun, {get_tuple_element, Tuple, Index, Result}) ->
  Mod1 = register_value(Tuple, Mod0),
  make_result(Mod1, uasm_bytecode:get_element(Tuple, Index, Result));

process_op(Mod0, _Fun, {move, Src, Dst}) ->
  Mod1 = register_value(Src, Mod0),
  make_result(Mod1, uasm_bytecode:move(Mod1, Src, Dst));

process_op(Mod0, _Fun, {call_fun, Arity}) ->
  make_result(Mod0, uasm_bytecode:call_fun(Mod0, Arity));

process_op(Mod0, _Fun, {kill, Dst}) ->
  make_result(Mod0, uasm_bytecode:set_nil(Mod0, Dst));

process_op(Mod0, _Fun, {test_heap, Need, Live}) ->
  make_result(Mod0, uasm_bytecode:test_heap(Mod0, Need, Live));

process_op(Mod0, Fun, {get_list, Src, H, T}) ->
  Call = #{
    '$' => e4bif,
    name => e4_decons,
    args => [Src, H, T]
  },
  process_op(Mod0, Fun, Call);

process_op(Mod0, Fun, {badmatch, Reg}) ->
  Call = #{
    '$' => e4bif,
    name => e4_badmatch,
    args => [Reg]
  },
  process_op(Mod0, Fun, Call);

process_op(Mod0, Fun, {case_end, Reg}) ->
  Call = #{
    '$' => e4bif,
    name => e4_casec,
    args => [Reg]
  },
  process_op(Mod0, Fun, Call);

process_op(Mod0, _Fun, {put_tuple, Size, Dst}) ->
  make_result(Mod0, uasm_bytecode:put_tuple(Mod0, Size, Dst));

process_op(Mod0, _Fun, {put, Val}) ->
  Mod1 = register_value(Val, Mod0),
  make_result(Mod1, uasm_bytecode:put(Mod1, Val));

process_op(Mod0, _Fun, {select_val, Src, Fail, Select}) ->
  Mod1 = register_value_jumptab(Select, Mod0),
  make_result(Mod0, uasm_bytecode:select_val(Src, Fail, Select, Mod1));

process_op(Mod0, _Fun, {put_list, H, T, Dst}) ->
  make_result(Mod0, uasm_bytecode:cons(H, T, Dst));

process_op(Mod0, _Fun, {jump, Dst}) ->
  make_result(Mod0, uasm_bytecode:jump(Dst));

process_op(Mod0, _Fun, {trim, N, _Unused}) ->
  make_result(Mod0, uasm_bytecode:trim(N));

process_op(Mod0, _Fun, {init, Y}) ->
  make_result(Mod0, uasm_bytecode:clear_stack(Y));

process_op(Mod0, _Fun, {make_fun2, Label, _Index, _Uniq, NumFree}) ->
  Mod1 = register_value_lambda(Label, NumFree, Mod0),
  make_result(Mod1, uasm_bytecode:make_fun(Label, NumFree, Mod1));

process_op(Mod0, _Fun, {set_tuple_element, Value, Tuple, Pos}) ->
  Mod1 = register_value(Value, Mod0),
  Mod2 = register_value(Tuple, Mod1),
  Mod3 = register_value(Pos, Mod2),
  make_result(Mod3, uasm_bytecode:set_element(Value, Tuple, Pos, Mod3));

process_op(Mod0, _Fun, {label, F}) ->
  make_result(Mod0, uasm_bytecode:label(F));

process_op(Mod0, _Fun, {'%', _Something}) ->
  make_result(Mod0, []);

process_op(_Mod0, Fun, Other) ->
  ?COMPILE_ERROR("Unknown op ~p in source fun ~s", [Other, fun_str(Fun)]).


make_result(Mod0, Code) ->
  #{'$' => processop_result,
    program => Mod0,
    output => Code}.


fun_str(#{'$' := e4fun, name := N, arity := A}) ->
  io_lib:format("~s/~B", [N, A]).


%% @doc Inform the program Mod0 that there will be a value in the program,
%% possibly a literal or an atom, so it will be added to the atom or literal
%% table if needed.
register_value({field_flags, _List}, Mod) -> Mod; % integer needs no reg

register_value(Int, Mod) when is_integer(Int) -> Mod;

register_value({integer, _}, Mod) -> Mod;

register_value({x, _}, Mod) -> Mod;

register_value({y, _}, Mod) -> Mod;

register_value(A, Mod0 = #{'$' := module}) when is_atom(A) ->
  register_value({atom, A}, Mod0);

register_value({atom, A}, Mod0) ->
  store_indexed_something(A, atoms, atom_index, Mod0);

register_value({extfunc, M, F, Arity}, Mod0) ->
  store_indexed_something({M, F, Arity}, imports, import_index, Mod0);

register_value({literal, Lit}, Mod0) ->
  store_indexed_something(Lit, literals, literal_index, Mod0);

register_value(Other, _Mod) ->
  ?COMPILE_ERROR("don't know how to register_value ~p", [Other]).


register_call_target({f, _}, Mod0 = #{'$' := module}) ->
  Mod0;

register_call_target({extfunc, M, F, Arity}, Mod0 = #{'$' := module}) ->
  Mod1 = register_value(M, Mod0),
  Mod2 = register_value(F, Mod1),
  register_value({extfunc, M, F, Arity}, Mod2).


register_valuelist([], Mod0) -> Mod0;
register_valuelist(List, Mod0) ->
  lists:foldl(fun register_value/2, Mod0, List).


%% @doc Adds a jump table to jumptab collection
register_value_jumptab(Select, Mod0 = #{'$' := module}) ->
  if length(Select) rem 2 =/= 0 ->
    ?COMPILE_ERROR("jumptab must have even length: ~p", [Select]);
    true -> ok
  end,
  store_indexed_something(Select, jumptabs, jtabs_index, Mod0).


register_value_lambda(Label, NumFree, Mod0) ->
  store_indexed_something({Label, NumFree}, lambdas, lambda_index, Mod0).


%% @doc Add value Val into orddict with key 'ValKey' in the module 'Mod',
%% key IndexKey stores the counter used as index
store_indexed_something(Val, ValKey, IndexKey, Mod0 = #{'$' := module}) ->
  Dict0 = maps:get(ValKey, Mod0),
  case orddict:find(Val, Dict0) of
    {ok, _} -> Mod0;
    error -> % not found, update the dict
      Index = maps:get(IndexKey, Mod0, 1),
      Dict1 = orddict:store(Val, Index, Dict0),
      Mod0#{
        ValKey => Dict1,
        IndexKey => Index + 1
      }
  end.
