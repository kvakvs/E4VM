%%% @doc Given simplified assembly AST produce binary E4 VM bytecode.

-module(e4asm_pass_asm).

%% API
-export([compile/1]).

-include_lib("e4compiler/include/e4c.hrl").
-include_lib("e4assembler/include/e4asm.hrl").


compile(#{'$' := e4mod, funs := Funs0} = M0) ->
  %% For each fun, process it
  {Funs1, M1} = lists:foldl(fun compile_fold_helper/2, M0, Funs0),

  M2 = M1#{'$' := e4asmmod, funs => Funs1},

  e4c:debug_write_term("e4asm_pass_asm.txt", M2),
  io:format("~s~n~p~n", [color:redb("E4ASM PASS ASM"), M2]),
  M2.

%% Folding over orddict will give us pairs {Key, Value}
-spec compile_fold_helper(e4fun(), e4mod()) -> e4mod().
compile_fold_helper({FunArity, F0 = #{'$' := e4fun}},
                    Mod0 = #{'$' := e4mod}) ->
  {F1, Mod1} = process_fun(F0, Mod0),

  NewFuns = orddict:store(FunArity, F1, maps:get(funs, Mod0)),
  Mod1#{funs := NewFuns}.

%%%-----------------------------------------------------------------------------

process_fun(Fun = #{'$' := e4fun, code := Code0}, M0) ->
  {Code1, M1} = lists:foldl(
    fun(Op, State) -> process_fun_helper(Fun, Op, State) end,
    #{binary => [], program => M0},
    Code0
  ),
  {Fun#{code := Code1}, M1}.

process_fun_helper(Fun = #{'$' := e4fun},
                   Op0,
                   #{binary := Accum, program := Mod0}) ->
  #{op_bin := Op1,
    program := Mod1} = process_op(Mod0, Fun, Op0),
  #{binary => [Op1 | Accum],
    program => Mod1}.


-spec process_op(e4mod(), e4fun(), tuple() | atom()) -> e4mod().
process_op(Mod0, Fun, {label, L}) ->
  make_emit(add_label(Mod0, Fun, L), []);

process_op(Mod0, _Fun,
           {func_info, {atom, _Mod}, {atom, FunName}, Arity}) ->
  Mod1 = register_value(Mod0, FunName),
  make_emit(Mod1, e4asm_bc:func_info(Mod1, FunName, Arity));

process_op(Mod0, _Fun, #{'$' := e4call, target := Target} = CallOp) ->
  Mod1 = register_call_target(Mod0, Target),
  make_emit(Mod1, e4asm_bc:call(Mod1, CallOp));

process_op(Mod0, _Fun, #{'$' := e4bif, args := Args, name := Name} = BifOp) ->
  Mod1 = register_value(Mod0, Name),
  Mod2 = register_values(Mod1, Args),
  make_emit(Mod1, e4asm_bc:bif(Mod2, BifOp));

process_op(Mod0, _Fun, {allocate, StackNeed, Live}) ->
  make_emit(Mod0, e4asm_bc:allocate(StackNeed, 0, Live));
process_op(Mod0, _Fun, {allocate_zero, StackNeed, Live}) ->
  make_emit(Mod0, e4asm_bc:allocate(StackNeed, 0, Live));
process_op(Mod0, _Fun, {allocate_heap, StackNeed, HeapNeed, Live}) ->
  make_emit(Mod0, e4asm_bc:allocate(StackNeed, HeapNeed, Live));
process_op(Mod0, _Fun, {allocate_heap_zero, StackNeed, HeapNeed, Live}) ->
  make_emit(Mod0, e4asm_bc:allocate(StackNeed, HeapNeed, Live));

process_op(Mod0, _Fun, {get_tuple_element, Tuple, Index, Result}) ->
  Mod1 = register_value(Mod0, Tuple),
  make_emit(Mod1, e4asm_bc:get_element(Tuple, Index, Result));

process_op(Mod0, _Fun, {move, Src, Dst}) ->
  make_emit(Mod0, e4asm_bc:move(Mod0, Src, Dst));

process_op(Mod0, _Fun, {call_fun, Arity}) ->
  make_emit(Mod0, e4asm_bc:call_fun(Mod0, Arity));

process_op(Mod0, _Fun, {kill, Dst}) ->
  make_emit(Mod0, e4asm_bc:set_nil(Mod0, Dst));

process_op(Mod0, _Fun, {test_heap, Need, Live}) ->
  make_emit(Mod0, e4asm_bc:test_heap(Mod0, Need, Live));

process_op(Mod0, _Fun, {put_tuple, Size, Dst}) ->
  make_emit(Mod0, e4asm_bc:put_tuple(Mod0, Size, Dst));
process_op(Mod0, _Fun, {put, Val}) ->
  make_emit(Mod0, e4asm_bc:put(Mod0, Val));

process_op(_Mod0, Fun, Other) ->
  ?COMPILE_ERROR("Unknown op ~p in ~s", [Other, fun_str(Fun)]).


make_emit(Mod0, Code) -> #{program => Mod0, op_bin => Code}.


fun_str(#{'$' := e4fun, name := N, arity := A}) ->
  io_lib:format("~s:~B", [N, A]).


add_label(Mod0, _Fun, _L) ->
  Mod0.


%% @doc Inform the program Mod0 that there will be a value in the program,
%% possibly a literal or an atom, so it will be added to the atom or literal
%% table if needed.
%%
register_value(Mod, Int) when is_integer(Int) -> Mod;
register_value(Mod, {x, _}) -> Mod;
register_value(Mod0 = #{'$' := e4mod}, A) when is_atom(A) ->
  Atoms = maps:get(atoms, Mod0, orddict:new()),
  case orddict:find(A, Atoms) of
    {ok, _} -> Mod0;
    error -> % not found, update atom table
      AtomIndex = maps:get(atom_index, Mod0, 1),
      Atoms1 = orddict:store(A, AtomIndex, Atoms),
      Mod0#{atoms => Atoms1, atom_index => AtomIndex + 1}
  end.


register_call_target(Mod0, {f, _}) -> Mod0;
register_call_target(Mod0, {extfunc, M, F, _Arity}) ->
  Mod1 = register_value(Mod0, M),
  register_value(Mod1, F).


register_values(Mod0, []) -> Mod0;
register_values(Mod0, List) ->
  lists:foldl(fun(A, Mod) -> register_value(Mod, A) end, Mod0, List).
