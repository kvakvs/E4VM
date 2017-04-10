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
  emit(add_label(Mod0, Fun, L), []);
process_op(Mod0, _Fun, {func_info, {atom, _Mod}, {atom, FunName}, Arity}) ->
  Mod1 = register_atom(Mod0, FunName),
  emit(Mod1, e4asm_bc:func_info(Mod1, FunName, Arity));
process_op(_Mod0, Fun, Other) ->
  ?COMPILE_ERROR("Unknown op ~p in ~s", [Other, fun_str(Fun)]).

emit(Mod0, Code) -> #{program => Mod0, op_bin => Code}.

fun_str(#{'$' := e4fun, name := N, arity := A}) ->
  io_lib:format("~s:~B", [N, A]).

add_label(Mod0, Fun, L) ->
  Mod0.

%% @doc Inform the program Mod0 that there will be an atom A, it will be
%% added to the atom table if needed.
register_atom(Mod0 = #{'$' := e4mod}, A) when is_atom(A) ->
  Atoms = maps:get(atoms, Mod0, orddict:new()),
  case orddict:find(A, Atoms) of
    {ok, _} -> Mod0;
    error -> % not found, update atom table
      AtomIndex = maps:get(atom_index, Mod0, 1),
      Atoms1 = orddict:store(A, AtomIndex, Atoms),
      Mod0#{atoms => Atoms1, atom_index => AtomIndex + 1}
  end.
