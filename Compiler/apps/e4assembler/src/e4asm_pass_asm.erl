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
                   Op,
                   #{binary := Accum, program := Mod0}) ->
  #{binary := Op, program := Mod1} = process_op(Mod0, Fun, Op),
  #{binary => [Op | Accum], program => Mod1}.

process_op(Mod0, Fun, {label, L}) ->
  #{program => add_label(Mod0, Fun, L),
    binary => []};
process_op(_Mod0, Fun, Other) ->
  ?COMPILE_ERROR("Unknown op ~p in ~s", [Other, fun_str(Fun)]).

fun_str(#{'$' := e4fun, name := N, arity := A}) ->
  io_lib:format("~s:~B", [N, A]).

add_label(Mod0, Fun, L) ->
  Mod0.
