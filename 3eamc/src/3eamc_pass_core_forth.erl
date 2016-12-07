-module('3eamc_pass_core_forth').

%% API
-export([process/1]).

-include_lib("compiler/src/core_parse.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
    module = undefined,
    atom_counter = 0,
    atoms = dict:new(),
    lit_counter = 0,
    literals = dict:new(),
    output = [],                % forth program output
    %% Compile-time state
    stack = []
}).

process(#c_module{name = Name, exports=_Exps, defs=Defs}) ->
    S0 = #state{ module = Name#c_literal.val },
    S1 = process_defs(S0, Defs),
    Output = lists:reverse(S1#state.output),
    io:format("OUT ~p~n", [Output]).

process_defs(State, []) -> State;
process_defs(State, [{#c_var{name = {Name, Arity}},
                     #c_fun{}=Fun} | Defs]) ->
    State1 = f_emit(State, [':', f_format_fun_name(State, Name, Arity)]),
    State2 = process_fun(State1, Fun),
    State3 = f_emit(State2, ';'),
    process_defs(State3, Defs).

process_fun(State, #c_fun{vars=Vars, body=Body}) ->
    %% Assume stack now only has reversed args
    State1 = State#state{stack = lists:reverse(lists:map(fun f_val/1, Vars))},
    State2 = process_code(State1, Body),
    %% Reset stackframe and kill remaining args
    StackSize = length(State2#state.stack),
    case StackSize of
        0 -> f_emit(State2, 'RET');
        _ -> f_emit(State2, [StackSize, 'RETN'])
    end.

process_code(State, []) -> State;
process_code(State, [X | Tail]) ->
    State1 = process_code(State, X),
    process_code(State1, Tail);
process_code(State0, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
    State = pattern_match(State0, Pats),
    case Guard of
        #c_literal{val = true} ->
            process_code(State, Body);
        _ ->
            State1 = f_emit(State, [
                f_expression(Guard), 'IF'
            ]),
            State2 = process_code(State1, Body),
            f_emit(State2, 'THEN')
    end;
process_code(State, #c_case{arg = Arg, clauses = Clauses}) ->
    process_case(State, Arg, Clauses);
process_code(State, #c_literal{val = Value}) ->
    f_emit(State, f_val(Value));
process_code(State, #c_let{vars = Vars, arg = Arg, body = Body}) ->
    State1 = f_emit(State, {'let <<', lists:map(fun f_val/1, Vars)}),
    State2 = process_code(State1, Arg),
    %% From here assume variable is added to the stack
    State3 = lists:foldl(fun(Var, S) -> stack_push(S, Var) end, State2, Vars),

    State4 = process_code(State3, Body),
    %% Assume variable is dropped
    State5 = stack_pop(State4),
    f_emit(State5, '>> endlet');
process_code(State, #c_apply{op = Op, args = Args}) ->
    f_emit(State,
        lists:reverse(lists:map(fun f_val/1, Args))
        ++ ['?apply', f_val(Op)]);
process_code(State, #c_call{module = M, name = N, args = Args}) ->
    f_emit(State,
        lists:reverse(lists:map(fun f_val/1, Args)) ++ [
            f_fun_ref(f_val(M), f_val(N), length(Args))
        ]);
process_code(State, #c_primop{name = Name, args = Args}) ->
    f_emit(State, ['?primop', f_val(Name), Args]);
process_code(State, #c_tuple{es = Es}) ->
    f_emit(State, ['?tuple', lists:map(fun f_val/1, Es)]);
process_code(State, #c_cons{hd = H, tl = T}) ->
    f_emit(State, ['?cons', H, T]);
process_code(State, #c_var{name = N}) ->
    f_emit(State, ['?var', N]);
process_code(_State, X) ->
    io:format("Unknown body part ~p~n", [X]),
    erlang:error(core_ast_error).

f_val(#c_literal{val = Unwrap}) -> f_val(Unwrap);
f_val(#c_var{name = {Fun, Arity}}) -> {funarity, Fun, Arity};
f_val(#c_var{name = N}) -> {var, N};
f_val({_, nil})   -> 'NIL';
f_val({_, Value}) -> {literal, Value};
f_val(X) -> X. % assume nothing left to unwrap

f_emit(State = #state{output = Output}, List) ->
    State#state{output = [List | Output]}.

f_format_fun_name(#state{ module = Mod }, Name, Arity) ->
    f_format_fun_name(Mod, Name, Arity);
f_format_fun_name(Mod, Name, Arity) when is_atom(Mod) ->
    lists:flatten(
        io_lib:format("~s:~s/~p", [Mod, Name, Arity])
    ).

f_fun_ref(#state{ module = Mod }, Name, Arity) ->
    f_format_fun_name(Mod, Name, Arity);
f_fun_ref(Mod, Name, Arity) when is_atom(Mod) ->
    {mfarity, Mod, Name, Arity}.

process_case(State, _Arg, []) -> State;
process_case(State, Arg, [Clause | Remaining]) ->
    State1 = process_code(State, Clause),
    process_case(State1, Arg, Remaining).

f_expression(#c_literal{} = Lit) -> f_val(Lit);
f_expression(Expr) ->
    ['expr', Expr].

stack_push(#state{ stack = S } = State, Value) ->
    State#state{ stack = [Value | S] }.

stack_pop(#state{ stack = [_ | S] } = State) ->
    State#state{ stack = S }.

pattern_match(State, Pats) ->
    f_emit(State, ['?pattern' | Pats]).