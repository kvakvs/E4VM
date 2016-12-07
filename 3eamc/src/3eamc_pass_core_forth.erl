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
    State1 = State#state{stack = lists:reverse(Vars)},
    State2 = process_fun_body(State1, Body),
    %% Reset stackframe and kill remaining args
    StackSize = length(State2#state.stack),
    case StackSize of
        0 -> f_emit(State2, 'RET');
        _ -> f_emit(State2, [StackSize, 'RETN'])
    end.

process_fun_body(State, []) -> State;
process_fun_body(State, [X | Tail]) ->
    State1 = process_fun_body(State, X),
    process_fun_body(State1, Tail);
process_fun_body(State, #c_clause{guard=Guard, body=Body}) ->
    f_emit(State, [clause, Guard, Body]);
process_fun_body(State, #c_case{arg = Arg, clauses = Clauses}) ->
    f_emit(State, f_case(Arg, Clauses));
process_fun_body(State, #c_literal{val = Value}) ->
    f_emit(State, f_literal(Value));
process_fun_body(State, #c_let{arg = Arg, body = Body}) ->
    f_emit(State, ['let', Arg, Body]);
process_fun_body(State, #c_apply{}) ->
    State;
process_fun_body(State, #c_call{}) ->
    State;
process_fun_body(_State, X) ->
    io:format("Unknown body part ~p~n", [X]),
    erlang:error(core_ast_error).

f_literal({_, nil})   -> 'NIL';
f_literal({_, Value}) -> {literal, Value}.

f_emit(State = #state{output = Output}, List) ->
    State#state{output = [List | Output]}.

f_format_fun_name(#state{ module = Mod }, Name, Arity) ->
    lists:flatten(
        io_lib:format("~s:~s/~p", [Mod, Name, Arity])
    ).

f_case(Arg, Clauses) ->
    [].
