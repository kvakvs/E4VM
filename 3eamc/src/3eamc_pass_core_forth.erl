-module('3eamc_pass_core_forth').

%% API
-export([process/1]).

-include_lib("compiler/src/core_parse.hrl").
%%-include_lib("eunit/include/eunit.hrl").

-record(state, {
    module = undefined,
    atom_counter = 0,
    atoms = dict:new(),
    lit_counter = 0,
    literals = dict:new(),
    output = [],                % forth program output
    %% Compile-time state
    stack = [],
    %% Set at function start, current args
    fun_args = [],
    %% A new #scope{} is added when entering case/let etc
    scopes = []
}).
-record(scope, {
    vars = [] % [{var,N}]
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
    ReverseArgs = lists:reverse(lists:map(fun f_val/1, Vars)),
    State1 = State#state{stack = ReverseArgs},
    State2 = scope_push(State1, #scope{vars = ReverseArgs}),
    State3 = process_code(State2, Body),

    %% Reset stackframe and kill remaining args
    StackSize = length(State3#state.stack),
    State4    = scope_pop(State3),

    %% Emit stack cleanup
    case StackSize of
        0 -> f_emit(State4, 'RET');
        _ -> f_emit(State4, [StackSize, 'RETN'])
    end.

process_code(State, []) -> State;
process_code(State, [X | Tail]) ->
    State1 = process_code(State, X),
    process_code(State1, Tail);

process_code(State, #c_case{arg = Arg, clauses = Clauses}) ->
    %% Arg = Tree, Clauses = [Tree]
    lists:foldl(fun(Clause, St) ->
            pattern_match(St, Arg, Clause)
        end, State, Clauses);

process_code(State, #c_literal{val = Value}) ->
    f_emit(State, f_val(Value));

process_code(State, #c_let{vars = Vars, arg = Arg, body = Body}) ->
    ReverseVars = lists:map(fun f_val/1, Vars),
    State1 = f_emit(State, {'let <<', ReverseVars}),
    State2 = scope_push(State1, ReverseVars),
    State3 = process_code(State2, Arg),

    %% From here assume variable is added to the stack
    State10 = lists:foldl(fun(Var, S) -> stack_push(S, Var) end, State3, Vars),

    State20 = process_code(State10, Body),
    %% Assume variable is dropped
    State21 = stack_pop(State20),
    State22 = scope_pop(State21),
    f_emit(State22, '>> endlet');

process_code(State, #c_apply{op = Op, args = Args}) ->
    State1 = f_emit(State,
        lists:reverse(lists:map(fun f_val/1, Args))
        ++ ['?apply']),
    f_store(State1, f_val(Op));

process_code(State, #c_call{module = M, name = N, args = Args}) ->
    f_emit(State,
        lists:reverse(lists:map(fun f_val/1, Args)) ++ [
            f_fun_ref(f_val(M), f_val(N), length(Args))
        ]);

process_code(State, #c_primop{name = Name, args = Args}) ->
    f_emit(State, ['?primop', f_val(Name), Args]);

process_code(State, #c_tuple{es = Es}) ->
    f_emit(State, [
        lists:reverse(lists:map(fun f_val/1, Es)),
        length(Es), 'MAKE-TUPLE'
    ]);

process_code(State, #c_cons{hd = H, tl = T}) ->
    f_emit(State, ['?cons', H, T]);
process_code(State, #c_var{name = N}) ->
    f_emit(State, ['?var', N]);
process_code(State, #c_alias{var = Var, pat = Pat}) ->
    f_emit(State, ['?alias', Var, Pat]);
process_code(_State, X) ->
    io:format("Unknown body part ~p~n", [X]),
    erlang:error(core_ast_error).

%% Builds code to match Arg vs Pats with Guard
%% Pats = [Tree], Guard = Tree, Body = Tree
pattern_match(State, Arg, #c_clause{pats = Pats, guard = Guard, body = Body}) ->
    State1 = f_emit(State, '?pattern <<'),
    State2 = lists:foldl(fun(P, St) -> process_code(St, P) end, State1, Pats),
    f_emit(State2, '>> ?endpat').

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

%%process_case(State, _Arg, []) -> State;
%%process_case(State, Arg, [Clause | Remaining]) ->
%%    State1 = f_emit(State, '?case'),
%%    State2 = process_code(State1, Clause),
%%    process_case(State2, Arg, Remaining).

%%f_expression(#c_literal{} = Lit) -> f_val(Lit);
%%f_expression(Expr) ->
%%    ['expr', Expr].

stack_push(#state{ stack = S } = State, Value) ->
    State#state{ stack = [Value | S] }.

stack_pop(#state{ stack = [_ | S] } = State) ->
    State#state{ stack = S }.

%% Searches the stack for {var,N} and creates if not found, returns its
%% position on the stack at this given moment
stack_find_create(#state{ stack = S } = State, Dst) ->
    case index_of(Dst, S) of
        not_found ->
            {State#state{ stack = [Dst | S] }, {index, 0}};
        Index ->
            {State, {index, Index}}
    end.

scope_push(#state{ scopes = S } = State, Value) ->
    State#state{ scopes = [Value | S] }.

scope_pop(#state{ scopes = [_ | S] } = State) ->
    State#state{ scopes = S }.

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

%% For a value on the stack top emits store instruction to have it in Dst
f_store(State, {funarity, F, Arity}) ->
    %% A function is created
    State;
f_store(State, {var, _} = Dst) ->
    {State1, {index, I}} = stack_find_create(State, Dst),
    f_emit(State1, [I, 'STORE', {comment, Dst}]).
