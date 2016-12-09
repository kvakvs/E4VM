-module(e4_pass_core_forth).

%% API
-export([process/1, process_code/2, state_new/0]).

-include_lib("compiler/src/core_parse.hrl").
%%-include_lib("eunit/include/eunit.hrl").

-record(state, {
    module=undefined,
    atom_counter=0,
    atoms=dict:new(),
    lit_counter=0,
    literals=dict:new(),
    output=[],                % forth program output
    %% Compile-time state
    stack=[],
    %% Set at function start, current args
    fun_args=[],
    %% A new #scope{} is added when entering case/let etc
    scopes=[]
}).
-record(scope, {
    vars=[] % [{var,N}]
}).

-import(e4_forth, [forth_if/2, forth_if/3, forth_and/1, forth_tuple/1,
    forth_compare/2]).

-type state() :: #state{}.
-type lhs() :: #c_literal{} | #c_var{} | #c_tuple{}. % TODO: binary, map
-type rhs() :: lhs().
-type lazy_emit() :: fun((state()) -> state()).
-type core_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
    | #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
    | #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
    | #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{} | #c_tuple{}
    | #c_values{} | #c_var{}.
-type core_ast() :: core_ast_element() | [core_ast_element()].

-type forth_word() :: atom().
-type forth_op() :: forth_word() | integer() | {comment, _} | integer() | lazy_emit().
-type forth_code() :: [forth_op()] | [forth_code()].

state_new() -> #state{}.

process(#c_module{name=Name, exports=_Exps, defs=Defs}) ->
    S0 = #state{module=Name#c_literal.val},
    S1 = process_defs(S0, Defs),
    lists:reverse(S1#state.output).

process_defs(State, []) -> State;
process_defs(State, [{#c_var{name={Name, Arity}}, #c_fun{} = Fun} | Defs]) ->
    State1 = emit(State, [
        [':', f_format_fun_name(State, Name, Arity),
            fun(St) -> crlf(), St end,
            fun(St) -> compile_fun(St, Fun) end,
         ';']
    ]),
    crlf(), crlf(),
    process_defs(State1, Defs).

compile_fun(State, #c_fun{vars=Vars, body=Body}) ->
    %% Assume stack now only has reversed args
    ReverseArgs = lists:reverse(lists:map(fun f_val/1, Vars)),
    State1 = State#state{stack=ReverseArgs},
    State2 = scope_push(State1, #scope{vars=ReverseArgs}),
    State3 = process_code(State2, Body),

    %% Reset stackframe and kill remaining args
    StackSize = length(State3#state.stack),
    State4 = scope_pop(State3),

    %% Emit stack cleanup
    case StackSize of
        0 -> emit(State4, 'RET');
        _ -> emit(State4, [[StackSize, 'RETN']])
    end.

-spec process_code(state(), core_ast()) -> state().
process_code(State, []) -> State;
process_code(State, [X | Tail]) ->
    State1 = process_code(State, X),
    process_code(State1, Tail);

process_code(State, #c_case{arg=Arg, clauses=Clauses}) ->
    %% Arg = Tree, Clauses = [Tree]
    lists:foldl(fun(Clause, St) ->
        pattern_match(St, Arg, Clause)
                end, State, Clauses);

process_code(State, #c_literal{val=Value}) ->
    emit(State, f_val(Value));

process_code(State, #c_let{vars=Vars, arg=Arg, body=Body}) ->
    ReverseVars = lists:map(fun f_val/1, Vars),
    State1 = scope_push(State, ReverseVars),
    %% From here assume variable is added to the stack
    State2 = lists:foldl(fun(Var, S) -> stack_push(S, Var) end, State1, Vars),

    State10 = emit(State2, [
        '?LET', ReverseVars,
        fun(St) -> process_code(St, Arg) end
    ]),

    State20 = process_code(State10, Body),
    %% Assume variable is dropped
    State21 = stack_pop(State20),
    crlf(),
    scope_pop(State21);

process_code(State, #c_apply{op=Op, args=Args}) ->
    State1 = emit(State,
        lists:reverse(lists:map(fun f_val/1, Args))
        ++ [length(Args), 'APPLY']),
    f_store(State1, f_val(Op));

process_code(State, #c_call{module=M, name=N, args=Args}) ->
    emit(State,
        lists:reverse(lists:map(fun f_val/1, Args)) ++ [
            f_fun_ref(f_val(M), f_val(N), length(Args))
        ]);

process_code(State, #c_primop{name=Name, args=Args}) ->
    emit(State, ['?primop', f_val(Name), Args]);

process_code(State, #c_tuple{es=Es}) ->
    emit(State, forth_tuple(lists:map(fun f_val/1, Es)));

process_code(State, #c_cons{hd=H, tl=T}) ->
    emit(State, ['?cons', H, T]);
process_code(State, #c_var{name=N}) ->
    emit(State, ['?var', N]);
process_code(State, #c_alias{var=Var, pat=Pat}) ->
    emit(State, ['?alias', Var, Pat]);
process_code(_State, X) ->
    io:format("Unknown body part ~p~n", [X]),
    erlang:error(core_ast_error).

f_val(#c_literal{val=Unwrap}) -> f_val(Unwrap);
f_val(#c_var{name={Fun, Arity}}) -> {funarity, Fun, Arity};
f_val(#c_var{name=N}) -> {var, N};
f_val(#c_tuple{es = Es}) -> forth_tuple(Es);
f_val({_, nil}) -> 'NIL';
f_val({_, Value}) -> {literal, Value};
f_val(X) -> X. % assume nothing left to unwrap

%% Takes list [code and lazy elements] which are callable fun/1
%% (fun(State) -> emit... end) and runs the lazy elements combining
%% the output together
-spec emit(state(), forth_op() | forth_code()) -> state().
emit(State, LazyOrCode) when not is_list(LazyOrCode) ->
    emit(State, [LazyOrCode]);
emit(State, LazyOrCode) ->
    lists:foldl(
        fun(Lazy, St) when is_function(Lazy,1) ->
                Lazy(St);
            (Nested, St) when is_list(Nested) ->
                emit(St, Nested);
            (C, St = #state{output=Output}) ->
                io:format("~p ", [C]),
                St#state{output = [C | Output]}
        end,
        State,
        LazyOrCode).


f_format_fun_name(#state{module=Mod}, Name, Arity) ->
    f_format_fun_name(Mod, Name, Arity);
f_format_fun_name(Mod, Name, Arity) when is_atom(Mod) ->
    iolist_to_binary(
        io_lib:format("~s:~s/~p", [Mod, Name, Arity])
    ).

f_fun_ref(#state{module=Mod}, Name, Arity) ->
    f_format_fun_name(Mod, Name, Arity);
f_fun_ref(Mod, Name, Arity) when is_atom(Mod) ->
    {mfarity, Mod, Name, Arity}.

stack_push(#state{stack=S} = State, Value) ->
    State#state{stack=[Value | S]}.

stack_pop(#state{stack=[_ | S]} = State) ->
    State#state{stack=S}.

%% Searches the stack for {var,N} and creates if not found, returns its
%% position on the stack at this given moment
stack_find_create(#state{stack=S} = State, Dst) ->
    case index_of(Dst, S) of
        not_found ->
            {State#state{stack=[Dst | S]}, {index, 0}};
        Index ->
            {State, {index, Index}}
    end.

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).


scope_push(#state{scopes=S} = State, Value) ->
    State#state{scopes=[Value | S]}.

scope_pop(#state{scopes=[_ | S]} = State) ->
    State#state{scopes=S}.

scope_find(#state{scopes=S}, Var) ->
    scope_find(S, Var);
scope_find([], _Var) -> false;
scope_find([#scope{vars=Vars} | Tail], Var) ->
    case lists:member(Var, Vars) of
        true -> true;
        false -> scope_find(Tail, Var)
    end.

%% Takes top scope and introduces a variable. It is a caller's responsibility
%% to have it on stack from now on.
scope_add_variable(State = #state{scopes=[First | Tail]}, Var) ->
    State#state{scopes=[scope_add_variable(First, Var) | Tail]};
scope_add_variable(Scope = #scope{vars=Vars}, Var) ->
    Scope#scope{vars=[Var | Vars]}.

%% For a value on the stack top emits store instruction to have it in Dst
f_store(State, {funarity, _F, _Arity}) ->
    %% A function is created
    State;
f_store(State, {var, _} = Dst) ->
    {State1, {index, I}} = stack_find_create(State, Dst),
    emit(State1, [I, 'STORE', {comment, Dst}]).

%% Builds code to match Arg vs Pats with Guard
%% Pats = [Tree], Guard = Tree, Body = Tree
pattern_match(State, Args, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
    f_match(State, Pats, Args, Guard, Body).
%%    State2 = lists:foldl(fun(P, St) -> process_code(St, P) end, State1, Pats),
%%    f_emit(State2, '>> ?endmatch').

%% For each element in Arg match element in Pats, additionally emit the
%% code to check Guard
f_match(State, Pats, Args0, Guard, Body) ->
    %% Convert to list if c_values is supplied
    Args1 = case Args0 of #c_values{es=Es} -> Es; _ -> Args0 end,
    %% Convert to list if it was a single tuple
    Args = case is_list(Args1) of true -> Args1; false -> [Args1] end,

    %% Pair args and pats and compare
%%    io:format("f_match Arg ~p~n  Pats ~p~n  Guard ~p~n", [Args, Pats, Guard]),
    {State1, AccumChecks, AccumAssignments} = lists:foldl(
        fun({Pat, Arg}, {St, AcChecks, AcAssignments}) ->
            {St1, Checks, Assignments} = f_match_one(St, Pat, Arg),
            {St1, [Checks | AcChecks], [Assignments | AcAssignments]}
        end,
        {State, [], []},
        lists:zip(Pats, Args)
    ),
    Accum1 = lists:reverse([Guard | AccumChecks]),
    emit(State1, forth_if(forth_and(Accum1), [
        AccumAssignments,
        fun(LazyState) -> process_code(LazyState, Body) end
    ])).

%% Given state and left/right side of the match, checks if left-hand variable
%% existed: if so - emits comparison, else introduces a new variable and emits
%% assignment
%% Returns {State, Checks, Assignments} - checks should be emitted first, if they
%% did not fail, assignments should be emitted
f_match_one(State, Lhs, Rhs) ->
    case scope_find(State, Lhs) of
        true ->
            %% TODO: compare harder
            {State, forth_compare(f_val(Lhs), f_val(Rhs)), []}; % compare
        false -> % introduce variable and use it
            %% TODO: This should be emitted AFTER the pattern check if it did not fail
            State1 = scope_add_variable(State, Lhs),
            case classify_lhs(Lhs) of
                variable ->
                    {State1, [], [make_store(State1, Lhs, Rhs)]};
                tuple ->
                    {State1, [], [make_store(State1, Lhs, Rhs)]};
                literal ->
                    {State1, [forth_compare(f_val(Lhs), f_val(Rhs))], []}
            end
    end.

-spec make_store(#state{}, lhs(), rhs()) -> forth_code().
make_store(_State, #c_literal{}, _Rhs) ->
    erlang:error({error, "can't store with a literal on the left hand side"});
make_store(_State, #c_var{} = Lhs, Rhs) ->
    %% for introduced variable this is basically a push
    %% TODO: maybe push a nonvalue when a variable is introduced and later assign it
    %% ... to preserve stack position
    %% TODO: handle {Var,...} = X, [Var,...] = X
    [{comment, 'assign', f_val(Lhs)}, f_val(Rhs)];
make_store(State, #c_tuple{es = Es}, Rhs) ->
    %% Unwrap tuple store into a serie of variable stores
    %% Rhs can be a single variable: Check if its a tuple, then use element(N,rhs)
    %% instead of 1 to 1 matching
    case Rhs of
        X when is_list(X) ->
            [make_store(State, LhsElement, RhsElement)
                || {LhsElement, RhsElement} <- lists:zip(Es, Rhs)];
        _ ->
            %% TODO: if Rhs is a single variable, replace the code below with lhs = element(rhs)
            []
    end.

classify_lhs(#c_literal{}) -> literal;
classify_lhs(#c_tuple{}) -> tuple;
classify_lhs(#c_var{}) -> variable.

crlf() -> io:format("~n", []).