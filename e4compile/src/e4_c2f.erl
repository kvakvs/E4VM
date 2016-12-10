-module(e4_c2f).

%% API
-export([process/1, process_code/2, state_new/0, state_output/1,
         scope_push/2, scope_pop/1, emit/2, stack_push/2, stack_pop/1]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4.hrl").
%%-include_lib("eunit/include/eunit.hrl").

-record(state, {
    module=undefined,
    atom_counter=0,
    atoms=dict:new(),
    lit_counter=0,
    literals=dict:new(),
    output=[] :: forth_code(),                % forth program output
    %% Compile-time state
    stack=[] :: [e4var()],
    %% Set at function start, current args
    fun_args=[] :: [e4var()],
    %% A new #scope{} is added when entering case/let etc
    scopes=[] :: [e4scope()]
}).

-import(e4_forth, [
    forth_if/2, forth_if/3, forth_and/1, forth_tuple/1, forth_compare/2,
    forth_literal/1
]).

-type state() :: #state{}.
-type core_lhs() :: #c_literal{} | #c_var{} | #c_tuple{}. % TODO: binary, map
-type core_rhs() :: core_lhs().
-type lazy_emit() :: fun((state()) -> state()).
-type core_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
    | #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
    | #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
    | #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{} | #c_tuple{}
    | #c_values{} | #c_var{}.
-type core_ast() :: core_ast_element() | [core_ast_element()].

-type forth_word() :: atom().
-type forth_op() :: forth_word() | e4comment() | e4lit() | lazy_emit().
-type forth_code() :: [forth_op() | forth_code()].

state_new() -> #state{}.
state_output(#state{output=Out}) -> Out.

process(#c_module{name=Name, exports=_Exps, defs=Defs}) ->
    S0 = #state{module=Name#c_literal.val},
    S1 = process_defs(S0, Defs),
    lists:reverse(S1#state.output).

process_defs(State, []) -> State;
process_defs(State, [{#c_var{name={Name, Arity}}, #c_fun{} = Fun} | Defs]) ->
    State1 = emit(State, [
        [':', format_fun_name(State, Name, Arity),
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
    State2 = scope_push(State1, #e4scope{vars=ReverseArgs}),
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
    emit(State, forth_literal(Value));

process_code(State, #c_let{vars=Vars, arg=Arg, body=Body}) ->
    ReverseVars = lists:map(fun f_val/1, Vars),
    State1 = scope_push(State, #e4scope{vars = ReverseVars}),
    %% From here assume variable is added to the stack
    State2 = lists:foldl(
        fun(#c_var{name=Var}, S) ->
            stack_push(S, #e4var{name=Var})
        end,
        State1, Vars),

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
    emit_store(State1, f_val(Op));

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
    compile_error("Unknown Core AST piece ~p~n", [X]).

f_val(#c_literal{val=Unwrap}) -> forth_literal(Unwrap);
f_val(#c_var{name={Fun, Arity}}) -> #e4funarity{fn=Fun, arity=Arity};
f_val(#c_var{name=N}) -> #e4var{name=N};
f_val(#c_tuple{es=Es}) -> forth_tuple(Es);
f_val({_, nil}) -> 'NIL';
f_val({_, Value}) -> #e4lit{val=Value};
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
                print_op(C),
                St#state{output = [C | Output]}
        end,
        State,
        LazyOrCode).


format_fun_name(#state{module=Mod}, Name, Arity) ->
    format_fun_name(Mod, Name, Arity);
format_fun_name(Mod, Name, Arity) when is_atom(Mod) ->
    #e4mfa{mod=Mod, fn=Name, arity=Arity}.
%%    binary_to_atom(
%%        iolist_to_binary(
%%            io_lib:format("~s:~s/~p", [Mod, Name, Arity])
%%        ), utf8
%%    ).

f_fun_ref(#state{module=Mod}, #e4lit{val=Name}, Arity) ->
    format_fun_name(Mod, Name, Arity);
f_fun_ref(#e4lit{val=Mod}, #e4lit{val=Name}, Arity) ->
    #e4mfa{mod=Mod, fn=Name, arity=Arity}.

stack_push(#state{stack=S} = State, #e4var{} = Value) ->
    State#state{stack=[Value | S]}.

stack_pop(#state{stack=[_ | S]} = State) ->
    State#state{stack=S}.

%% Searches the stack for {var,N} and creates if not found, returns its
%% position on the stack at this given moment
-spec stack_find_create(state(), e4var()) -> {state(), {index, integer()}}.
stack_find_create(#state{stack=S} = State, #e4var{} = Dst) ->
    case index_of(Dst, S) of
        not_found ->
%%            State1 = emit(State, [#e4lit{val = 'NONVALUE'}]),
            {State#state{stack=[Dst | S]}, {index, 0}};
        Index ->
            {State, {index, Index}}
    end.

%% Find without creating
stack_find(#state{stack=S}, #e4var{} = Dst) ->
    case index_of(Dst, S) of
        not_found ->
            compile_error("Local variable ~p not found", [Dst]);
        Index ->
            {index, Index}
    end.

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).

-spec scope_push(state(), e4scope()) -> state().
scope_push(#state{scopes=S} = State, #e4scope{} = NewScope) ->
    State#state{scopes=[NewScope | S]}.

-spec scope_pop(state()) -> state().
scope_pop(#state{scopes=[_ | S]} = State) ->
    State#state{scopes=S}.

-spec var_exists(state() | [e4scope()], e4var()) -> boolean().
var_exists(#state{scopes=Scopes}, #e4var{} = Var) ->
    lists:any(fun(Scope) -> lists:member(Var, Scope#e4scope.vars) end,
              Scopes).

%% Takes top scope and introduces a variable. It is a caller's responsibility
%% to have it on stack from now on.
-spec scope_add_variable(state() | e4scope(), e4var()) -> state() | e4scope().
scope_add_variable(#state{scopes=[]}, Var) ->
    compile_error("There is no scope to add a variable ~p", [Var]);
scope_add_variable(State = #state{scopes=[First | Tail]}, Var) ->
    State#state{scopes=[scope_add_variable(First, Var) | Tail]};
scope_add_variable(Scope = #e4scope{vars=Vars}, Var) ->
    Scope#e4scope{vars=[Var | Vars]}.

%% For a value on the stack top emits store instruction to have it in Dst
emit_store(State, #e4funarity{}) ->
    %% A function is created
    State;
emit_store(State, #e4var{} = Dst) ->
    {State1, {index, I}} = stack_find_create(State, Dst),
    emit(State1, [#e4lit{val = I},
                  'STACK-WRITE',
                  #e4comment{txt=Dst}]).

%% Builds code to match Arg vs Pats with Guard
%% Pats = [Tree], Guard = Tree, Body = Tree
pattern_match(State, Args, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
    pattern_match_2(State, Pats, Args, Guard, Body).

%% For each element in Arg match element in Pats, additionally emit the
%% code to check Guard
-spec pattern_match_2(state(),
                      Pats :: [core_ast()],
                      Args0 :: core_ast(),
                      Guard :: core_ast(),
                      Body :: core_ast()) -> state().
pattern_match_2(State, Pats, Args0, Guard, Body) ->
    %% Convert to list if c_values is supplied
    Args1 = case Args0 of
                #c_values{es=Es} -> Es;
                _ -> Args0
            end,
    %% Convert to list if it was a single tuple
    Args = case is_list(Args1) of
               true -> Args1;
               false -> [Args1]
           end,
    %% Pair args and pats and compare
    PatsArgs = lists:zip(Pats, Args),
    lists:foldl(
        fun({Pat, Arg}, St) -> pattern_match_pairs(St, Pat, Arg) end,
        State, PatsArgs).
%%    Accum1 = lists:reverse([Guard | AccumChecks]),
%%    emit(State1, forth_if(
%%        forth_and(Accum1), [
%%            AccumAssignments, ?Lazy(process_code(St, Body))
%%        ]
%%    )).

%%-spec pattern_match_2_fold({Pat :: core_lhs(), Arg :: core_rhs()},
%%                           {state(), forth_code(), forth_code()})
%%                          -> {state(), forth_code(), forth_code()}.
%%pattern_match_2_fold({Pat, Arg}, {State, AcChecks, AcAssignments}) ->
%%    {State1, Checks, Assignments} = pattern_match_pairs(State, Pat, Arg),
%%    {State1, [Checks | AcChecks], [Assignments | AcAssignments]}.

%% Given state and left/right side of the match, checks if left-hand variable
%% existed: if so - emits comparison, else introduces a new variable and emits
%% assignment
%% Returns {State, Checks, Assignments} - checks should be emitted first, if they
%% did not fail, assignments should be emitted
-spec pattern_match_pairs(state(), core_lhs(), core_rhs()) -> state().
pattern_match_pairs(State, #c_var{name=LhsName}, Rhs) ->
    Lhs = #e4var{name=LhsName},
    case var_exists(State, Lhs) of
        true -> % variable exists, so read it and compare
            emit(State, forth_compare(emit_read(State, Lhs), Rhs));
        false -> % introduce variable and use it
            State1 = scope_add_variable(State, Lhs),
            pattern_match_var_versus(State1, Lhs, Rhs)
    end;
pattern_match_pairs(State, #c_literal{val=LhsLit}, Rhs) ->
    emit(State, forth_compare(#e4lit{val=LhsLit}, f_val(Rhs)));
pattern_match_pairs(State, #c_tuple{es=LhsElements}, Rhs) ->
    emit(State, [
        fun(St) ->
            pattern_match_tuple_versus(St, LhsElements, Rhs)
        end
    ]);
pattern_match_pairs(_State, Lhs, Rhs) ->
    compile_error("Match ~9999p versus ~9999p not implemented", [Lhs, Rhs]).

-spec pattern_match_var_versus(state(), e4var(), core_rhs()) -> state().
pattern_match_var_versus(State, #e4var{} = Lhs, #c_var{name=RhsName}) ->
    Rhs = #e4var{name=RhsName},
    {State1, {index, Index}} = stack_find_create(State, Lhs),
    emit(State1, [
        forth_literal(Index), emit_read(State1, Rhs), 'STACK-WRITE'
    ]);
pattern_match_var_versus(_State, _Lhs, Rhs) ->
    compile_error("Match var versus ~9999p is not implemented", [Rhs]).

pattern_match_tuple_versus(State, LhsElements, #c_var{name=RhsName}) ->
    %% TODO: check that Rhs is a tuple
    %% Iterate a list of [{1,Lhs1}, {2,Lhs2}, ...] and get element from Rhs
    Pairs = lists:zip(LhsElements, lists:seq(1, length(LhsElements))),
    lists:foldl(fun pattern_match_tuple_fold/2, State, Pairs);
pattern_match_tuple_versus(_State, _Lhs, Rhs) ->
    compile_error("Match var versus ~9999p is not implemented", [Rhs]).

pattern_match_tuple_fold({#c_var{name=LhsName}, Index}, St) ->
    St1 = emit(St, [forth_literal(Index), 'TUPLE-ELEMENT']),
    Lhs = #e4var{name=LhsName},
    emit_store(St1, Lhs).

%%pattern_match_tuple_versus(State, #c_tuple{es = Es}, Rhs) ->
%%    %% Unwrap tuple store into a serie of variable stores
%%    %% Rhs can be a single variable: Check if its a tuple, then use element(N,rhs)
%%    %% instead of 1 to 1 matching
%%    case Rhs of
%%        X when is_list(X) ->
%%            lists:foldl(
%%                fun({LhsEl, RhsEl}, St) -> pattern_match_var_versus(St, LhsEl, RhsEl) end,
%%                State,
%%                lists:zip(Es, Rhs));
%%        _ ->
%%            %% TODO: if Rhs is a single variable, replace the code below with lhs = element(rhs)
%%            State
%%    end.

crlf() -> io:format("~n", []).

compile_error(Format, Args) ->
    E = lists:flatten(io_lib:format(Format, Args)),
    erlang:error(E).

-spec emit_read(state(), e4var()) -> forth_code().
emit_read(State, #e4var{} = Var) ->
    {index, Index} = stack_find(State, Var),
    [forth_literal(Index), 'STACK-READ'].

print_op(C = #e4mfa{}) -> io:format("~s ", [color:blueb(str(C))]);
print_op(C = #e4funarity{}) -> io:format("~s ", [color:blueb(str(C))]);
print_op(C = #e4var{}) -> io:format("~s ", [color:green(str(C))]);
print_op(C = #e4lit{}) -> io:format("~s ", [color:magenta(str(C))]);
print_op(C = #e4comment{}) -> io:format("~s ", [color:cyan(str(C))]);
print_op(C) when is_atom(C) -> io:format("~s ", [color:white(str(C))]);
print_op(C) when is_integer(C) -> io:format("~s ", [color:red(str(C))]);
print_op(C) -> io:format("~p ", [C]).

str(X) when is_atom(X) -> lists:flatten(io_lib:format("~s", [X]));
str(X) -> lists:flatten(io_lib:format("~p", [X])).
