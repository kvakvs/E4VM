%%% @doc From Erlang Core produces intermediate Forth-ified syntax tree with
%%% variable accesses and scopes marked and constructs created.
-module(e4_c2f).

%% API
-export([process/1, process_code/2, module_new/0, get_code/1,
         emit/2, format_code/2]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4.hrl").

-type core_lhs() :: #c_literal{} | #c_var{} | #c_tuple{}. % TODO: binary, map
-type core_rhs() :: core_lhs().
-type core_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
    | #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
    | #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
    | #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{} | #c_tuple{}
    | #c_values{} | #c_var{}.
-type core_ast() :: core_ast_element() | [core_ast_element()].

module_new() -> #e4module{}.

-spec process(#c_module{}) -> e4block().
process(#c_module{name=_Name, exports=_Exps, defs=Defs}) ->
    %M0 = #e4module{module=Name#c_literal.val},
    Block = e4_forth:block(
        [e4_forth:comment("begin mod")],
        [],
        [e4_forth:comment("end mod")]),
    process_fun_defs(Block, Defs).

add_code(Block = #e4block{code=C}, AddCode) ->
    Block#e4block{code=[AddCode|C]}.

-spec process_fun_defs(e4block(), core_ast()) -> e4block().
process_fun_defs(ModB, []) -> ModB;
process_fun_defs(ModB0, [{#c_var{name={Name, Arity}}, #c_fun{} = Fun} | Remaining]) ->
    Block1 = e4_forth:block(
            [':', format_fun_name(ModB0, Name, Arity)],
            [compile_fun(Fun)],
            [';', e4_forth:comment("end fun ~s/~p", [Name, Arity])]),
    ModB1 = add_code(ModB0, Block1),
    process_fun_defs(ModB1, Remaining).

compile_fun(#c_fun{vars=Vars, body=Body}) ->
    %% Assume stack now only has reversed args
    ReverseArgs = lists:reverse(lists:map(fun make_var/1, Vars)),
    Block0 = e4_forth:block([], [], [], ReverseArgs),
    process_code(Block0, Body).

-spec process_code(e4block(), core_ast()) -> e4block().
process_code(Block, []) -> Block;
process_code(Block0, [CoreOp | Tail]) ->
    Block1 = process_code(Block0, CoreOp),
    process_code(Block1, Tail);

process_code(Block0, #c_case{arg=Arg, clauses=Clauses}) ->
    %% Arg = Tree, Clauses = [Tree]
    lists:foldl(
        fun(Clause, Blk) ->
            pattern_match(Blk, Arg, Clause)
        end, Block0, Clauses);

process_code(Block0, #c_literal{val=Value}) ->
    emit(Block0, e4_forth:lit(Value));

process_code(Block0, #c_let{vars=Vars, arg=Arg, body=Body}) ->
    ReverseVars = lists:map(fun make_var/1, Vars),
    LetBlock = e4_forth:block(
        [e4_forth:comment("begin let")],
        [],
        [e4_forth:comment("end let")],
        Block0#e4block.scope ++ ReverseVars),

    LetBlock1 = process_code(LetBlock, Arg),
    process_code(LetBlock1, Body);

process_code(Block0, #c_apply{op=Op, args=Args}) ->
    emit(Block0,
         lists:reverse(lists:map(fun retrieve/1, Args)) ++ [
             length(Args), retrieve(Op), 'APPLY'
        ]
    );

process_code(Block0, #c_call{module=M, name=N, args=Args}) ->
    emit(Block0,
         lists:reverse(lists:map(fun retrieve/1, Args)) ++ [
            [retrieve(M), retrieve(N), length(Args)]
        ]);

process_code(Block0, #c_primop{name=Name, args=Args}) ->
    emit(Block0, ['?primop', f_val(Name), Args]);

process_code(Block0, #c_tuple{es=Es}) ->
    emit(Block0, e4_forth:tuple(lists:map(fun retrieve/1, Es)));

process_code(Block0, #c_cons{hd=H, tl=T}) ->
    emit(Block0, ['?cons', H, T]);

process_code(Block0, #c_var{name=N}) ->
    emit(Block0, ['?var', N]);

process_code(Block0, #c_alias{var=Var, pat=Pat}) ->
    emit(Block0, ['?alias', Var, Pat]);

process_code(_Block, X) ->
    compile_error("Unknown Core AST piece ~p~n", [X]).

make_var(#c_var{name=N}) -> #e4var{name=N}.

retrieve(#e4var{}=Var) -> e4_forth:retrieve(Var).

%% TODO: this plays no specific role, redo this or rename
f_val(#c_literal{val=Unwrap}) -> e4_forth:lit(Unwrap);
f_val(#c_var{name={Fun, Arity}}) -> #e4funarity{fn=Fun, arity=Arity};
f_val(#c_var{name=N}) -> #e4var{name=N};
f_val(#c_tuple{es=Es}) -> e4_forth:tuple(Es);
f_val({_, nil}) -> e4_forth:nil();
f_val({_, Value}) -> #e4lit{val=Value};
f_val(X) -> X. % assume nothing left to unwrap

%% Takes list [code and lazy elements] which are callable fun/1
%% (fun(State) -> emit... end) and runs the lazy elements combining
%% the output together
-spec emit(e4block(), forth_op() | forth_code()) -> e4block().
emit(Block, AddCode) when not is_list(AddCode) ->
    emit(Block, [AddCode]);
emit(Block, AddCode) ->
    lists:foldl(
        fun(Nested, Blk) when is_list(Nested) ->
                emit(Blk, Nested);
            (ForthOp, Blk = #e4block{code=Code}) ->
                Blk#e4block{code=[ForthOp | Code]}
        end,
        Block,
        AddCode).

format_fun_name(#e4module{module=Mod}, Name, Arity) ->
    format_fun_name(Mod, Name, Arity);
format_fun_name(Mod, Name, Arity) when is_atom(Mod) ->
    #e4mfa{mod=Mod, fn=Name, arity=Arity}.

%%f_fun_ref(#e4module{module=Mod}, #e4lit{val=Name}, Arity) ->
%%    format_fun_name(Mod, Name, Arity);
%%f_fun_ref(#e4lit{val=Mod}, #e4lit{val=Name}, Arity) ->
%%    #e4mfa{mod=Mod, fn=Name, arity=Arity}.

%% Builds code to match Arg vs Pats with Guard
%% Pats = [Tree], Guard = Tree, Body = Tree
pattern_match(State, Args, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
    pattern_match_2(State, Pats, Args, Guard, Body).

%% For each element in Arg match element in Pats, additionally emit the
%% code to check Guard
-spec pattern_match_2(e4module(),
                      Pats :: [core_ast()],
                      Args0 :: core_ast(),
                      Guard :: core_ast(),
                      Body :: core_ast()) -> e4module().
pattern_match_2(State, Pats, Args0, _Guard, _Body) ->
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

%% @doc Given state and left/right side of the match, checks if left-hand
%% variable existed: if so - emits comparison, else introduces a new variable
%% and emits the assignment.
-spec pattern_match_pairs(e4module(), core_lhs(), core_rhs()) -> e4module().
pattern_match_pairs(State, #c_var{name=LhsName}, Rhs) ->
    Lhs = #e4var{name=LhsName},
    case var_exists(State, Lhs) of
        true -> % variable exists, so read it and compare
            emit(State, e4_forth:compare(make_read(State, Lhs), Rhs));
        false -> % introduce variable and use it
            State1 = scope_add_variable(State, Lhs),
            pattern_match_var_versus(State1, Lhs, Rhs)
    end;
pattern_match_pairs(State, #c_literal{val=LhsLit}, Rhs) ->
    emit(State, e4_forth:compare(#e4lit{val=LhsLit}, f_val(Rhs)));
pattern_match_pairs(State, #c_tuple{es=LhsElements}, Rhs) ->
    emit(State, [
        fun(St) ->
            pattern_match_tuple_versus(St, LhsElements, Rhs)
        end
    ]);
pattern_match_pairs(_State, Lhs, Rhs) ->
    compile_error("Match ~9999p versus ~9999p not implemented", [Lhs, Rhs]).

-spec pattern_match_var_versus(e4module(), e4var(), core_rhs()) -> e4module().
pattern_match_var_versus(State, #e4var{} = Lhs, #c_var{name=RhsName}) ->
    Rhs = #e4var{name=RhsName},
    case stack_find_create(State, Lhs) of
        {State1A, {index, Index}} ->
            emit(State1A, [
                e4_forth:lit(Index), make_read(State1A, Rhs), 'STACK-WRITE',
                e4_forth:comment("write ~p", [Lhs])
            ]);
        {State1B, {created, _J}} ->
            emit(State1B, [make_read(State1B, Rhs),
                           e4_forth:comment("created var ~p", [Lhs])])
    end;
pattern_match_var_versus(_State, _Lhs, Rhs) ->
    compile_error("Match var versus ~9999p is not implemented", [Rhs]).

pattern_match_tuple_versus(State, LhsElements, #c_var{name=RhsName}) ->
    %% Iterate a list of [{1,Lhs1}, {2,Lhs2}, ...] and get element from Rhs
    Pairs = lists:zip(LhsElements, lists:seq(1, length(LhsElements))),
    %% check that Rhs is a tuple
    Rhs = #e4var{name = RhsName},
    State1 = emit(State, [
        e4_forth:'if'([make_read(State, Rhs)], []),
        e4_forth:comment("is tuple?", [])
    ]),
    lists:foldl(fun pattern_match_tuple_fold/2, State1, Pairs);
pattern_match_tuple_versus(_State, _Lhs, Rhs) ->
    compile_error("Match var versus ~9999p is not implemented", [Rhs]).

pattern_match_tuple_fold({#c_var{name=LhsName}, Index}, St) ->
    St1 = emit(St, [e4_forth:lit(Index), 'TUPLE-ELEMENT']),
    Lhs = #e4var{name=LhsName},
    emit_store(St1, Lhs).

crlf() -> io:format("~n", []).

compile_error(Format, Args) ->
    E = lists:flatten(io_lib:format(Format, Args)),
    erlang:error(E).

-spec make_read(e4module(), e4var()) -> forth_code().
make_read(State, #e4var{} = Var) ->
    {index, Index} = stack_find(State, Var),
    [e4_forth:lit(Index), 'STACK-READ',
        e4_forth:comment("read ~p", [Var])].

%% @doc Given the code creates color string with the output
format_code([], Accum) -> lists:reverse(Accum);
format_code([H| T], Accum) -> format_code(T, [format_op(H) | Accum]).

format_op(#e4mfa{mod=M, fn=F, arity=A}) ->
    io_lib:format("~s:~s/~p ", [M, F, A]);
format_op(C = #e4funarity{}) -> io_lib:format("~s ", [color:blueb(str(C))]);
format_op(C = #e4var{}) -> io_lib:format("~s ", [color:red(str(C))]);
format_op(#e4lit{val=L}) -> io_lib:format("'~s ", [color:magenta(str(L))]);
format_op(#e4comment{comment=T}) -> io_lib:format("\\ ~s~n", [color:cyan(str(T))]);
format_op(C) when is_atom(C) -> io_lib:format("~s ", [color:white(str(C))]);
format_op(C) when is_integer(C) -> io_lib:format("~s ", [color:red(str(C))]);
format_op(#e4block{before=Before, 'after'=After, code=Code}) ->
    [
        io_lib:format("~s ", [color:magenta("-{ ")]),
        format_code(Before, []),
        format_code(Code, []),
        format_code(After, []),
        io_lib:format("~s ", [color:magenta("}- ")])
    ];
format_op(C) -> io_lib:format("~p ", [C]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).

get_code(#e4module{code=Code}) -> Code.
