%%% @doc From Core Erlang produces intermediate Core Forth syntax tree with
%% scopes and variable accesses marked
-module(e4_core_cforth).

%% API
-export([process/1, process_code/2, module_new/0, get_code/1,
    emit/2, format_core_forth/2]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4_cf.hrl").

-import(e4, [compile_error/2]).

-type cerl_lhs() :: #c_literal{} | #c_var{} | #c_tuple{}. % TODO: binary, map
-type cerl_rhs() :: cerl_lhs().
-type cerl_ast_element() :: #c_literal{} | #c_alias{} | #c_apply{} | #c_binary{}
    | #c_bitstr{} | #c_call{} | #c_case{} | #c_catch{} | #c_clause{}
    | #c_cons{} | #c_fun{} | #c_let{} | #c_letrec{} | #c_map{} | #c_map_pair{}
    | #c_module{} | #c_primop{} | #c_receive{} | #c_seq{} | #c_try{}
    | #c_tuple{} | #c_values{} | #c_var{}.
-type cerl_ast() :: cerl_ast_element() | [cerl_ast_element()].
-type cerl_module() :: #c_module{}.

module_new() -> #cf_mod{}.

-spec process(cerl_module()) -> cf_block().
process(#c_module{name=_Name, exports=_Exps, defs=Defs}) ->
    %M0 = #e4module{module=Name#c_literal.val},
    Block = e4_cf:block(
        [e4_cf:comment("begin mod")],
        [],
        [e4_cf:comment("end mod")]),
    Out = process_fun_defs(Block, Defs),
%%    io:format("~p~n", [Out]),
    io:format("~s~n", [format_core_forth(Out, 0)]),
    Out.

add_code(Block = #cf_block{code=C}, AddCode) ->
    Block#cf_block{code=[AddCode| C]}.

-spec process_fun_defs(cf_block(), cerl_ast()) -> cf_block().
process_fun_defs(ModB, []) -> ModB;
process_fun_defs(ModB0, [{#c_var{name={Name, Arity}},
                          #c_fun{} = Fun} | Remaining]) ->
    Block1 = e4_cf:block(
            [':', format_fun_name(Name, Arity)],
            [compile_fun(Fun)],
            [';', e4_cf:comment("end fun ~s/~p", [Name, Arity])]),
    ModB1 = add_code(ModB0, Block1),
    process_fun_defs(ModB1, Remaining).

compile_fun(#c_fun{vars=Vars, body=Body}) ->
    %% Assume stack now only has reversed args
    ReverseArgs = lists:reverse(lists:map(fun e4_cf:var/1, Vars)),
    Block0 = e4_cf:block([], [], [], ReverseArgs),
    Block1 = lists:foldl(
        fun(V, Blk) -> emit(Blk, e4_cf:mark_new_arg(V)) end,
        Block0,
        ReverseArgs),
    process_code(Block1, Body).

-spec process_code(cf_block(), cerl_ast()) -> cf_block().
process_code(Block, []) -> Block;
process_code(Block0, [CoreOp | Tail]) ->
    Block1 = process_code(Block0, CoreOp),
    process_code(Block1, Tail);

process_code(Block0, #c_case{arg=Arg, clauses=Clauses}) ->
    %% Arg = Tree, Clauses = [Tree]
    Case0 = e4_cf:block(
        [e4_cf:comment("begin case(~s)", [format_vars(Arg)])],
        [],
        [e4_cf:comment("end case")]),
    Case1 = lists:foldl(
        fun(Clause, Blk) ->
            Blk1 = pattern_match(Blk, Arg, Clause),
            process_code(Blk1, Clause)
        end,
        Case0,
        Clauses),
    emit(Block0, Case1);

process_code(Block0, #c_clause{body=Body}) ->
    ClauseBlock0 = e4_cf:block(
        [e4_cf:comment("begin clause")],
        [],
        [e4_cf:comment("end clause")]),
    ClauseBlock1 = process_code(ClauseBlock0, Body),
    emit(Block0, ClauseBlock1);

process_code(Block0, #c_literal{val=Value}) ->
    emit(Block0, e4_cf:lit(Value));

process_code(Block0, #c_let{vars=Vars, arg=Arg, body=Body}) ->
    % ReverseVars = lists:map(fun e4_cf:var/1, Vars),
    LetBlock = e4_cf:block(
        [e4_cf:comment("begin let(~s)", [format_vars(Vars)])],
        [],
        [e4_cf:comment("end let")],
        Block0#cf_block.scope
        %% ++ ReverseVars
    ),

    %% LetBlock1 = process_code(LetBlock, Arg),
    LetBlock1 = lists:foldl(
        fun(V, Blk) ->
            V1 = e4_cf:var(V),
            Blk1 = emit(Blk, e4_cf:mark_new_var(V1)),
            scope_add_var(Blk1, V1)
        end,
        LetBlock,
        Vars),
    LetBlock2 = pattern_match_pairs(LetBlock1, Vars, Arg),
    LetBlock3 = process_code(LetBlock2, Body),
    emit(Block0, LetBlock3);

process_code(Block0, #c_apply{op=Op, args=Args}) ->
    emit(Block0,
         lists:reverse(lists:map(fun e4_cf:retrieve/1, Args)) ++ [
             e4_cf:lit(length(Args)),
             e4_cf:retrieve(Op),
             'APPLY'
        ]
    );

process_code(Block0, #c_call{module=M, name=N, args=Args}) ->
    emit(Block0,
         lists:reverse(lists:map(fun e4_cf:retrieve/1, Args)) ++ [
            [#cf_mfarity{mod=e4_cf:retrieve(M),
                         fn=e4_cf:retrieve(N),
                         arity=length(Args)}]
         ]);

process_code(Block0, #c_primop{name=Name, args=Args}) ->
    emit(Block0, lists:reverse(lists:map(fun e4_cf:retrieve/1, Args)) ++ [
        e4_cf:retrieve(Name),
        'PRIMOP'
    ]);

process_code(Block0, #c_tuple{es=Es}) ->
    emit(Block0, e4_cf:tuple(lists:map(fun e4_cf:retrieve/1, Es)));

process_code(Block0, #c_cons{hd=H, tl=T}) ->
    emit(Block0, ['?cons', H, T]);

process_code(Block0, #c_var{name=N}) ->
    emit(Block0, [e4_cf:comment("retrieve ~p", [N])]);

process_code(Block0, #c_alias{var=Var, pat=Pat}) ->
    emit(Block0, ['?alias', Var, Pat]);

process_code(_Block, X) ->
    compile_error("E4Cerl: Unknown Core AST piece ~p~n", [X]).

-spec emit(Block :: cf_block(), Code :: cf_op() | cf_code()) -> cf_block().
emit(Block, AddCode) when not is_list(AddCode) ->
    emit(Block, [AddCode]);
emit(Block, AddCode) ->
    lists:foldl(
        fun(Nested, Blk) when is_list(Nested) ->
                emit(Blk, Nested);
            (ForthOp, Blk = #cf_block{code=Code}) ->
                %% TODO: Fix me i'm slow
                Blk#cf_block{code=Code ++ [ForthOp]}
        end,
        Block,
        AddCode).

format_fun_name(Name, Arity) ->
    #cf_mfarity{mod='.', fn=Name, arity=Arity}.

%%format_fun_name(Mod, Name, Arity) when is_atom(Mod) ->
%%    #cf_mfarity{mod=Mod, fn=Name, arity=Arity}.

%%f_fun_ref(#e4module{module=Mod}, #e4lit{val=Name}, Arity) ->
%%    format_fun_name(Mod, Name, Arity);
%%f_fun_ref(#e4lit{val=Mod}, #e4lit{val=Name}, Arity) ->
%%    #e4mfa{mod=Mod, fn=Name, arity=Arity}.

%% Builds code to match Arg vs Pats with Guard
%% Pats = [Tree], Guard = Tree, Body = Tree
pattern_match(State, Args, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
    pattern_match2(State, Pats, Args, Guard, Body).

%% For each element in Arg match element in Pats, additionally emit the
%% code to check Guard
-spec pattern_match2(cf_block(),
                      Pats :: [cerl_ast()], Args0 :: cerl_ast(),
                      Guard :: cerl_ast(), Body :: cerl_ast()) -> cf_block().
pattern_match2(Block0, Pats, Args0, _Guard, _Body) ->
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
        fun({Pat, Arg}, Blk) -> pattern_match_pairs(Blk, Pat, Arg) end,
        Block0, PatsArgs).

var_exists(#cf_block{scope=Scope}, #cf_var{} = Var) ->
    lists:member(Var, Scope).

%% @doc Given state and left/right side of the match, checks if left-hand
%% variable existed: if so - emits comparison, else introduces a new variable
%% and emits the assignment.
-spec pattern_match_pairs(cf_block(),
                          Lhs :: cerl_lhs() | cf_var(),
                          Rhs :: cerl_rhs() | cf_var())
                         -> cf_block().
pattern_match_pairs(Block0, #c_var{name=LhsName}, Rhs) -> % unwrap left
    pattern_match_pairs(Block0, e4_cf:var(LhsName), Rhs);

pattern_match_pairs(Block0, Lhs, #c_var{name=RhsName}) -> % unwrap right
    pattern_match_pairs(Block0, Lhs, e4_cf:var(RhsName));

pattern_match_pairs(Block0 = #cf_block{scope=Scope0}, #cf_var{}=Lhs, Rhs) ->
    case var_exists(Block0, Lhs) of % if have variable in scope
        true -> % variable exists, so read it and compare
            emit(Block0, [
                e4_cf:match_two_values(e4_cf:retrieve(Lhs), Rhs),
                e4_cf:comment("match two known")
            ]);
        false -> % introduce variable and use it
            Block1 = Block0#cf_block{scope = Scope0},
            pattern_match_var_versus(Block1, Lhs, Rhs)
    end;
pattern_match_pairs(Block0, #c_literal{val=LhsLit}, Rhs) ->
    emit(Block0, e4_cf:match_two_values(
        e4_cf:lit(LhsLit),
        e4_cf:retrieve(Rhs)
    ));
pattern_match_pairs(Block0 = #cf_block{}, #c_tuple{es=LhsElements}, Rhs) ->
    pattern_match_tuple_versus(Block0, LhsElements, Rhs);
pattern_match_pairs(Block0, [#c_var{}=Lhs0], Rhs) ->
    Lhs = e4_cf:var(Lhs0),
    Block1 = process_code(Block0, Rhs), % assume Rhs leaves 1 value on stack?
    Block2 = case var_exists(Block1, Lhs) of
        true -> emit(Block1, [e4_cf:store(Lhs)]); % Store Rhs result into Lhs
        false ->
            emit(Block1, [
                e4_cf:mark_new_var(Lhs),
                e4_cf:store(Lhs),
                e4_cf:comment("introduce variable")
            ])
    end,
    scope_add_var(Block2, Lhs);
pattern_match_pairs(_State, Lhs, Rhs) ->
    compile_error("E4Cerl: Match ~9999p versus ~9999p not implemented",
        [Lhs, Rhs]).

-spec pattern_match_var_versus(Block :: cf_block(),
                               cf_var(), cerl_rhs()|cf_var())
                              -> cf_block().
pattern_match_var_versus(Block0, #c_var{name=LhsName}, Rhs) -> % unwrap left
    pattern_match_var_versus(Block0, e4_cf:var(LhsName), Rhs);

pattern_match_var_versus(Block0, Lhs, #c_var{name=RhsName}) -> % unwrap right
    pattern_match_var_versus(Block0, Lhs, e4_cf:var(RhsName));

pattern_match_var_versus(Block0, #cf_var{} = Lhs, Rhs) ->
    case var_exists(Block0, Lhs) of
        true -> % var exists, so compare
            emit(Block0, [
                e4_cf:comment("compare-match ~p = ~p", [Lhs, Rhs]),
                e4_cf:unless(
                    [e4_cf:equals(e4_cf:retrieve(Lhs), e4_cf:retrieve(Rhs))],
                    e4_cf:block(['BADMATCH'])
                )
                %% TODO: Use fail label instead of badmatch if possible
            ]);
        false -> % var did not exist, so copy-assign
            Block1 = emit(Block0, [
                e4_cf:comment("assign-match ~s = ~s",
                    [
                        format_vars([Lhs]), format_vars([Rhs])
                    ]),
                e4_cf:mark_alias(Lhs, Rhs)
            ]),
            scope_add_var(Block1, Lhs)
    end;
pattern_match_var_versus(_Blk, L, R) ->
    compile_error("E4Cerl: Match var ~9999p against ~9999p is not implemented",
        [L, R]).

pattern_match_tuple_versus(Block0, LhsElements, #c_var{}=Rhs) ->
    pattern_match_tuple_versus(Block0, LhsElements, e4_cf:var(Rhs));
pattern_match_tuple_versus(Block0, LhsElements, Rhs) ->
    %% Iterate a list of [{1,Lhs1}, {2,Lhs2}, ...] and get element from Rhs
    LhsPairs = lists:zip(lists:seq(1, length(LhsElements)),
                      LhsElements),
    %% check that Rhs is a tuple
    Block1 = emit(Block0, [
        e4_cf:unless(
            [e4_cf:retrieve(Rhs), e4_cf:lit(length(LhsElements)), 'IS-TUPLE'],
            e4_cf:block(['BADARG'])
        )
    ]),
    %% For all variables in the left introduce a variable and create
    %% variable assignment
    lists:foldl(
        fun({Index, Lhs1}, Blk0) ->
            Blk1 = emit(Blk0, [
                e4_cf:mark_new_var(Lhs1),
                'DUP',
                e4_cf:element(Index, #cf_stack_top{})
            ]),
            pattern_match_var_versus(Blk1, Lhs1, #cf_stack_top{})
        end,
        emit(Block1, [e4_cf:retrieve(Rhs)]),
        LhsPairs).
%%pattern_match_tuple_versus(_State, _Lhs, Rhs) ->
%%    compile_error("E4Cerl: Match tuple vs ~9999p is not implemented", [Rhs]).

get_code(#cf_mod{code=Code}) -> Code.

i(I) -> lists:duplicate((I-1) * 4, 32).

format_core_forth(L, Indent) when is_list(L) ->
    [format_core_forth(Item, Indent) || Item <- L];
format_core_forth(#cf_block{before=B, scope=_S, code=C, 'after'=A}, Indent) ->
    [format_core_forth(B, Indent+1),
     format_core_forth(C, Indent+1),
     format_core_forth(A, Indent+1)];
format_core_forth(C, Indent) ->
    io_lib:format("~s~s~n", [i(Indent), format_op(C)]).

format_op(#cf_var{name=V}) -> color:blueb(str(V));
format_op(W) when is_atom(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);
%%format_op(LitInt) when is_integer(LitInt) ->
%%    io_lib:format("'~s", [color:magenta(str(LitInt))]);
format_op(#cf_lit{val=L}) ->
    io_lib:format("'~s", [color:magenta(str(L))]);
format_op(#cf_retrieve{var=V}) ->
    io_lib:format("~s(~s)", [color:green("retrieve"), format_op(V)]);
format_op(#cf_store{var=#cf_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:red("store"), format_op(V)]);
format_op(#cf_new_var{var=#cf_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:blackb("var"), format_op(V)]);
format_op(#cf_new_arg{var=#cf_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:blackb("arg"), format_op(V)]);
format_op(#cf_alias{var=V, existing=Alt}) ->
    io_lib:format("~s(~s=~s)", [
        color:blackb("alias"), format_op(V), format_op(Alt)]);
format_op(#cf_comment{comment=C}) ->
    io_lib:format("~s ~s",
                  [color:blackb("\\"), color:blackb(C)]);
format_op(#cf_mfarity{mod=M, fn=F, arity=A}) ->
    io_lib:format("~s~s,~s,~s~s",
                  [
                      color:magentab("MFA("),
                      format_op(M),
                      format_op(F),
                      str(A),
                      color:magentab(")")
                  ]);
format_op(#cf_var{}=Var) ->
    io_lib:format("~s", [format_op(Var)]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).

scope_add_var(Block = #cf_block{scope=Scope}, Var) ->
    Block#cf_block{scope=ordsets:add_element(Var, Scope)}.

format_vars(#c_values{es=Vars}) -> format_vars(Vars);
format_vars(Vars) ->
    Names = lists:map(
        fun(#cf_var{name=N}) -> atom_to_list(N);
           (#c_var{name=N}) -> atom_to_list(N);
            (#cf_stack_top{}) -> "$stack-top"
        end,
        Vars),
    string:join(Names, ", ").
