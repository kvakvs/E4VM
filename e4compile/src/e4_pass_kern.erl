%%% @doc Pass 1: From Core Erlang produces intermediate Forth syntax tree with
%% scopes defined and basic blocks (code constructs) marked
-module(e4_pass_kern).

%% API
-export([process/1]).

-include_lib("compiler/src/core_parse.hrl").

-include("e4_kernel_erl.hrl").
-include("e4_forth.hrl").
-include("e4.hrl").
-import(e4_f1, [emit/2]).

-record(match_ctx, {
    match_vars= [] :: [k_var()],    % vars which appeared in #k_match{}
    select_var,                     % focused var which appeared #k_select{}
    type :: k_tuple | k_atom | k_int
}).
-type match_ctx() :: #match_ctx{}.
%%-type match_elem_group() :: k_match() | k_alt() | k_select() | k_type_clause()
%%                          | k_val_clause() | k_seq() | list().

%%module_new() -> #f_mod_pass1{}.

-spec process(k_mdef()) -> f_block().
process(#k_mdef{name=Name, exports=Exps, attributes=_Attr, body=Body}) ->
    Block0 = e4_f1:block(
        [
            e4_f1:comment("begin mod ~s", [Name]),
            e4_f1:include("forth-lib/e4core.e4"),
            e4_f1:comment("MODULE ~s", [Name]),
            lists:map(fun process_export/1, Exps)
        ],
        [],
        [e4_f1:comment("end mod ~s", [Name])]),
    Out = process_code(Block0, #match_ctx{}, Body),
    file:write_file("pass1.txt", iolist_to_binary(io_lib:format("~p", [Out]))),
%%    io:format("~s~n~s~n",
%%              [color:on_white(color:black(" PASS 1 ")),
%%               e4_print_ic:format_ic(Out, 0)]),
    Out.

process_export({F, Arity}) -> [e4_f1:export(F, Arity)].

%%p(L) -> e4_print_ic:format_core_forth(L, 0).

-spec process_code(f_block(), match_ctx(), k_ast()) -> f_block().
process_code(Block = #f_block{}, #match_ctx{}, []) -> Block;

%% Special case returning a var if it comes last in the list
process_code(Block = #f_block{}, #match_ctx{}, #k_var{}=Var) ->
    emit(Block, e4_f1:eval(Var));

%% Special case 2, a var is not allowed otherwise
%%process_code(Block = #f_block{}, #match_ctx{}, #k_var{}=Var) ->
%%    ?COMPILE_ERROR("A free-floating var is not allowed in the input: ~s",
%%        [?COLOR_TERM(red, Var)]);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{}, [CoreOp | Tail]) ->
    Block1 = process_code(Block0, Ctx, CoreOp),
    process_code(Block1, Ctx, Tail);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_fdef{func=Name, arity=Arity, vars=Vars, body=Body}) ->
%%    io:format("k_fdef name=~s vars~p~n", [Name, Vars]),
    Block1 = e4_f1:block(
        [<<":">>, format_fun_name(Name, Arity)],
        [],
        [<<";">>, e4_f1:comment("end fun ~s/~p", [Name, Arity])]),
    Block2 = lists:foldl(
        fun(A, Blk) -> emit(Blk, e4_f1:mark_new_arg(A)) end,
        Block1, Vars
    ),
    Block3 = process_code(Block2, Ctx, Body),
    emit(Block0, Block3);

%%process_code(Block0, #k_var{name=Var}) ->
%%    io:format("k_var ~p~n", [Var]),
%%    emit(Block0, e4_f:eval(Var));

%%
%% Begin guards
%%
process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_guard{clauses=Clauses}) ->
    lists:foldl(
        fun(C, Blk) ->
            process_code(Blk, Ctx, C)
        end, Block0, Clauses);
process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_guard_clause{guard=G, body=Body}) ->
    ConditionCode = process_code(
        e4_f1:block([e4_f1:comment("begin guard cond")],
                    [],
                    [e4_f1:comment("end guard cond")]),
        Ctx, G),
    BodyCode = process_code(
        e4_f1:block([e4_f1:comment("begin guard body")],
                    [],
                    [e4_f1:comment("end guard body")]),
        Ctx, Body),
    emit(Block0, e4_f1:'if'(ConditionCode, BodyCode));
%%
%% end guards

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_try{arg=Arg, vars=Vars, evars=Evars, handler=Handler,
                 body=Body, ret=Ret}) ->
%%    io:format("k_try args ~p~nvars ~p~nevars ~p~nhandler ~p~nret ~p~n"
%%              "body ~p~n", [Arg, Vars, Evars, Handler, Ret, Body]),
    Try0 = e4_f1:block(
        [e4_f1:comment("begin try")],
        [],
        [e4_f1:comment("end try")]),
    Try1 = process_code(Try0, Ctx, Arg),
    Try2 = emit(Try1, lists:map(fun e4_f1:store/1, Vars)),
    Try3 = process_code(Try2, Ctx, Body),
    emit(Block0, Try3);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_seq{arg=Arg, body=Body}) ->
%%    io:format("k_seq arg=~p~n  body=~p~n", [Arg, Body]),
    Block1 = process_code(Block0, Ctx, Arg),
    process_code(Block1, Ctx, Body);

process_code(Block0 = #f_block{}, #match_ctx{}, #k_return{args=Args}) ->
%%    io:format("k_return args=~p~n", [Args]),
    Block1 = eval_args(Block0, Args),
    emit(Block1, [?F_RET]);

process_code(Block0 = #f_block{}, #match_ctx{}, #k_enter{op=Op, args=Args}) ->
%%    io:format("k_enter op=~p args=~p~n", [Op, Args]),
    Block1 = eval_args(Block0, Args),
    emit(Block1, [Op, <<".CALL-TAIL">>]);

process_code(Block0 = #f_block{}, #match_ctx{},
             #k_call{op=Op, args=Args, ret=Ret}) ->
%%    io:format("k_call op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [e4_f1:eval(Op), <<".CALL">>]),
    emit(Block2, e4_f1:store(Ret));

process_code(Block0 = #f_block{}, #match_ctx{},
             #k_bif{op=Op, args=Args, ret=Ret}) ->
%%    io:format("k_bif op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [e4_f1:eval(Op), <<".BIF">>]),
    emit(Block2, [e4_f1:store(Ret)]);

process_code(Block0 = #f_block{}, #match_ctx{},
             #k_test{op=Op, args=Args}) ->
    %% Emit args
    Block1 = eval_args(Block0, Args),
    Block2 = emit(Block1, [e4_f1:eval(Op), <<".CALL">>]),
%%    case Inv of
%%        true -> emit(Block2, <<".INVERT">>);
%%        false -> Block2
%%    end;
    Block2;
process_code(Block0, #match_ctx{}, #k_put{arg=Arg, ret=Ret}) ->
%%    io:format("k_put arg=~p ret=~p~n", [Arg, Ret]),
    emit(Block0, [e4_f1:eval(Arg), e4_f1:store(Ret)]);

process_code(Block0 = #f_block{}, Ctx = #match_ctx{},
             #k_alt{first=First, then=Then}) ->
%%    io:format("k_alt first=~p~nthen=~p~n", [First, Then]),
    Alt0 = e4_f1:block(
        [e4_f1:comment("begin alt")],
        [],
        [e4_f1:comment("end alt")]),
    Alt1 = process_code(Alt0, Ctx, First),
%%    p(Alt1),
%%    io:format("Alt1 ~p~n", [Alt1]),
    Alt2 = process_code(Alt1, Ctx, Then),
    emit(Block0, Alt2);

process_code(Block0, #match_ctx{}, #k_match{vars=Vars, body=Body, ret=Ret}) ->
%%    io:format("k_match vars=~p~n", [Vars]),
    Match0 = e4_f1:block(
        [e4_f1:comment("begin match")],
        [],
        [e4_f1:comment("end match")]),
    Match1 = process_code(Match0, #match_ctx{match_vars=Vars}, Body),
    Match2 = emit(Match1, [e4_f1:store(Ret)]),
    emit(Block0, Match2);

process_code(Block0 = #f_block{}, Context0 = #match_ctx{},
             #k_select{var=Var, types=Types}) ->
%%    io:format("k_select var=~p~ntypes=~p~n", [Var, Types]),
    Select0 = e4_f1:block(
        [e4_f1:comment("begin select")],
        [],
        [e4_f1:comment("end select")]),
    %% Update focused select var
    Context1 = Context0#match_ctx{select_var=Var},
    %% Go deeper
    Select1 = process_code(Select0, Context1, Types),
    emit(Block0, Select1);

process_code(Block0 = #f_block{}, Context = #match_ctx{},
            #k_type_clause{type=Type, values=Values}) ->
%%    io:format("k_type_clause t=~p~n  val=~p~n", [Type, Values]),
    Type0 = match_if_type(Type, Context),
    Context1 = Context#match_ctx{type = Type},
    Type1 = process_code(Type0, Context1, Values),
    emit(Block0, Type1);

process_code(Block0 = #f_block{},
             #match_ctx{select_var=Rhs, type=Type} = Context,
             #k_val_clause{val=Lhs, body=Body}) ->
%%    io:format("k_val_clause lhs=~999p~n"
%%              "  rhs=~p~n  body=~p~n", [Lhs, Rhs, Body]),
    Val0 = e4_f1:block(
        [e4_f1:comment("begin val clause")],
        [],
        [e4_f1:comment("end val clause")]),
    %% In case RVal is a complex expression, save into tmp variable and emit
    %% the accompanying code (possibly empty list)
    %% The tmp code is inserted into the outer scope
    {RTmp, RTmpEmit} = e4_f1:make_tmp(Block0, Rhs),
    Block1 = emit(Block0, RTmpEmit),

    %% Create a conditional block for pattern match which checks if all
    %% values on the left match all values on the right.
    MatchBlock = emit_match(Block0#f_block.scope, Type, Lhs, RTmp),

    %% Process body of the clause
    Val1 = process_code(Val0, Context, Body),

    %% Insert processed code into match condition block
    MatchBlock1 = emit(MatchBlock, Val1),

    %% Now insert match condition block into the parent block
    emit(Block1, MatchBlock1);

process_code(#f_block{}, #match_ctx{}, X) ->
    ?COMPILE_ERROR("E4Cerl: Unknown Core AST piece ~s~n",
                   [?COLOR_TERM(red, X)]).

%% @doc Transform list of variables, literals and expressions into something
%% which evaluates them, or reads variable values or something. And leaves all
%% of them on the stack in the good order for a function call following after.
eval_args(Block, Args) ->
    lists:foldl(
        fun(A, Blk) -> emit(Blk, e4_f1:eval(A)) end,
        Block, Args
    ).

format_fun_name(Name, Arity) ->
    #k_local{name=Name, arity=Arity}.

%%get_code(#f_mod_pass1{code=Code}) -> Code.

%% @doc Create an IF block which checks if Context variable(s) are a Type
match_if_type(k_literal, _Context) ->
    e4_f1:block(); % do nothing just proceed to value clause and compare directly
match_if_type(Type, Context = #match_ctx{}) ->
    e4_f1:'if'(
        [e4_f1:eval(Context#match_ctx.select_var), make_type_check(Type)],
        e4_f1:block()
    ).

make_type_check(k_nil)      -> ?F_IS_NIL;
make_type_check(k_int)      -> <<".INT?">>;
make_type_check(k_cons)     -> <<".CONS?">>;
make_type_check(k_atom)     -> <<".ATOM?">>;
make_type_check(k_tuple)    -> <<".TUPLE?">>.

%% @doc Create a conditional block for pattern match which checks if all
%% values on the left match all values on the right.
%% Code should be inserted inside this block by the caller.
-spec emit_match(Scope :: [binary() | atom()],
                 k_tuple | k_cons | k_nil | k_float | k_atom | k_literal,
                 _Lhs, _Rhs) -> forth_ic().
emit_match(Scope, k_tuple, #k_tuple{es=LhsElements}, Rhs) ->
    %% Assuming Rhs is also a tuple, take elements from it and match against
    %% each of the LhsElements. Create new variables as needed.
    RhsElements = lists:map(
        fun(I) ->
            %% Naive approach is to retrieve it every time. Rhs is a temporary
            %% for non trivial expressions so it should be fast enough
            [e4_f1:eval(Rhs), e4_f1:lit(I), <<".GET-ELEMENT">>]
        end,
        lists:seq(1, length(LhsElements))
    ),
    VarPairs = lists:zip(LhsElements, RhsElements),

    %% Now partition a list of [{L,R}, ... ] pairs into such, where L is
    %% known and can be compared and other, where L is an unbound name and
    %% will always match, producing a bound variable.
    PartitionFun = fun({L, _R}) -> e4_helper:is_in_the_scope(Scope, L) end,
    {ComparePairs, AssignPairs} = lists:partition(PartitionFun, VarPairs),

    %% Dump out the comparison-matching code
    Block0 = lists:foldl(
        fun({Lhs1, Rhs1}, Block0) ->
            emit(Block0, e4_f1:equals(Lhs1, Rhs1))
        end,
        e4_f1:block([e4_f1:comment("emit_match todo IF here")]),
        ComparePairs
    ),
    %% Dump out the assigning code
    lists:foldl(
        fun({Lhs1, Rhs1}, Blk) ->
            emit(Blk, [e4_f1:eval(Rhs1), e4_f1:store(Lhs1)])
        end,
        Block0,
        AssignPairs
    );
emit_match(Scope, k_cons, #k_cons{hd=LHead, tl=LTail}, Rhs) ->
    %% Assuming Rhs is also a cons, take [H|T] from it and match against
    %% left Head and Tail. Create new variables as needed.

    LHeadExists = e4_helper:is_in_the_scope(Scope, LHead),
    LTailExists = e4_helper:is_in_the_scope(Scope, LTail),

    case LHeadExists of
        true ->
            case LTailExists of
                true ->
                    %% Both [LH|LT] on the left exist, we just compare
                    %% them with [RH|RT] and join with &&
                    e4_f1:'if'([
                        e4_f1:eval(Rhs), <<".DECONS">>, % [H|T] -- H T
                        e4_f1:eval(LTail), <<"==">>, <<"SWAP">>,
                        e4_f1:eval(LHead), <<"==">>, <<"AND">>
                    ], []);
                false ->
                    %% only LHead exists, so we compare heads and
                    %% assign RTail to LTail
                    e4_f1:'if'([
                        e4_f1:eval(Rhs), <<".HD">>,
                        e4_f1:eval(LHead), <<"==">>
                    ], [
                        e4_f1:eval(Rhs), <<".TL">>,
                        e4_f1:store(LTail)
                    ])
            end;
        false ->
            case LTailExists of
                true ->
                    e4_f1:'if'([
                        e4_f1:eval(Rhs), <<".TL">>,
                        e4_f1:eval(LTail), <<"==">>
                    ], [
                        e4_f1:eval(Rhs), <<".HD">>,
                        e4_f1:store(LHead)
                    ]);
                false ->
                    %% Neither LHead nor LTail exist, so just assign
                    e4_f1:block([
                        e4_f1:eval(Rhs), <<".DECONS">>, % [H|T] -- H T
                        e4_f1:store(LTail),
                        e4_f1:store(LHead)
                    ])
            end
    end;
emit_match(_Scope, k_nil, _, R)    -> e4_f1:block([e4_f1:eval(R), ?F_IS_NIL]);
emit_match(_Scope, k_float, L, R)  -> e4_f1:block(e4_f1:equals(L, R));
emit_match(_Scope, k_int, L, R)    -> e4_f1:block(e4_f1:equals(L, R));
emit_match(_Scope, k_atom, L, R)   -> e4_f1:block(e4_f1:equals(L, R));
emit_match(_Scope, k_literal, L, R) -> e4_f1:block(e4_f1:equals(L, R));
emit_match(_Scope, _, #k_var{} = L, #k_var{} = R) ->
    e4_f1:block(e4_f1:equals(L, R));
emit_match(_Scope, k_atom, [LhsVar], Rhs) ->
    e4_f1:block(e4_f1:equals(LhsVar, Rhs)).
