%%% @doc Pass 1: From Kernel Erlang produces intermediate C++ AST tree with
%% scopes defined and basic blocks (code constructs) marked
-module(e4_pass_kern).

%% API
-export([process/1]).

-include_lib("compiler/src/core_parse.hrl").

-include_lib("e4c/include/kernel_erl.hrl").
-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").

-type ktype_clause_type() :: k_tuple | k_atom | k_int | k_float | k_nil
                            | k_cons | k_literal | k_map | k_binary.
-record(match_ctx, {
    match_vars = [] :: [k_var()],   % vars which appeared in #k_match{}
    select_var,                     % focused var which appeared #k_select{}
    type = k_nil :: ktype_clause_type()
}).
-type match_ctx() :: #match_ctx{}.

-spec process(k_mdef()) -> cpp_block().
process(#k_mdef{name = Name, exports = _Exps,
                attributes = _Attr, body = Body}) ->
    % exports -> lists:map(fun process_export/1, Exps)
    ModuleBody = process_code(ecpp_ast:block(), #match_ctx{}, Body),
    e4c:debug_write_term("e4c_pass_kern.txt", ModuleBody),

%%    io:format("~s~n~s~n",
%%              [color:on_white(color:black(" PASS 1 ")),
%%               e4_print_ic:format_ic(Out, 0)]),
    ecpp_ast:module(Name, ModuleBody).

%%process_export({F, Arity}) -> [ecpp_ast:comment("export ~s/~B", [F, Arity])].

%%p(L) -> e4_print_ic:format_core_forth(L, 0).

-spec process_code(cpp_block(), match_ctx(), k_ast()) -> cpp_block().
process_code(Root, #match_ctx{}, []) -> Root;

%% Special case returning a var if it comes last in the list
process_code(Root, #match_ctx{}, #k_var{} = Var) ->
    ecpp_block:emit(Root, e4_f1:eval(Var));

process_code(Root, Ctx = #match_ctx{}, [CoreOp | Tail]) ->
    Block1 = process_code(Root, Ctx, CoreOp),
    process_code(Block1, Ctx, Tail);

process_code(Root,
             Ctx = #match_ctx{},
             #k_fdef{func=Name, arity=Arity, vars=Vars, body=Body}) ->
%%    io:format("k_fdef name=~s vars~p~n", [Name, Vars]),
%%    Block2 = lists:foldl(
%%        fun(A0, Blk) ->
%%            A1 = e4_f1:var(A0),
%%            emit(Blk, e4_f1:mark_new_arg(A1))
%%        end,
%%        Block1, Vars
%%    ),
    Code = process_code(ecpp_ast:block(), Ctx, Body),
    Fun = ecpp_ast:function("Term",
                            ecpp_ast:fun_name(Name, Arity),
                            lists:map(fun e4_f1:var/1, Vars),
                            Code),
    ecpp_block:emit(Root, Fun);

%%process_code(Block0, #k_var{name=Var}) ->
%%    io:format("k_var ~p~n", [Var]),
%%    emit(Block0, e4_f:eval(Var));

%%
%% Begin guards
%%
process_code(Root,
             Ctx = #match_ctx{},
             #k_guard{clauses = Clauses}) ->
    lists:foldl(
             fun(C, Blk) ->
            process_code(Blk, Ctx, C)
        end, Root, Clauses);
process_code(Root,
             Ctx = #match_ctx{},
             #k_guard_clause{guard = G, body = Body}) ->
    GCRef = erlang:make_ref(),
    GuardCond = process_code(
        ecpp_ast:block([ecpp_ast:comment("begin guard cond ~p", [GCRef])],
                    [],
                    [ecpp_ast:comment("end guard cond ~p", [GCRef])]),
        Ctx, G),
    GuardBody = process_code(
        ecpp_ast:block([ecpp_ast:comment("begin guard body ~p", [GCRef])],
                    [],
                    [ecpp_ast:comment("end guard body ~p", [GCRef])]),
        Ctx, Body),
    ecpp_block:emit(Root, ecpp_ast:if_block(GuardCond, GuardBody));
%%
%% end guards

process_code(Root,
             Ctx = #match_ctx{},
             #k_try{arg = Arg, vars = _Vars,
                    evars = _Evars, handler = Handler,
                    body = _Body, ret = _Ret}) ->
%%    io:format("k_try args ~p~nvars ~p~nevars ~p~nhandler ~p~nret ~p~n"
%%              "body ~p~n", [Arg, Vars, Evars, Handler, Ret, Body]),
    Try1 = ecpp_ast:try_block(process_code(Root, Ctx, Arg),
                              []), %process_code(Root, Handler)),

    %StoreCode = lists:map(fun e4_f1:store/1, Vars),
    %Try2 = emit(Try1, StoreCode),

    %Try3 = process_code(Try2, Ctx, Body),
    ecpp_block:emit(Root, Try1);

process_code(Root, Ctx = #match_ctx{},
             #k_seq{arg=Arg, body=Body}) ->
%%    io:format("k_seq arg=~p~n  body=~p~n", [Arg, Body]),
    Root1 = process_code(Root, Ctx, Arg),
    process_code(Root1, Ctx, Body);

process_code(Root, #match_ctx{}, #k_return{args = Arg0}) ->
%%    io:format("k_return args=~p~n", [Args]),
    [Arg1] = eval_args(Arg0),
    ecpp_block:emit(Root, ecpp_ast:return(Arg1));

process_code(Root,
             #match_ctx{},
             #k_enter{op=Op, args=Args}) ->
%%    io:format("k_enter op=~p args=~p~n", [Op, Args]),
    CallCode = ecpp_ast:call(e4_f1:eval(Op), eval_args(Args)),
    ecpp_block:emit(Root, CallCode);

process_code(Root,
             #match_ctx{},
             #k_call{op = Op, args = Args, ret = Ret}) ->
%%    io:format("k_call op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    CallCode = ecpp_ast:call(e4_f1:eval(Op), eval_args(Args)),
    Code = ecpp_ast:assign(e4_f1:var(Ret), CallCode),
    ecpp_block:emit(Root, Code);

process_code(Root,
             #match_ctx{},
             #k_bif{op = Op, args = Args0, ret = Ret0}) ->
%%    io:format("k_bif op=~p~n  args=~p~n  ret=~p~n", [Op, Args, Ret]),
    CallCode = ecpp_ast:call(e4_f1:eval(Op), eval_args(Args0)),
    Code = ecpp_ast:assign(e4_f1:var(Ret0), CallCode),
    ecpp_block:emit(Root, Code);

process_code(Root,
             #match_ctx{},
             #k_test{op = Op, args = Args}) ->
    %% Emit args
%%    Block1 = eval_args(Block0, Args),
%%    Block2 = emit(Block1, [e4_f1:eval(Op), ?F_ERL_CALL]),
    CallCode = ecpp_ast:call(e4_f1:eval(Op), eval_args(Args)),
    ecpp_block:emit(Root, CallCode);

process_code(Root,
             #match_ctx{},
             #k_put{arg = Arg, ret = Ret0}) ->
%%    io:format("k_put arg=~p ret=~p~n", [Arg, Ret]),
    ecpp_block:emit(Root,
                    ecpp_ast:assign(e4_f1:var(Ret0),
                                    e4_f1:eval(Arg)));

process_code(Root,
             Ctx = #match_ctx{},
             #k_alt{first = First, then = Then}) ->
    Alt0 = ecpp_ast:block(
        [ecpp_ast:comment("begin alt")],
        [],
        [ecpp_ast:comment("end alt")]),
    Alt1 = process_code(Alt0, Ctx, First),
    Alt2 = process_code(Alt1, Ctx, Then),
    ecpp_block:emit(Root, Alt2);

process_code(Root,
             #match_ctx{},
             #k_match{vars = Vars, body = Body, ret = _Ret}) ->
%%    io:format("k_match vars=~p~n", [Vars]),
    Match0 = ecpp_ast:block(
        [ecpp_ast:comment("begin match")],
        [],
        [ecpp_ast:comment("end match")]),
    Match1 = process_code(Match0, #match_ctx{match_vars = Vars}, Body),
    %Match2 = emit(Match1, ecpp_ast:assign(Ret, )]),
    ecpp_block:emit(Root, Match1);

process_code(Root,
             Context0 = #match_ctx{},
             #k_select{var=Var, types=Types}) ->
    Select0 = ecpp_ast:block(
        [ecpp_ast:comment("begin select")],
        [],
        [ecpp_ast:comment("end select")]),
    %% Update focused select var
    Context1 = Context0#match_ctx{select_var=Var},
    %% Go deeper
    Select1 = process_code(Select0, Context1, Types),
    ecpp_block:emit(Root, Select1);

process_code(Root,
             Context = #match_ctx{},
             #k_type_clause{type = Type, values = Values}) ->
%%    io:format("k_type_clause t=~p~n  val=~p~n", [Type, Values]),
    TypeBlock0 = match_if_type(Type, Context),
    Context1 = Context#match_ctx{type = Type},
    TypeBlock1 = process_code(TypeBlock0, Context1, Values),
    ecpp_block:emit(Root, TypeBlock1);

process_code(Root,
             #match_ctx{select_var = Rhs0, type = Type} = Context,
             #k_val_clause{val = Lhs, body = Body}) ->
    Val0 = ecpp_ast:block(
        [ecpp_ast:comment("begin val clause")],
        [],
        [ecpp_ast:comment("end val clause")]),
    %% In case RVal is a complex expression, save into tmp variable and emit
    %% the accompanying code (possibly empty list)
    %% The tmp code is inserted into the outer scope
    Rhs = e4_f1:var(Rhs0),
    #{name := RTmp, code := RTmpEmit} = e4_f1:make_tmp(Rhs),
    Root1 = ecpp_block:emit(Root, RTmpEmit),

    %% Create a conditional block for pattern match which checks if all
    %% values on the left match all values on the right.
    MatchBlock = emit_match(ecpp_block:scope(Root), Type, Lhs, RTmp),

    %% Process body of the clause
    Val1 = process_code(Val0, Context, Body),

    %% Insert processed code into match condition block
    MatchBlock1 = ecpp_block:emit(MatchBlock, Val1),

    %% Now insert match condition block into the parent block
    ecpp_block:emit(Root1, MatchBlock1);

process_code(_Root, #match_ctx{}, X) ->
    ?COMPILE_ERROR("Unknown input AST piece ~s~n",
                   [?COLOR_TERM(red, X)]).

%% @doc Transform list of variables, literals and expressions into something
%% which evaluates them, or reads variable values or something. And leaves all
%% of them on the stack in the good order for a function call following after.
eval_args(Args) ->
    lists:map(fun e4_f1:eval/1, Args).

%%get_code(#f_mod_pass1{code=Code}) -> Code.

%% @doc Create an IF block which checks if Context variable(s) are a Type
match_if_type(k_literal, _Context) ->
    ecpp_ast:block(); % do nothing, proceed to value clause and compare directly
match_if_type(Type, Context = #match_ctx{}) ->
    ecpp_ast:if_block(
        ecpp_ast:call(type_test_function(Type),
                      [e4_f1:eval(Context#match_ctx.select_var)]),
        ecpp_ast:block()
    ).

type_test_function(k_nil)      -> "is_nil";
type_test_function(k_int)      -> "is_integer";
type_test_function(k_cons)     -> "is_cons";
type_test_function(k_atom)     -> "is_atom";
type_test_function(k_tuple)    -> "is_tuple".

%% @doc Create a conditional block for pattern match which checks if all
%% values on the left match all values on the right.
%% Code should be inserted inside this block by the caller.
-spec emit_match(Scope :: [binary() | atom()],
                 Type :: ktype_clause_type(),
                 _Lhs, _Rhs) -> forth_ic().
emit_match(Scope, k_tuple, #k_tuple{es = LhsElements0}, Rhs) ->
    %% Assuming Rhs is also a tuple, take elements from it and match against
    %% each of the LhsElements. Create new variables as needed.
    LhsElements = lists:map(fun e4_f1:var/1, LhsElements0),
    RhsElements = lists:map(
        fun(I) ->
            %% Naive approach is to retrieve it every time. Rhs is a temporary
            %% for non trivial expressions so it should be fast enough
            e4_f1:element(I, Rhs)
        end,
        lists:seq(1, length(LhsElements))
    ),
    VarPairs = lists:zip(LhsElements, RhsElements),

    %% Now partition a list of [{L,R}, ... ] pairs into such, where L is
    %% known and can be compared and other, where L is an unbound name and
    %% will always match, producing a bound variable.
    PartitionFun = fun({L, _R}) -> e4_f1:is_in_the_scope(Scope, L) end,
    {ComparePairs, AssignPairs} = lists:partition(PartitionFun, VarPairs),

    %% Dump out the comparison-matching code
    Block0 = lists:foldl(
        fun({Lhs1, Rhs1}, Block0) ->
            ecpp_block:emit(Block0, e4_f1:equals(Lhs1, Rhs1))
        end,
        ecpp_ast:block([ecpp_ast:comment("emit_match todo IF here")]),
        ComparePairs
    ),
    %% Dump out the assigning code
    lists:foldl(
        fun({Lhs1, Rhs1}, Blk) ->
            Lhs2 = e4_f1:var(Lhs1),
            ecpp_block:emit(Blk, ecpp_ast:assign(Lhs2, e4_f1:eval(Rhs1)))
        end,
        Block0,
        AssignPairs
    );
emit_match(Scope, k_cons, #k_cons{hd=LHead, tl=LTail}, Rhs) ->
    %% Assuming Rhs is also a cons, take [H|T] from it and match against
    %% left Head and Tail. Create new variables as needed.

    LHeadExists = e4_f1:is_in_the_scope(Scope, e4_f1:var(LHead)),
    LTailExists = e4_f1:is_in_the_scope(Scope, e4_f1:var(LTail)),

    case LHeadExists of
        true ->
            case LTailExists of
                true ->
                    %% Both [LH|LT] on the left exist, we just compare
                    %% them with [RH|RT] and join with &&
                    ecpp_ast:if_block(
                        ecpp_ast:call("and", [
                            ecpp_ast:call("equals",
                                          [ecpp_ast:call("hd", e4_f1:eval(Rhs)),
                                          e4_f1:eval(LHead)]),
                            ecpp_ast:call("equals",
                                          [ecpp_ast:call("tl", e4_f1:eval(Rhs)),
                                          e4_f1:eval(LHead)])
                            ]),
                        []);
                false ->
                    %% only LHead exists, so we compare heads and
                    %% assign RTail to LTail
                    ecpp_ast:if_block(
                        ecpp_ast:call("equals", [
                            ecpp_ast:call("head", [e4_f1:eval(Rhs)]),
                            e4_f1:eval(LTail)
                        ]),
                        ecpp_ast:assign(
                            LHead,
                            ecpp_ast:call("tail", [e4_f1:eval(Rhs)])
                        ))
            end;
        false ->
            case LTailExists of
                true ->
                    ecpp_ast:if_block(
                        ecpp_ast:call("equals", [
                            ecpp_ast:call("tail", [e4_f1:eval(Rhs)]),
                            e4_f1:eval(LTail)
                        ]),
                        ecpp_ast:assign(
                            LHead,
                            ecpp_ast:call("head", [e4_f1:eval(Rhs)])
                        ));
                false ->
                    %% Neither LHead nor LTail exist, so just assign
                    ecpp_ast:block([
                        ecpp_ast:call("decons",
                                      [ecpp_ast:ref(e4_f1:eval(LHead)),
                                       ecpp_ast:ref(e4_f1:eval(LTail))
                                      ])
                    ])
            end
    end;
emit_match(_Scope, k_nil, _, R)    ->
    ecpp_ast:block([ ecpp_ast:call("is_nil", [e4_f1:eval(R)]) ]);
emit_match(_Scope, k_float, L, R)  -> ecpp_ast:block(e4_f1:equals(L, R));
emit_match(_Scope, k_int, L, R)    -> ecpp_ast:block(e4_f1:equals(L, R));
emit_match(_Scope, k_atom, L, R)   -> ecpp_ast:block(e4_f1:equals(L, R));
emit_match(_Scope, k_literal, L, R) -> ecpp_ast:block(e4_f1:equals(L, R));
emit_match(_Scope, _, #k_var{} = L, #k_var{} = R) ->
    ecpp_ast:block(e4_f1:equals(L, R));
emit_match(_Scope, k_atom, [LhsVar], Rhs) ->
    ecpp_ast:block(e4_f1:equals(LhsVar, Rhs)).
