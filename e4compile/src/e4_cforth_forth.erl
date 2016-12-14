%%%-------------------------------------------------------------------
%%% @doc Compile Core Forth AST into Forth program consisting of words
%%% and literals. Optimize. Resolve variables.
%%% @end
%%%-------------------------------------------------------------------
-module(e4_cforth_forth).

%% API
-export([process/1]).

-include("e4_cf.hrl").
-include("e4_f.hrl").

-import(e4, [compile_error/2]).

process(CoreForth) ->
    #f_module{output=F} = process_code(#f_module{}, CoreForth),
    io:format("~p~n", [F]),
    F.

process_code(Mod0 = #f_module{output=Code}, []) ->
    Mod0#f_module{output=lists:reverse(Code)};
process_code(Mod0 = #f_module{}, [CF | Tail]) ->
    Mod1 = process_code(Mod0, CF),
    process_code(Mod1, Tail);
process_code(Mod0, Op) -> % if a single item is given, like a root block
    process_op(Mod0, Op).

-spec emit(Block :: cf_block(), Code :: cf_op() | cf_code()) -> cf_block().
emit(Mod0, AddCode) when not is_list(AddCode) ->
    emit(Mod0, [AddCode]);
emit(Mod0, AddCode) ->
    lists:foldl(
        fun(Nested, Md) when is_list(Nested) ->
                emit(Md, Nested);
            (ForthOp, Md = #f_module{output=Code}) ->
                %% TODO: Fix me i'm slow
                Md#f_module{output=Code++[ForthOp]}
        end,
        Mod0,
        AddCode).

process_op(Mod0 = #f_module{}, A) when is_atom(A) ->
    emit(Mod0, A); % pass through forth words
process_op(Mod0 = #f_module{scope=OuterScope},
           #cf_block{before=Before, code=Code, 'after'=After, scope=InnerScope}) ->
    %% Enter deeper scope
    EnterScope = ordsets:union([InnerScope, OuterScope]),
    Mod1 = Mod0, %stack_frame_enter(Mod0, InnerScope),

    io:format("enter scope ~s~n", [e4_f:format_scope(EnterScope)]),
    Mod2 = Mod1#f_module{scope=EnterScope},

    Mod3 = process_code(Mod2, [Before ++ Code ++ After]),

    %% Restore scope
    io:format("leave scope, restore ~sn", [e4_f:format_scope(OuterScope)]),
    Mod4 = Mod3, %stack_frame_leave(Mod3, length(InnerScope)),
    Mod4#f_module{scope=OuterScope};

process_op(Mod0 = #f_module{}, #cf_comment{} = C) ->
    emit(Mod0, C);
process_op(Mod0 = #f_module{}, #cf_mfarity{} = MFA) ->
    emit(Mod0, MFA);
process_op(Mod0 = #f_module{}, #cf_retrieve{var=V}) ->
    emit(Mod0, e4_f:retrieve(Mod0, V));
process_op(Mod0 = #f_module{}, #cf_store{var=V}) ->
    emit(Mod0, e4_f:store(Mod0, V));
process_op(Mod0 = #f_module{}, #cf_lit{} = Lit) ->
    emit(Mod0, Lit);
process_op(Mod0 = #f_module{}, #cf_alias{var=Var, alt=A}) ->
    emit(Mod0, [e4_cf:comment("alias for ~p=~p", [Var, A])]);
process_op(_Mod0, CF) ->
    compile_error("E4Core4: Unknown op ~p~n", [CF]).

