%%% @doc Intermediate format library for pass 2
%%% Allocates variables and places load/store instructions
%%% in the code
%%% @end
-module(e4_f2).

%% API
-export([format_vars/1, alloc_var/3, add_alias/3,
         format_scope/1, emit/2, merge/2, retrieve/2, store/2]).

-include("e4_kernel_erl.hrl").
-include("e4_forth.hrl").
-include("e4.hrl").

retrieve(#f_var_storage{} = Scope, Var) ->
    CfVar = e4_f1:var(Var),
%%    io:format("retr ~p in vars ~s~n", [CfVar, format_vars(Scope)]),
    {ok, Index} = index_in_storage(Scope, CfVar),
    #f2_ld{index=Index}.

store(#f_var_storage{} = Scope, Var) ->
    CfVar = e4_f1:var(Var),
%%    io:format("stor ~p into vars ~s~n", [CfVar, format_vars(AllocatedVars)]),
    {ok, Index} = index_in_storage(Scope, CfVar),
    #f2_st{index=Index}.

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> {ok, Index};
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

format_vars(#f_var_storage{stack_frame=Frm, args=Args}) ->
    ExtractFn = fun(#k_var{name=N}) -> erlang:binary_to_list(N) end,
    FrmVars = lists:map(ExtractFn, Frm),
    ArgVars = lists:map(ExtractFn, Args),
    ["Vars{frm=[", string:join(FrmVars, ", "),
        "] args=[", string:join(ArgVars, ", "),
        "]}" ].

format_scope(Scope) when is_list(Scope) ->
    {_, V} = lists:unzip(Scope),
    S = lists:map(fun erlang:atom_to_list/1, V),
    [$[, string:join(S, ", "), $]].

%% @doc Given the current variable storage find the position of the variable.
%% it can be argument on stack (pre existing) or belong to the stack frame.
%% A stack frame looks like: arg3 arg2 arg1 |stack_base_pointer| var1 var2 ...
%% which gives args positive values (forward in memory) and vars on frame
%% negative values (back in memory), and stack grows back in memory too when
%% more frames are needed.
index_in_storage(Storage = #f_var_storage{aliases=Aliases},
                 CfVar0 = #k_var{}) ->
    case index_in_storage2(Storage, CfVar0) of
        {ok, I} -> {ok, I};
        not_found ->
            %% Maybe an alias?
            CfVar = case orddict:find(CfVar0, Aliases) of
                        {ok, OtherVar} -> OtherVar;
                        error ->
                            ?COMPILE_ERROR("Variable ~s not found",
                                           [?COLOR_TERM(red, CfVar0)])
                    end,
            case index_in_storage2(Storage, CfVar) of
                {ok, I} -> {ok, I};
                not_found ->
                    ?COMPILE_ERROR("Variable ~s not found",
                                   [?COLOR_TERM(red, CfVar0)])
            end
    end.

index_in_storage2(#f_var_storage{stack_frame=StackFrame, args=Args},
                  CfVar = #k_var{}) ->
    case index_of(CfVar, Args) of
        {ok, ArgIndex} -> {ok, ArgIndex};
        not_found ->
            case index_of(CfVar, StackFrame) of
                {ok, FrameIndex} -> {ok, -FrameIndex};
                not_found -> not_found
            end
    end.

-spec alloc_var(f_var_storage(), k_var(), f_var_alloc_type())
        -> f_var_storage().
alloc_var(Storage = #f_var_storage{args=Existing},
          Var = #k_var{},
          pre_existing) ->
    Storage#f_var_storage{args=[Var | Existing]};
alloc_var(Storage = #f_var_storage{stack_frame=Frame},
          Var = #k_var{},
          stack_frame) ->
    Storage#f_var_storage{stack_frame=[Var | Frame]}.

add_alias(Storage = #f_var_storage{aliases=Aliases},
          Var = #k_var{},
          ExistingVar = #k_var{}) ->
    Aliases1 = orddict:store(Var, ExistingVar, Aliases),
    Storage#f_var_storage{aliases=Aliases1}.

-spec emit(Mod :: f_module(), Code :: forth_ic()) -> f_block().
%%emit(_Mod0, #f_ld{}) -> compile_error("E4 Pass2: can't emit LD construct");
%%emit(_Mod0, #f_st{}) -> compile_error("E4 Pass2: can't emit ST construct");
emit(Mod0, AddCode) when not is_list(AddCode) ->
    emit(Mod0, [AddCode]);
emit(Mod0, AddCode) ->
    lists:foldl(
        fun(Nested, Md) when is_list(Nested) ->
            emit(Md, Nested);
            (ForthOp, Md = #f_module{output=Code}) ->
                Md#f_module{output=Code ++ [ForthOp]}
        end,
        Mod0,
        AddCode).

merge(#f_var_storage{stack_frame=Stack1, args=Args1},
      #f_var_storage{stack_frame=Stack2, args=Args2}) ->
    Overlap1 = ordsets:intersection(
        ordsets:from_list(Stack1),
        ordsets:from_list(Stack2)),
    case Overlap1 of [] -> ok; _ ->
        ?COMPILE_ERROR("Var in nested scope shadow outer scope ~p", [Overlap1])
    end,
    #f_var_storage{stack_frame=Stack1 ++ Stack2,
                   args=Args1 ++ Args2}.
