%%% @doc Pass 2: allocates variables and places load/store instructions
%%% in the code
-module(e4_f2).

%% API
-export([retrieve/2, store/2, format_vars/1, alloc_var/3, add_alias/3,
    format_scope/1]).

-include("e4_forth.hrl").

retrieve(Mod = #f_module{}, #f_ld{var=Var}) ->
    retrieve(Mod, Var);
retrieve(#f_module{}, #f_mfa{}=MFA) ->
    MFA;
retrieve(#f_module{scope=_Scope, alloc_vars=AllocatedVars}, Var) ->
    CfVar = e4_f:var(Var),
%%    io:format("retr ~p in vars ~s~n", [CfVar, format_vars(AllocatedVars)]),
    {ok, Index} = index_in_storage(AllocatedVars, CfVar),
    [e4_f:lit(Index), <<".LD">>].

store(#f_module{scope=_Scope, alloc_vars=AllocatedVars}, Var) ->
    CfVar = e4_f:var(Var),
%%    io:format("stor ~p into vars ~s~n", [CfVar, format_vars(AllocatedVars)]),
    {ok, Index} = index_in_storage(AllocatedVars, CfVar),
    [e4_f:lit(Index), <<".ST">>].

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> {ok, Index};
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

format_vars(#f_var_storage{stack_frame=Frm, args=Args}) ->
    ExtractFn = fun(#f_var{name=N}) -> erlang:atom_to_list(N) end,
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
                 CfVar0 = #f_var{}) ->
    case index_in_storage2(Storage, CfVar0) of
        {ok, I} -> {ok, I};
        not_found ->
            %% Maybe an alias?
            CfVar = case orddict:find(CfVar0, Aliases) of
                        {ok, OtherVar} -> OtherVar;
                        error ->
                            e4:compile_error("variable ~p not found", [CfVar0])
                    end,
            case index_in_storage2(Storage, CfVar) of
                {ok, I} -> {ok, I};
                not_found ->
                    e4:compile_error("variable ~p not found", [CfVar0])
            end
    end.

index_in_storage2(#f_var_storage{stack_frame=StackFrame, args=Args},
                  CfVar = #f_var{}) ->
    case index_of(CfVar, Args) of
        {ok, ArgIndex} -> {ok, ArgIndex};
        not_found ->
            case index_of(CfVar, StackFrame) of
                {ok, FrameIndex} -> {ok, -FrameIndex};
                not_found -> not_found
            end
    end.

-spec alloc_var(f_var_storage(), f_var(), f_var_alloc_type())
        -> f_var_storage().
alloc_var(Storage = #f_var_storage{args=Existing},
          Var = #f_var{},
          pre_existing) ->
    Storage#f_var_storage{args=[Var | Existing]};
alloc_var(Storage = #f_var_storage{stack_frame=Frame},
          Var = #f_var{},
          stack_frame) ->
    Storage#f_var_storage{stack_frame=[Var | Frame]}.

add_alias(Storage = #f_var_storage{aliases=Aliases},
          Var = #f_var{},
          ExistingVar = #f_var{}) ->
    Aliases1 = orddict:store(Var, ExistingVar, Aliases),
    Storage#f_var_storage{aliases=Aliases1}.
