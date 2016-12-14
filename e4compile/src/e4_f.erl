%%%-------------------------------------------------------------------
%%% @doc Forth primitives and helpers
%%% @end
%%%-------------------------------------------------------------------
-module(e4_f).

%% API
-export([retrieve/2, store/2, format_vars/1, alloc_var/3, add_alias/3]).

-include("e4_f.hrl").

retrieve(#f_module{scope=_Scope, alloc_vars=AllocatedVars}, Var) ->
    CfVar = e4_cf:var(Var),
    io:format("retr ~p in vars ~s~n", [CfVar, format_vars(AllocatedVars)]),
    {ok, Index} = index_in_storage(AllocatedVars, CfVar),
    [e4_cf:lit(Index), 'LD'].

store(#f_module{scope=_Scope, alloc_vars=AllocatedVars}, Var) ->
    CfVar = e4_cf:var(Var),
    io:format("stor ~p into vars ~s~n", [CfVar, format_vars(AllocatedVars)]),
    {ok, Index} = index_in_storage(AllocatedVars, CfVar),
    [e4_cf:lit(Index), 'ST'].

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> {ok, Index};
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

format_vars(#f_var_storage{stack_frame=Frm, args=Args}) ->
    ExtractFn = fun(#cf_var{name=N}) -> erlang:atom_to_list(N) end,
    FrmVars = lists:map(ExtractFn, Frm),
    ArgVars = lists:map(ExtractFn, Args),
    ["Vars{frm=[", string:join(FrmVars, ", "),
        "] args=[", string:join(ArgVars, ", "),
        "]}" ];
format_vars(Scope) ->
    {_, V} = lists:unzip(Scope),
    S = lists:map(fun erlang:atom_to_list/1, V),
    [$[, string:join(S, ", "), $]].

%% @doc Given the current variable storage find the position of the variable.
%% it can be argument on stack (pre existing) or belong to the stack frame.
%% A stack frame looks like: arg3 arg2 arg1 |stack_base_pointer| var1 var2 ...
%% which gives args positive values (forward in memory) and vars on frame
%% negative values (back in memory), and stack grows back in memory too when
%% more frames are needed.
index_in_storage(#f_var_storage{stack_frame=Frm, args=Args, aliases=Aliases},
                 CfVar0 = #cf_var{}) ->
    %% Maybe an alias?
    CfVar = case orddict:find(CfVar0, Aliases) of
                {ok, OtherVar} -> OtherVar;
                error -> CfVar0
            end,
    case index_of(CfVar, Args) of
        {ok, ArgIndex} -> {ok, ArgIndex};
        not_found ->
            case index_of(CfVar, Frm) of
                {ok, FrameIndex} -> {ok, -FrameIndex};
                not_found ->
                    e4:compile_error("variable ~p not found", [CfVar])
            end
    end.

-spec alloc_var([f_var_storage()], cf_var(), f_var_alloc_type())
        -> [f_var_storage()].
alloc_var(Storage = #f_var_storage{args=Existing},
          Var = #cf_var{},
          pre_existing) ->
    Storage#f_var_storage{args=[Var | Existing]};
alloc_var(Storage = #f_var_storage{stack_frame=Frame},
          Var = #cf_var{},
          stack_frame) ->
    Storage#f_var_storage{stack_frame=[Var | Frame]}.

add_alias(Storage = #f_var_storage{aliases=Aliases},
          Var = #cf_var{},
          ExistingVar = #cf_var{}) ->
    Storage#f_var_storage{args=orddict:store(Var, ExistingVar, Aliases)}.
