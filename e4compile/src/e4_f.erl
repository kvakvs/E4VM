%%%-------------------------------------------------------------------
%%% @doc Forth primitives and helpers
%%% @end
%%%-------------------------------------------------------------------
-module(e4_f).

%% API
-export([retrieve/2, store/2, format_scope/1]).

-include("e4_f.hrl").

retrieve(#f_mod{scope=Scope, stack=_Stack}, Var) ->
    CfVar = e4_cf:var(Var),
    io:format("retr ~p in sco ~s~n", [CfVar, format_scope(Scope)]),
    {ok, Index} = index_of(CfVar, Scope),
    [e4_cf:lit(Index), 'LD'].

store(#f_mod{scope=Scope, stack=_Stack}, Var) ->
    CfVar = e4_cf:var(Var),
    io:format("stor ~p in sco ~s~n", [CfVar, format_scope(Scope)]),
    {ok, Index} = index_of(e4_cf:var(CfVar), Scope),
    [e4_cf:lit(Index), 'ST'].

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> {ok, Index};
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

format_scope(Scope) ->
    {_, V} = lists:unzip(Scope),
    S = lists:map(fun erlang:atom_to_list/1, V),
    [$[, string:join(S, ", "), $]].