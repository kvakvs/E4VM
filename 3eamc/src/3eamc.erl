%%%-------------------------------------------------------------------
%% @doc 3eamc public API
%% @end
%%%-------------------------------------------------------------------

-module('3eamc').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_3eamc/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, Args) ->
    start_3eamc(Args).
%%    '3eamc_sup':start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_3eamc([]) ->
    io:format("All arguments were processed~n", []),
    ok;
start_3eamc([F | Tail]) ->
    io:format("Processing: ~p~n...", [F]),
    '3eamc_compiler':process(atom_to_list(F)),
    start_3eamc(Tail).
