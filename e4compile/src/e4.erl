%%%-------------------------------------------------------------------
%% @doc E4 Erlang-Forth public API
%% @end
%%%-------------------------------------------------------------------

-module(e4).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_e4_compiler/1, start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    start(normal, init:get_plain_arguments()).

start(_StartType, Args) ->
    start_e4_compiler(Args),
    init:stop(),
    {ok, self()}.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_e4_compiler([]) ->
    io:format("~s~n", [color:greenb("E4: All arguments were processed")]),
    ok;
start_e4_compiler([F | Tail]) ->
    io:format("E4: Processing: ~p...~n", [F]),
    try e4_compiler:process(F)
    catch T:Err ->
        io:format("~n~s~n~p ~p~n~p~n", [
            color:red("E4: Failed"),
            T, Err, erlang:get_stacktrace()])
    end,
    start_e4_compiler(Tail).
