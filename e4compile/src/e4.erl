%%%-------------------------------------------------------------------
%% @doc E4 Erlang-Forth public API
%% @end
%%%-------------------------------------------------------------------

-module(e4).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, arguments_loop/1, start/0, try_do/2]).

-include("e4.hrl").

%%====================================================================
%% API
%%====================================================================

start() ->
    start(normal, init:get_plain_arguments()).

start(_StartType, Args) ->
    arguments_loop(Args),
    init:stop(),
    {ok, self()}.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

arguments_loop([]) ->
    io:format("~s~n", [color:greenb("E4: All arguments were processed")]),
    ok;
arguments_loop([F | Tail]) ->
    io:format("E4: Processing: ~p...~n", [F]),
    try
        try_do("top-level compiler invocation",
               fun() -> e4_compiler:process(F) end),
        arguments_loop(Tail)
    catch throw:compile_failed ->
        io:format("~s - ~s~n", [color:yellow("E4: Compilation failed"), F])
    end.

try_do(What, Fun) ->
    try Fun()
    catch
        throw:compile_failed ->
            erlang:throw(compile_failed); % chain the error out
        T:Err ->
            io:format("~n~s (~s): ~s~n"
                      "~p~n",
                      [color:red("E4: Failed"),
                       color:yellow(What),
                       ?COLOR_TERM(redb, {T, Err}),
                       erlang:get_stacktrace()
                       %?COLOR_TERM(blackb, erlang:get_stacktrace())
                      ]),
            erlang:throw(compile_failed)
    end.
