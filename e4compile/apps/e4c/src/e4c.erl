%%%-------------------------------------------------------------------
%% @doc E4 Erlang-Forth public API
%% @end
%%%-------------------------------------------------------------------

-module(e4c).

-behaviour(application).

%% Application callbacks
-export([
      arguments_loop/1,
      debug_write_term/2,
      error/1,
      start/0,
      start/2,
      stop/1,
      try_do/2,
      varint/1
]).

-include_lib("e4c/include/e4c.hrl").

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
arguments_loop([InputPath | Tail]) ->
    io:format("E4: Processing: ~p...~n", [InputPath]),
    try
        %% Process Kernel to Forth transformation
        {_Mod, _Tokens} = try_do("top-level E4C invocation",
                               fun() -> e4c_compile:process(InputPath) end),
        %% Process Forth tokens to Forth bytecode transformation
%%        try_do("top-level J1C invocation",
%%               fun() -> j1c_compile:process(InputPath, Mod, Tokens) end),
        arguments_loop(Tail)
    catch throw:compile_failed ->
        io:format("~s - ~s~n", [color:yellow("E4: Compilation failed"),
                                InputPath])
    end.

try_do(What, Fun) ->
    try Fun()
    catch
        throw:compile_failed ->
            erlang:throw(compile_failed); % chain the error out
        T:Err ->
            io:format("~n~s (~s): ~s~n"
                      "~s~n",
                      [color:white("E4: Failed"),
                       color:yellow(What),
                       ?COLOR_TERM(redb, {T, Err}),
                       ?COLOR_TERM(blackb, erlang:get_stacktrace())
                      ]),
            erlang:throw(compile_failed)
    end.


%% @doc Variable length UTF8-like encoding, highest bit is set to 1 for every
%% encoded 7+1 bit sequence, and is set to 0 in the last 7+1 bits
varint(N) when N < 0 -> erlang:error("varint: n<0");
varint(N) when N =< 127 -> <<0:1, N:7>>; % last byte
varint(N) ->
    Bytes = [<<0:1, (N rem 128):7>>, varint_with_bit(N bsr 7)],
    iolist_to_binary(lists:reverse(Bytes)).

%% @doc Same as varint with high bit always set
varint_with_bit(N) when N =< 127 -> <<1:1, N:7>>; % last byte
varint_with_bit(N) ->
    Bytes = [<<1:1, (N rem 128):7>>, varint_with_bit(N bsr 7)],
    iolist_to_binary(lists:reverse(Bytes)).

error(header) ->
    io:format("~n"
              "+--------------------------------------------------------~n"
              "| Error: ");
error(footer) ->
    io:format("~n"
              "+--------------------------------------------------------").

debug_write_term(Filename, Term) ->
    file:write_file(Filename,
                    erlang:iolist_to_binary(
                        io_lib:format("~p", [Term]))).
