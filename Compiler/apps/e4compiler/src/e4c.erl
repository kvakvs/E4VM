%%%-------------------------------------------------------------------
%% @doc E4 Erlang-Forth public API
%% @end
%%%-------------------------------------------------------------------

-module(e4c).

-behaviour(application).

%% Application callbacks
-export([
  arguments_loop/1,
  debug/1,
  debug_write_term/2,
  error/1,
  start/0,
  start/2,
  stop/1,
  try_do/2,
  varint/1
]).

-include_lib("e4compiler/include/e4c.hrl").

%%====================================================================
%% API
%%====================================================================

start() ->
  start(normal, init:get_plain_arguments()).

%% @doc Pass in a string which will be compiled
debug(Filename) ->
  arguments_loop([Filename]).

start(_StartType, Args) ->
  %% TODO: Asking for trouble here but hell is this fast
  put(e4_machine_word_bits, 64),

  arguments_loop(Args),
  init:stop().

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
    compile(InputPath),
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

compile(InputPath) ->
  %% Process Kernel to Assembler transformation
  Module = try_do("top-level E4C invocation",
    fun() -> e4c_compile:process(InputPath) end
  ),
  try_do("top-level E4ASM invocation",
    fun() -> e4asm_compile:process(InputPath, Module) end
  ).

  %% Compile E4 Assembler to Forth bytecode transformation
%%  try_do("top-level E4ASM invocation",
%%         fun() -> e4asm_compile:process(InputPath, Mod, Tokens) end).

%% @doc Variable length unsigned int encoding, highest bit is set to 1 for every
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
