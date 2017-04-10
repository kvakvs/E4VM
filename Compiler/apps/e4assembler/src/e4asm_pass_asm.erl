%%% @doc Given simplified assembly AST produce binary E4 VM bytecode.

-module(e4asm_pass_asm).

%% API
-export([compile/1]).

-include_lib("e4compiler/include/e4c.hrl").

compile(Prog0) ->
    Prog3 = [], %process_words(Prog0),

    e4c:debug_write_term("e4asm_pass_asm.txt", Prog3),
%%    io:format("~s~n~p~n", [color:redb("J1C PASS 1"), J1Forth]),
    Prog3.
