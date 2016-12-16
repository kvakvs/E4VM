-module(e4_compiler).
-export([process/1]).

-include("e4_forth.hrl").

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
process(F) ->
    case compile:file(F, [to_core, binary, report]) of
        {ok, _M, CoreErlang} ->
            CoreForth = e4_pass2:process(CoreErlang),
            Forth = e4_pass1:process(CoreForth),
            _Binary = e4_forth_bytecode:process(Forth);
        E ->
            io:format("~n~s: ~p~n", [F, E])
    end.
