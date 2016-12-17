-module(e4_compiler).
-export([process/1]).

-include("e4_forth.hrl").

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
process(F) ->
    case compile:file(F, [to_core, binary, report]) of
        {ok, _M, CoreErlang} ->
            Forth1 = e4_pass1:process(CoreErlang),
            Forth2 = e4_pass2:process(Forth1),
            RealForth1 = e4_pass3:process(Forth2),
            _RealForth2 = e4_pass_opt1:process(RealForth1);
        E ->
            io:format("~n~s: ~p~n", [F, E])
    end.
