-module(uerlc_compile).
-export([process/1]).

%% @doc Takes filename as input, produces compiled BEAM assembly AST and then
%% passes are invoked to process it further
-spec process(string()) -> #{}.
process(F) ->
  case compile:file(F, ['S', binary, report]) of
    {ok, _ModuleName, BeamS} ->
      uerlc:try_do(
        "e4_pass_S - Erlang 'S' Assembly to E4 Assembly",
        fun() -> uerlc_pass_from_S:process(BeamS) end
      );
      %% TODO optimize {label1} Code {label2} Code can be collapsed
      %% into {label1}{label} Code if the code is identical
    Error ->
      io:format("~n~s: ~p~n", [F, Error])
  end.
