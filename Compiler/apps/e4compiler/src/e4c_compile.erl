-module(e4c_compile).
-export([process/1]).

-include_lib("e4compiler/include/forth.hrl").

%% @doc Takes filename as input, produces compiled BEAM assembly AST and then
%% passes are invoked to process it further
-spec process(string()) -> {module(), [binary()]}.
process(F) ->
  case compile:file(F, ['S', binary, report]) of
    {ok, _ModuleName, BeamS} ->
      e4c:try_do("e4_pass_S - Erlang 'S' Assembly to E4 Assembly",
                 fun() -> e4c_pass_S:process(BeamS) end);
    Error ->
      io:format("~n~s: ~p~n", [F, Error])
  end.
