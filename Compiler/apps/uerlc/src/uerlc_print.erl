%%% @doc Print the nice indented colored intermediate code
%%% @end

-module(uerlc_print).

-include_lib("uerlc/include/uerlc.hrl").

%% API
-export([format_ic/2]).

format_ic(L, Indent) when is_list(L) ->
    [format_ic(Item, Indent) || Item <- L];
format_ic(C, Indent) ->
    io_lib:format("~s~s~n", [i(Indent), format_op(C)]).

format_op(Other) ->
    ?COMPILE_ERROR("format_op: unknown ~p", [Other]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str({A, B}) when is_atom(A), is_integer(B) ->
    io_lib:format("~s/~p", [A, B]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).

i(I) when I =< 0 -> [];
i(I) -> lists:duplicate((I-1) * 4, 32).
