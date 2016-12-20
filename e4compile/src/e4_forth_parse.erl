%%% @doc Forth source handling

-module(e4_forth_parse).

%% API
-export([parse/1]).
-include("e4.hrl").

parse(Filename) ->
    case file:read_file(Filename) of
        {ok, F} ->
            Parts = binary:split(
                F,
                [<<" ">>, <<"\n">>, <<"\t">>, <<"\r">>],
                [global, trim_all]
            ),
            lists:reverse(parse_postprocess(Parts, []));
        {error, E} ->
            ?COMPILE_ERROR("E4: Include file ~p error ~p", [Filename, E])
    end.

parse_postprocess([], Out) -> lists:flatten(Out);
parse_postprocess([<<"(">> | Tail], Out) ->
    Tail1 = lists:dropwhile(fun (<<")">>) -> false; (_) -> true end, Tail),
    [<<")">> | Tail2] = Tail1,
    parse_postprocess(Tail2, Out);
parse_postprocess([Op | Tail], Out) ->
    Out1 = [parse_postprocess_op(Op)] ++ [Out],
    parse_postprocess(Tail, Out1).

%%parse_postprocess_op(<<>>) -> [];
parse_postprocess_op(Op) -> Op.
