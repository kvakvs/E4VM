%%% @doc Forth source handling

-module(j1c_parse).

%% API
-export([parse/1]).
-include_lib("e4c/include/e4c.hrl").

parse(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            Parts = binary:split(
                Content,
                [<<" ">>, <<"\n">>, <<"\t">>, <<"\r">>],
                [global] % R17 does not have trim_all
            ),
            Parts1 = lists:filter(fun(<<>>) -> false; (_) -> true end, Parts),
            lists:reverse(parse_postprocess(Parts1, []));
        {error, E} ->
            ?COMPILE_ERROR("E4: Include file ~p error ~p", [Filename, E])
    end.

parse_postprocess([], Out) -> lists:flatten(Out);
parse_postprocess([<<"(">> | Tail], Out) ->
    %% TODO: produce #f_comment{} instead
    Tail1 = lists:dropwhile(fun (<<")">>) -> false; (_) -> true end, Tail),
    [<<")">> | Tail2] = Tail1,
    parse_postprocess(Tail2, Out);
parse_postprocess([Op | Tail], Out) ->
    Out1 = [parse_postprocess_op(Op)] ++ [Out],
    parse_postprocess(Tail, Out1).

%%parse_postprocess_op(<<>>) -> [];
parse_postprocess_op(Op) -> Op.
