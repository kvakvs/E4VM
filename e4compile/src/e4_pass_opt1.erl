%%% @doc Try find some patterns and optimize them

-module(e4_pass_opt1).

%% API
-export([process/1]).
-include("e4_forth.hrl").

process(Forth) ->
    Output = optimize_code([], Forth),
    Output1 = optimize_code([], Output),
    _Output2 = lists:map(
        fun(<<";">>) -> io_lib:format(";~n", []);
            (X) when is_integer(X) -> io_lib:format("~p ", [X]);
            (X) -> io_lib:format("~s ", [X])
        end,
        Output1),
%%    io:format("~s~n~s~n", [color:redb("PASS4 OPT#1"), Output2]),
    Output.

%% TODO store(x) + retrieve(x) maybe add a custom opcode to write without consuming stack

optimize_code(Code, []) -> lists:flatten(lists:reverse(Code));
optimize_code(Code, [E1, <<".ENTER">>, E2, <<".ENTER">> | Tail])
    when is_integer(E1), is_integer(E2) ->
    Sum = E1+E2,
    io:format("simplify enter(~p) + enter(~p)~n", [E1, E2]),
    optimize_code([[Sum, <<".ENTER">>] | Code], Tail);
optimize_code(Code, [L1, <<".LEAVE">>, L2, <<".LEAVE">> | Tail])
    when is_integer(L1), is_integer(L2) ->
    Sum = L1 + L2,
    io:format("simplify leave(~p) + leave(~p)~n", [L1, L2]),
    optimize_code([[Sum, <<".LEAVE">>] | Code], Tail);
optimize_code(Code, [Op | Tail]) ->
    %% if a single item is given, like a root block
    optimize_code([Op | Code], Tail).

%%process_code(Code, [Op | Tail]) ->
%%    Piece = process_code([], Op),
%%    process_code([Piece | Code], Tail);
%%process_code(Code, Op) -> % if a single item is given, like a root block
%%    [process_op(Op) | Code].

%%-spec process_op(intermediate_forth_op()) -> forth_code().
%%process_op(Op) -> Op.
%%%%    compile_error("E4 Pass3: Unknown op ~p~n", [Op]).
