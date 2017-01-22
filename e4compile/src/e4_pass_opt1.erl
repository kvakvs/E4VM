%%% @doc Try find some bad code patterns and optimize them

-module(e4_pass_opt1).

%% API
-export([process/1]).
-include("e4_forth.hrl").

process(Forth) ->
    Output = optimize_code([], Forth),
    Output1 = optimize_code([], Output),
%%    _Output2 = lists:map(
%%        fun(<<";">>) -> io_lib:format(";~n", []);
%%            (X) when is_integer(X) -> io_lib:format("~p ", [X]);
%%            (X) -> io_lib:format("~s ", [X])
%%        end,
%%        Output1),
%%    io:format("~s~n~s~n", [color:redb("PASS4 OPT#1"), Output2]),
    Output1.

%% TODO store(x) + retrieve(x) maybe add a custom opcode to write without
%% consuming stack
%% TODO LD (something) LD again = can go with DUP
%% TODO jump or call-tail + leave = remove leave or something
%% TODO: enter(0) is omitted so also can omit leave maybe?
%% TODO: leave ret leave ;

optimize_code(Code, []) -> lists:flatten(lists:reverse(Code));
%%optimize_code(Code, [E1, ?F_ENTER, E2, ?F_ENTER | Tail])
%%    when is_integer(E1), is_integer(E2) ->
%%    Sum = E1+E2,
%%    io:format("simplify enter(~p) + enter(~p)~n", [E1, E2]),
%%    optimize_code([[Sum, ?F_ENTER] | Code], Tail);
%%optimize_code(Code, [L1, ?F_LEAVE, L2, ?F_LEAVE | Tail])
%%    when is_integer(L1), is_integer(L2) ->
%%    Sum = L1 + L2,
%%    io:format("simplify leave(~p) + leave(~p)~n", [L1, L2]),
%%    optimize_code([[Sum, ?F_LEAVE] | Code], Tail);
optimize_code(Code, [Op | Tail]) ->
    %% if a single item is given, like a root block
    optimize_code([Op | Code], Tail).
