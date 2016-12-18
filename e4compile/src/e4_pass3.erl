%%% @doc Compile Forth helper records into a Forth program consisting of 'words'
%%% and 'literals' only.
%%% This is a simple pass which carries no context object and only accumulates
%%% the code output in a list.

-module(e4_pass3).

%% API
-export([process/1]).
-include("e4_forth.hrl").
-import(e4, [compile_error/2]).

process(Forth) ->
    Output = process_code([], Forth),
    Output2 = lists:map(
        fun(<<";">>) -> io_lib:format(";~n", []);
            (X) when is_integer(X) -> io_lib:format("~p ", [X]);
            (X) -> io_lib:format("~s ", [X]) end,
        Output),
    io:format("~s~n~s~n", [color:redb("PASS3"), Output2]),
%%    io:format("~s~n~p~n", [color:redb("PASS3"), Output]),
    Output.

process_code(Code, []) -> lists:flatten(lists:reverse(Code));

process_code(Code, [Op | Tail]) ->
    Piece = process_code([], Op),
    process_code([Piece | Code], Tail);
process_code(Code, Op) -> % if a single item is given, like a root block
    [process_op(Op) | Code].

-spec process_op(intermediate_forth_op()) -> forth_code().
process_op(A) when ?IS_FORTH_WORD(A) -> A; % forth words (as atoms)
%%process_op(B) when is_binary(B) -> B; % forth words (parsed from disk)

process_op(#f_lit{val=V}) -> f_lit(V);
process_op(#f_mfa{mod=M, fn=F, arity=A}) ->
    [<<".MFA">>, f_lit(M), f_lit(F), f_lit(A)];

process_op(#f_comment{comment=C}) -> []; %['(', C, ')'];
process_op(#f_enter{size=Size}) -> [f_lit(Size), <<".ENTER">>];
process_op(#f_leave{size=Size}) -> [f_lit(Size), <<".LEAVE">>];
process_op(Op) ->
    compile_error("E4 Pass3: Unknown op ~p~n", [Op]).

f_lit(V) when is_integer(V) -> V;
f_lit(V) when is_atom(V) -> <<"'", (atom_to_binary(V, utf8))/binary>>.
