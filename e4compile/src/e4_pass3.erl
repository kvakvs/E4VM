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
    io:format("PASS3~n~p~n", [Output]),
    Output.

process_code(Code, []) -> lists:flatten(lists:reverse(Code));

process_code(Code, [Op | Tail]) ->
    Piece = process_code([], Op),
    process_code([Piece | Code], Tail);
process_code(Code, Op) -> % if a single item is given, like a root block
    [process_op(Op) | Code].

-spec process_op(intermediate_forth_op()) -> forth_code().
process_op(A) when is_atom(A) -> A; % pass through forth words
process_op(#f_lit{}=L) -> L; % pass through forth literals
process_op(#f_comment{comment=C}) -> ['(', C, ')'];
process_op(#f_enter{size=Size}) -> [e4_f:lit(Size), '.ENTER'];
process_op(#f_leave{size=Size}) -> [e4_f:lit(Size), '.LEAVE'];
process_op(#f_mfa{}=MFA) -> MFA;
process_op(Op) ->
    compile_error("E4 Pass3: Unknown op ~p~n", [Op]).
