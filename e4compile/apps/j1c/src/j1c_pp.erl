%%% @doc Forth Preprocessor.
%%% Analyzes source, replaces literal groups of keywords with #j1lit etc
%%% @end
-module(j1c_pp).

%% API
-export([process/2]).

-include_lib("e4c/include/forth.hrl").
-include_lib("j1c/include/j1.hrl").

process(ModuleName, SrcForth) ->
    Prog0 = #j1prog{mod = ModuleName},

    % Module name should always be #0
    {Prog1, _} = j1c_prog:atom_index_or_create(
        Prog0, atom_to_binary(ModuleName, utf8)),

    Preprocessed1 = preprocess_numbers(SrcForth, []),
    {Prog2, Preprocessed2} = preprocess(Prog1, [], Preprocessed1),
    file:write_file("j1c_pass_forth-pp.txt",
                    iolist_to_binary(io_lib:format("~p", [Preprocessed2]))),
    {Prog2, Preprocessed2}.

preprocess(Prog = #j1prog{}, Acc, []) ->
    {Prog, lists:flatten(lists:reverse(Acc))};

preprocess(Prog0 = #j1prog{}, Acc, [?F_LIT_ATOM, A | Tail]) ->
    {Prog1, Repr} = j1c_prog:lit(Prog0, atom, A),
    preprocess(Prog1, [Repr | Acc], Tail);
preprocess(Prog0 = #j1prog{}, Acc, [#k_atom{val = A} | Tail]) ->
    {Prog1, Repr} = j1c_prog:lit(Prog0, atom, A),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog0 = #j1prog{},
           Acc,
           [?F_LIT_MFA, ?F_LIT_ATOM, M, ?F_LIT_ATOM, F, Arity | Tail]) ->
    {Prog1, Repr} = j1c_prog:lit(Prog0, mfa, {M, F, Arity}),
    preprocess(Prog1, [Repr | Acc], Tail);
preprocess(Prog0 = #j1prog{},
           Acc,
           [?F_LIT_FUNA, ?F_LIT_ATOM, Fn, Arity | Tail]) ->
    io:format("lit funa ~p ~B~n", [Fn, Arity]),
    {Prog1, Repr} = j1c_prog:lit(Prog0, funarity, {Fn, Arity}),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog0 = #j1prog{}, Acc, [#k_literal{val = L} | Tail]) ->
    {Prog1, Repr} = j1c_prog:lit(Prog0, arbitrary, L),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog, Acc, [H | Tail]) ->
    preprocess(Prog, [H | Acc], Tail).


preprocess_numbers([], Acc) -> lists:reverse(Acc);
preprocess_numbers([Word | Tail], Acc) ->
    io:format("~p~n", [Word]),
    case (catch erlang:binary_to_integer(Word)) of
        X when is_integer(X) -> preprocess_numbers(Tail, [X | Acc]);
        {'EXIT', {badarg, _}} -> preprocess_numbers(Tail, [Word | Acc])
    end.
