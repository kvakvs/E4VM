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

    {Prog2, Preprocessed} = preprocess(Prog1, [], SrcForth),
    file:write_file("j1c_pass_forth-pp.txt",
                    iolist_to_binary(io_lib:format("~p", [Preprocessed]))),
    {Prog2, Preprocessed}.

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
           [?F_LIT_MFA, ?F_LIT_ATOM, M, ?F_LIT_ATOM, F, ArityB | Tail]) ->
    Arity = erlang:binary_to_integer(ArityB),
    {Prog1, Repr} = j1c_prog:lit(Prog0, mfa, {M, F, Arity}),
    preprocess(Prog1, [Repr | Acc], Tail);
preprocess(Prog0 = #j1prog{},
           Acc,
           [?F_LIT_FUNA, ?F_LIT_ATOM, Fn, ArityB | Tail]) ->
    Arity = erlang:binary_to_integer(ArityB),
    io:format("lit funa ~p ~B~n", [Fn, Arity]),
    {Prog1, Repr} = j1c_prog:lit(Prog0, funarity, {Fn, Arity}),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog0 = #j1prog{}, Acc, [#k_literal{val = L} | Tail]) ->
    {Prog1, Repr} = j1c_prog:lit(Prog0, arbitrary, L),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog, Acc, [H | Tail]) ->
    preprocess(Prog, [H | Acc], Tail).
