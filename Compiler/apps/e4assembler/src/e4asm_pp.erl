%%% @doc Forth Preprocessor.
%%% Analyzes source, replaces literal groups of keywords with #j1lit etc
%%% @end
-module(e4asm_pp).

%% API
-export([forth_preproc/2]).

-include_lib("e4compiler/include/forth.hrl").
-include_lib("e4assembler/include/j1.hrl").

forth_preproc(ModuleName, SrcForth) ->
    Prog0 = #j1prog{mod = ModuleName},

    % Module name should always be #0
    {Prog1, _} = e4asm_prog:atom_index_or_create(
        Prog0, atom_to_binary(ModuleName, utf8)),

    Preprocessed1 = parse_numbers(SrcForth, []),
    #{p := Prog2, result := Preprocessed2} =
        preprocess(Prog1, [], Preprocessed1),
%%    e4c:debug_write_term("j1c_pass_forth-pp.txt", Preprocessed2),
    #{p => Prog2, result => Preprocessed2}.

-spec preprocess(j1prog(), list(), list()) -> #{p => j1prog(), result => list()}.
preprocess(Prog = #j1prog{}, Acc, []) ->
    #{p => Prog,
      result => lists:flatten(lists:reverse(Acc))};

preprocess(Prog0 = #j1prog{}, Acc, [?F_LIT_ATOM, A | Tail]) ->
    #{p := Prog1, forth := Repr} = e4asm_prog:lit_atom(Prog0, A),
    preprocess(Prog1, [Repr | Acc], Tail);
preprocess(Prog0 = #j1prog{}, Acc, [#k_atom{val = A} | Tail]) ->
    #{p := Prog1, forth := Repr} = e4asm_prog:lit_atom(Prog0, A),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog0 = #j1prog{},
           Acc,
           [?F_LIT_MFA, ?F_LIT_ATOM, M, ?F_LIT_ATOM, F, Arity | Tail]) ->
    #{p := Prog1, forth := Repr} = e4asm_prog:lit_mfa(Prog0, {M, F, Arity}),
    preprocess(Prog1, [Repr | Acc], Tail);
preprocess(Prog0 = #j1prog{},
           Acc,
           [?F_LIT_FUNA, ?F_LIT_ATOM, Fn, Arity | Tail]) ->
    #{p := Prog1, forth := Repr} = e4asm_prog:lit_funarity(Prog0, {Fn, Arity}),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog0 = #j1prog{}, Acc, [#k_literal{val = L} | Tail]) ->
    #{p := Prog1, forth := Repr} = e4asm_prog:lit_arbitrary(Prog0, L),
    preprocess(Prog1, [Repr | Acc], Tail);

preprocess(Prog, Acc, [H | Tail]) ->
    preprocess(Prog, [H | Acc], Tail).


parse_numbers([], Acc) -> lists:reverse(Acc);
parse_numbers([?F_LIT_ATOM, Word | Tail], Acc) -> % atoms can be '123' too!
    %% If atom keyword is followed by anything even a number, assume it is an
    %% atom and keep it binary, don't convert to an integer
    parse_numbers(Tail, [Word, ?F_LIT_ATOM | Acc]);
parse_numbers([Word | Tail], Acc) ->
    case (catch erlang:binary_to_integer(Word)) of
        X when is_integer(X) -> parse_numbers(Tail, [X | Acc]);
        {'EXIT', {badarg, _}} -> parse_numbers(Tail, [Word | Acc])
    end.
