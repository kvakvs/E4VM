%%% @doc #j1prog manipulation and helpers
%%% @end
-module(j1c_prog).

%% API
-export([
      atom_index_or_create/2,
      lit_arbitrary/2,
      lit_atom/2,
      lit_funarity/2,
      lit_mfa/2,
      literal_index_or_create/2
]).

-include_lib("e4c/include/forth.hrl").
-include_lib("j1c/include/j1.hrl").

%% @doc Looks up an atom in the atom table, returns its paired value or creates
%% a new atom, assigns it next available index and returns it
atom_index_or_create(Prog0 = #j1prog{atom_id = AtomId, atoms = Atoms}, Value)
    when is_binary(Value) ->
    case orddict:find(Value, Atoms) of
        error ->
            Prog1 = Prog0#j1prog{
                atom_id = AtomId + 1,
                atoms = orddict:store(Value, AtomId, Atoms)
            },
            {Prog1, AtomId};
        {ok, Existing} ->
            {Prog0, Existing}
    end.

%% @doc Looks up a literal in the literal table, returns its paired value or
%% creates a new literal, assigns it next available index and returns it
literal_index_or_create(Prog0 = #j1prog{lit_id = LitId, literals = Literals},
                        Value) ->
    case orddict:find(Value, Literals) of
        error ->
%%            io:format("literal create ~p~n", [Value]),
            Prog1 = Prog0#j1prog{
                lit_id = LitId + 1,
                literals = orddict:store(Value, LitId, Literals)
            },
            #{p => Prog1,
              lit_index => LitId};
        {ok, Existing} ->
            #{p => Prog0,
              lit_index => Existing}
    end.

lit_atom(Prog0 = #j1prog{}, Word) when is_binary(Word) ->
    {Prog1, AIndex} = j1c_prog:atom_index_or_create(Prog0, Word),
    #{ p => Prog1,
       forth => #j1atom{id = AIndex, debug = Word}
    }.

%% @doc Create a literal  in program dictionary, and return modified
%% #j1prog{} and its new representation
lit_mfa(Prog0 = #j1prog{}, {M, F, A1}) when is_integer(A1) ->
    M1 = eval(M),
    F1 = eval(F),
    #{p := Prog1, lit_index := LIndex} =
        j1c_prog:literal_index_or_create(Prog0, {?TAG_LIT_MFARITY, M1, F1, A1}),
    #{p => Prog1,
      forth => #j1lit{id = LIndex, debug = {?TAG_LIT_MFARITY, M, F, A1}}
    }.

lit_funarity(Prog0 = #j1prog{}, {F, A1}) when is_integer(A1) ->
    F1 = eval(F),
    #{p := Prog1, lit_index := LIndex} =
        j1c_prog:literal_index_or_create(Prog0, {?TAG_LIT_FUNARITY, F1, A1}),
    #{p => Prog1,
      forth => #j1lit{id = LIndex, debug = {?TAG_LIT_FUNARITY, F, A1}}
    }.

lit_arbitrary(Prog0 = #j1prog{}, Lit) ->
    #{p := Prog1, lit_index := LIndex} =
        j1c_prog:literal_index_or_create(Prog0, Lit),
    #{p => Prog1,
      forth => #j1lit{id = LIndex, debug = Lit}
    }.

eval(#k_atom{val = A}) -> erlang:binary_to_atom(A, utf8);
eval(Bin) when is_binary(Bin) -> Bin.
