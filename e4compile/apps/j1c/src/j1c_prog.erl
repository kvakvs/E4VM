%%% @doc #j1prog manipulation and helpers
%%% @end
-module(j1c_prog).

%% API
-export([atom_index_or_create/2, literal_index_or_create/2, lit/3]).

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
            {Prog1, LitId};
        {ok, Existing} ->
            {Prog0, Existing}
    end.

%% @doc Create a literal or atom in program dictionary, and return modified
%% #j1prog{} and its new representation
lit(Prog0 = #j1prog{}, atom, Word) when is_binary(Word) ->
    {Prog1, AIndex} = j1c_prog:atom_index_or_create(Prog0, Word),
    {Prog1,
     #j1atom{id = AIndex, debug = Word}
    };
lit(Prog0 = #j1prog{}, mfa, {M, F, A1}) when is_integer(A1) ->
    M1 = eval(M),
    F1 = eval(F),
    {Prog1, LIndex} = j1c_prog:literal_index_or_create(Prog0, {'$MFA', M1, F1, A1}),
    {Prog1,
     #j1lit{id = LIndex, debug = {mfa, M, F, A1}}
    };
lit(Prog0 = #j1prog{}, funarity, {F, A1}) when is_integer(A1) ->
    F1 = eval(F),
    {Prog1, LIndex} = j1c_prog:literal_index_or_create(Prog0, {'$FA', F1, A1}),
    {Prog1,
     #j1lit{id = LIndex, debug = {funarity, F, A1}}
    };
lit(Prog0 = #j1prog{}, arbitrary, Lit) ->
    {Prog1, LIndex} = j1c_prog:literal_index_or_create(Prog0, Lit),
    {Prog1,
     #j1lit{id = LIndex, debug = Lit}
    }.

eval(#k_atom{val = A}) -> erlang:binary_to_atom(A, utf8);
eval(Bin) when is_binary(Bin) -> Bin.
