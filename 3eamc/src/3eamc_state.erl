-module('3eamc_state').

%% API
-export([atom_index/1, literal_index/1]).

%% Registers atom in the atom table (unique), returns binary representation
%% ready to be written
atom_index(A) ->
    case ets:lookup(atoms, A) of
        [] ->
            Info = ets:info(atoms),
            Id2 = proplists:get_value(size, Info),
            ets:insert(atoms, {A, Id2}),
            Id2;
        [{A, Id1}|_] -> Id1
    end.

%% Registers a literal the table (unique), returns binary representation
%% ready to be written
literal_index(L) ->
    case ets:lookup(literals, L) of
        [] ->
            Info = ets:info(literals),
            Id2 = proplists:get_value(size, Info),
            ets:insert(literals, {L, Id2}),
            Id2;
        [{L, Id1}|_] -> Id1
    end.

