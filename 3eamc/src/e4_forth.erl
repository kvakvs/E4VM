-module(e4_forth).

%% API
-export([forth_and/1, forth_if/2, forth_if/3, forth_tuple/1,
    forth_compare/2]).

-include_lib("compiler/src/core_parse.hrl").

%% Takes list of Forth checks and creates forth instructions which produce
%% true if all conditions are true
%% Assumption: each Cond in Conds is a Forth sequence which leaves one value on stack
forth_and(Conds) ->
    %% Remove true clauses
    Conds1 = lists:filter(
        fun(#c_literal{val='true'}) -> false; (_) -> true end,
        Conds),
    [Conds1, lists:duplicate(length(Conds1), 'AND')].

forth_if(#c_literal{val='true'}, Body) -> Body;
forth_if(Cond, Body) ->
    [Cond, 'IF', Body, 'THEN'].

forth_if(Cond, Body, Else) ->
    [Cond, 'IF', Body, 'ELSE', Else, 'THEN'].

%% Takes list of Forth expressions where each leaves one value on stack
%% and constructs a tuple of that size
forth_tuple(Values) ->
    [lists:reverse(Values), length(Values), 'MAKE-TUPLE'].

forth_compare(Lhs, Rhs) ->
    [Lhs, Rhs, '=='].
