-module(e4_forth).

%% API
-export(['and'/1, 'if'/2, 'if'/3, tuple/1, compare/2, lit/1, comment/2, nil/0]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4.hrl").

%% Takes list of Forth checks and creates forth instructions which produce
%% true if all conditions are true
%% Assumption: each Cond in Conds is a Forth sequence which leaves one value on stack
'and'([]) -> [];
'and'(Conds) ->
    %% Remove true clauses
    Conds1 = lists:filter(
        fun(#c_literal{val='true'}) -> false;
            ([]) -> false;
            (_) -> true
        end,
        Conds),
    case Conds1 of
        [] -> [];
        _ -> [Conds1, lists:duplicate(length(Conds1) - 1, 'AND')]
    end.

'if'(#e4lit{val='true'}, Body) -> Body;
'if'(Cond, Body) ->
    [?Lazy(Cond), 'IF', ?Lazy(Body), 'THEN'].

'if'(Cond, Body, Else) ->
    [?Lazy(Cond), 'IF', ?Lazy(Body), 'ELSE', ?Lazy(Else), 'THEN'].

%% Takes list of Forth expressions where each leaves one value on stack
%% and constructs a tuple of that size
tuple(Values) ->
    [lists:reverse(Values), length(Values), 'MAKE-TUPLE'].

compare(Lhs, Rhs) ->
    [Lhs, Rhs, '=='].

lit(Value) -> #e4lit{val=Value}.

comment(Format, Args) ->
    Txt = iolist_to_binary(io_lib:format(Format, Args)),
    #e4comment{comment=Txt}.

nil() -> 'NIL'.
