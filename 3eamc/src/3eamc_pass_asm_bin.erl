-module('3eamc_pass_asm_bin').
-include("3eamc.hrl").

%% API
-export([transform/1]).
-import('3eamc_encode', [varint/1, val_int/1, val_zreg/1, val_xreg/1]).

%% Encode
transform({'#label', _N}) -> [];
transform({'_op_move', A, B}) ->
    [?OPCODE_MOVE, value(A), value(B)];
%%transform({'_op_syscall', Id}) when Id >= 0 andalso Id =< 15 ->
%%    ?OPCODE_SYSCALL + (Id bsl 4);
transform({'_op_call', Arity}) ->
    [?OPCODE_CALL, varint(Arity)];
transform({'_op_tail_call', Arity}) ->
    [?OPCODE_TAIL_CALL, varint(Arity)];
transform({'_op_cons', H, T, Dst}) ->
    [?OPCODE_CONS, value(H), value(T), value(Dst)];
transform({'_op_ret'}) ->
    ?OPCODE_RET;
transform({'_op_test1', Pred, Fail, Arg1}) ->
    [?OPCODE_TEST1, predicate(Pred), value(Fail), value(Arg1)];
transform({'_op_test2', Pred, Fail, Arg1, Arg2}) ->
    [?OPCODE_TEST1, predicate(Pred), value(Fail), value(Arg1), value(Arg2)];
transform({'_op_decons', H, T, List}) ->
    [?OPCODE_DECONS, value(H), value(T), value(List)];
transform(X) ->
    io:format(?MODULE_STRING ++ " Cannot transform: ~p", [X]),
    erlang:error({asm_to_bin_error, ?MODULE}).

predicate(is_eq_exact)      -> 0;
predicate(is_nil)           -> 1;
predicate(is_nonempty_list) -> 2;
predicate(X) ->
    erlang:error({unknown_predicate, X}).

%% Encodes one value or slot
value(nil) -> ?VAL_NIL;
value(N) when is_integer(N) -> [?VAL_INTEGER, varint(N)];
value({integer, N}) -> [?VAL_INTEGER, varint(N)];
value({x, X}) -> [?VAL_X, varint(X)];
value({y, Y}) -> [?VAL_Y, varint(Y)];
value({z, Z}) -> [?VAL_Z + (Z bsl 4)];
value({f, Label}) -> [varint(Label)];
value({atom, A}) -> [?VAL_ATOM, atom_index(A)];
value({extfunc, Mod, Fun, Arity}) ->
    [?VAL_MFARITY, atom_index(Mod), atom_index(Fun), varint(Arity)];
value({imm, L}) ->
    case classify_value(L) of
        {ok, Type} ->
            value({Type, L});
        false -> % encode as a literal
            value({literal, L})
    end;
value({literal, L}) -> [?VAL_LIT, literal_index(L)].

classify_value(X) when is_atom(X) -> {ok, atom};
classify_value(X) when is_integer(X) -> {ok, integer};
classify_value(_) -> false.

%% Registers atom in the atom table (unique), returns binary representation
%% ready to be written
atom_index(A) ->
    case ets:lookup(atoms, A) of
        [] ->
            Info = ets:info(atoms),
            Id2 = proplists:get_value(size, Info),
            ets:insert(atoms, {A, Id2}),
            varint(Id2);
        [{A, Id1}|_] ->
            varint(Id1)
    end.

%% Registers a literal the table (unique), returns binary representation
%% ready to be written
literal_index(L) ->
    case ets:lookup(literals, L) of
        [] ->
            Info = ets:info(literals),
            Id2 = proplists:get_value(size, Info),
            ets:insert(literals, {L, Id2}),
            varint(Id2);
        [{L, Id1}|_] ->
            varint(Id1)
    end.
