-module('3eamc_compiler').
-export([process/1]).

-include("3eamc.hrl").
-import('3eamc_encode', [varint/1, val_zreg/1, val_int/1]).
-import('3eamc_pass_beam_forth', [asm_move/2, asm_syscall/1, f_emit_call/3, asm_apply/2]).

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
process(F) ->
    case compile:file(F, [to_asm, binary, report]) of
        {ok, _M, Result} ->
            process_asm(F, Result);
        E ->
            io:format("~s: ~p~n", [F, E])
    end.

%% File format:
%% ?SIGNATURE
%% ?SIG_EXPORTS, int:length(Exports), [int:fun_atom, int:arity]
%% ?SIG_CODE, (see code format at write_code below)
%% ?SIG_ATOMS, int:length(Atoms), [int:length(atom_utf8), characters]
%% // atom[0] is always module name
process_asm(F, {ModName, Exports, _I, Code, _NumLabels}) ->
    ets:new(atoms, [named_table]),
    ets:new(literals, [named_table]),
    atom_index(ModName),

    OutExports = write_exports(Exports, []),
    OutCode = lists:map(fun(Op) -> io_lib:format("~p~n", [Op]) end,
        '3eamc_pass_beam_forth':process(Code, [])),
OutImports = [],
    OutLiterals = [],
    %% Atoms goes last
    OutAtoms = write_all_atoms(),

    Out = [?SIGNATURE,
        write_section(OutAtoms,   ?SIG_ATOMS),
        write_section(OutCode,    ?SIG_CODE),
        write_section(OutExports, ?SIG_EXPORTS),
        OutImports,
        OutLiterals
    ],
    file:write_file(F ++ ".3eam", Out),
    ok.

write_atoms_write_one(A) ->
    ABin = atom_to_binary(A, utf8),
    [(varint(byte_size(ABin))), ABin].

write_all_atoms() ->
    All0 = ets:tab2list(atoms),
    All1 = lists:keysort(2, All0),
    [varint(length(All0)) |
        [write_atoms_write_one(Atom) || {Atom, _Id} <- All1]].

write_exports([], Accum) -> Accum;
write_exports([{Fun, Arity} | Exports], Accum) ->
    Out = [atom_index(Fun), varint(Arity)],
    write_exports(Exports, [Out | Accum]).

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

compile_forth_to_binary([], Accum) ->
    lists:flatten(lists:reverse(Accum));
compile_forth_to_binary([Item | Tail], Accum) ->
    io:format("-- asm_bin ~p...~n", [Item]),
    compile_forth_to_binary(Tail, [
        '3eamc_pass_forth_bin':transform(Item) | Accum
    ]).


%% Prepents a tag (signature) and varint section size to the data
write_section(Iolist, Signature) ->
    [Signature, varint(iolist_size(Iolist)) | Iolist].


%% Encodes one value or slot
value(N) when is_integer(N) -> [?VAL_INTEGER, varint(N)];
value({x, X}) -> [?VAL_X, varint(X)];
value({y, Y}) -> [?VAL_Y, varint(Y)];
value({f, Label}) -> [varint(Label)];
value({atom, A}) -> [?VAL_ATOM, atom_index(A)];
value({extfunc, Mod, Fun, Arity}) ->
    [?VAL_MFARITY, atom_index(Mod), atom_index(Fun), varint(Arity)];
value({literal, L}) -> [?VAL_LIT, literal_index(L)].
