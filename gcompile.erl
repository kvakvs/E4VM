-module(gcompile).
-compile([export_all]).

-define(SIGNATURE, <<"3EAM">>).
-define(SIG_EXPORTS, <<"ExpT">>).
-define(SIG_ATOMS, <<"Atom">>).
-define(SIG_CODE, <<"Code">>).

-define(MARK_FUNCTION, <<255:8>>).
-define(MARK_END_CODE, <<254:8>>).
-define(VAL_X,          1).
-define(VAL_Y,          2).
-define(VAL_ATOM,       3).
-define(VAL_MFARITY,    4).
-define(VAL_INTEGER,    5).
-define(VAL_LIT,        6).

-define(OPCODE_MOVE,        1).
-define(OPCODE_CALL,        2).
-define(OPCODE_TAIL_CALL,   2+128).
-define(OPCODE_APPLY,       3).
-define(OPCODE_TAIL_APPLY,  3+128).
-define(OPCODE_CONS,        4).
-define(OPCODE_RET,         5).
-define(OPCODE_ALLOC,       6).
-define(OPCODE_DEALLOC,     6+128).

main([]) ->
    io:format("All arguments were processed~n", []),
    ok;
main([F | Tail]) ->
    io:format("Processing: ~p~n...", [F]),
    process(atom_to_list(F)),
    main(Tail).

process(F) ->
    case compile:file(F, ['S', binary, report]) of
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
    OutCode = write_code(Code, []),
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

%% Variable length UTF8-like encoding, highest bit is set to 1 for every
%% encoded 7+1 bit sequence, and is set to 0 in the last 7+1 bits
varint(N) when N < 0 -> erlang:error("varint: n<0");
varint(N) when N =< 127 -> <<0:1, N:7>>; % last byte
varint(N) -> [<<1:1, (N rem 128):7>>, varint(N bsr 7)].

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

write_code([], Accum) -> Accum;
write_code([Item | Tail], Accum) ->
    write_code(Tail, [process_code_item(Item) | Accum]).

%% Encodes one tuple from code (in the compiled BEAM)
%% This function is used for single BEAM instructions. Sequences of BEAM
%% instructions are TODO process sequences in the outer loop (write_code)
process_code_item({function, Name, Arity, _Label, Code}) ->
    [?MARK_FUNCTION, atom_index(Name), varint(Arity),
        varint(length(Code)),
        [process_code_item(I) || I <- Code]
    ];
process_code_item({label, _L}) -> [];
process_code_item({line, _LN}) -> [];
process_code_item({func_info, _Mod, _Fun, _Arity}) -> [];
process_code_item({allocate, A, B}) ->
    [?OPCODE_ALLOC, varint(A), varint(B)];
process_code_item({deallocate, A}) ->
    [?OPCODE_DEALLOC, varint(A)];
process_code_item({move, Src, Dst}) ->
    [?OPCODE_MOVE, value(Src), value(Dst)];
process_code_item({call, _Arity, Label}) ->
    [?OPCODE_CALL, value(Label)];
process_code_item({call_ext_only, _Arity, MFArity}) ->
    [?OPCODE_TAIL_CALL, value(MFArity)];
process_code_item({apply, Arity}) ->
    %% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
    [?OPCODE_APPLY, value(Arity)];
process_code_item({apply_last, Arity, _}) ->
    %% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
    [?OPCODE_TAIL_APPLY, value(Arity)];
process_code_item({test_heap, _M, _N}) -> [];
process_code_item({put_list, Head, Tail, Dst}) ->
    [?OPCODE_CONS, value(Head), value(Tail), value(Dst)];
process_code_item({put_tuple, N, Dst}) -> [?OPCODE_CALL, value(Dst)];
process_code_item({put, Val}) -> value(Val);
process_code_item(return) -> ?OPCODE_RET;
process_code_item(Other) ->
    io:format("Unsupported opcode ignored: ~p~n", [Other]),
    [].

%% Encodes one value or slot
value(N) when is_integer(N) -> [?VAL_INTEGER, varint(N)];
value({x, X}) -> [?VAL_X, varint(X)];
value({y, Y}) -> [?VAL_Y, varint(Y)];
value({f, Label}) -> [varint(Label)];
value({atom, A}) -> [?VAL_ATOM, atom_index(A)];
value({extfunc, Mod, Fun, Arity}) ->
    [?VAL_MFARITY, atom_index(Mod), atom_index(Fun), varint(Arity)];
value({literal, L}) -> [?VAL_LIT, literal_index(L)].

%% Prepents a tag (signature) and varint section size to the data
write_section(Iolist, Signature) ->
    [Signature, varint(iolist_size(Iolist)) | Iolist].
