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
    '3eamc_state':atom_index(ModName),

    OutExports = write_exports(Exports, []),
    OutCode = lists:map(
        fun(Op) ->
            io_lib:format("~p~n", [Op])
        end,
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
    Out = ['3eamc_state':atom_index(Fun), varint(Arity)],
    write_exports(Exports, [Out | Accum]).

%% Prepents a tag (signature) and varint section size to the data
write_section(Iolist, Signature) ->
    [Signature, varint(iolist_size(Iolist)) | Iolist].
