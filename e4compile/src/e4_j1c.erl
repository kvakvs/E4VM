%%% @doc J1-like forth to binary compiler adjusted for Erlang needs with
%%% added types, literals etc.

-module(e4_j1c).

%% API
-export([compile/2]).
-include("e4_forth.hrl").
-include("e4_j1.hrl").
-import(e4, [compile_error/2]).

-record(program, {
    mod :: atom(),
    dict = orddict:new() :: orddict:orddict(binary(), integer()),
    dict_nif = orddict:new() :: orddict:orddict(binary(), integer()),
    consts = orddict:new() :: orddict:orddict(),
    vars = orddict:new() :: orddict:orddict(),
    modules = orddict:new() :: orddict:orddict(),
    condstack = [],

    atom_id = 0 :: integer(),
    atoms = orddict:new() :: orddict:orddict(binary(), integer()),

    pc = 0 :: integer(),
    output = [] :: iolist()
}).
-type program() :: #program{}.

compile(ModuleName, Input) ->
    Prog0 = #program{mod = ModuleName},
    Prog1 = compile2(Prog0, Input),
    lists:reverse(Prog1#program.output).


compile2(Prog0 = #program{}, []) -> Prog0;

%% --- special words ---
compile2(Prog0 = #program{}, [<<":">>, <<".MFA">>, Mod, Fn, Arity | Tail]) ->
    Prog1 = prog_add_word(Prog0, mfa_to_word(Prog0, Mod, Fn, Arity)),
    compile2(Prog1, Tail);

compile2(Prog0 = #program{}, [<<":MODULE">>, Name | Tail]) ->
    compile2(Prog0#program{mod=Name}, Tail);
compile2(Prog0 = #program{}, [<<":">>, Name | Tail]) ->
    Prog1 = prog_add_word(Prog0, Name),
    compile2(Prog1, Tail);
compile2(Prog0 = #program{}, [<<":NIF">>, Name, Index0 | Tail]) ->
    Index1 = binary_to_integer(Index0),
    Prog1 = prog_add_nif(Prog0, Name, Index1),
    compile2(Prog1, Tail);
compile2(Prog0 = #program{}, [<<";">> | Tail]) ->
    Prog1 = emit_alu(Prog0, #alu{op=0, rpc=1, ds=2}),
    compile2(Prog1, Tail);

%% --- literals ---
compile2(Prog0 = #program{}, [<<"'", Word/binary>> | Tail]) -> % a quoted 'atom
    Prog1 = emit_lit(Prog0, atom, Word),
    compile2(Prog1, Tail);
compile2(Prog0 = #program{}, [<<First:8, _/binary>> = Word | Tail])
    when First >= $0, First =< $9; First =:= $-
    -> % begins with a number or a minus
    Prog1 = emit_lit(Prog0, integer, binary_to_integer(Word)),
    compile2(Prog1, Tail);

compile2(Prog0 = #program{}, [Word | Tail]) ->
    %% Possibly a word, try resolve
    Prog1 = case prog_find_word(Prog0, Word) of
                not_found -> emit_base_word(Prog0, Word);
                Index -> emit_call(Prog0, Index)
            end,
    compile2(Prog1, Tail);
compile2(_Prog, [Other | _]) ->
    compile_error("E4 J1C: unknown op ~p", [Other]).

%% @doc Formats MFArity as a string, substitutes current module name if it was
%% set to "."
mfa_to_word(Prog0, <<"'.">>, Fn, Arity) ->
    mfa_to_word(Prog0, Prog0#program.mod, Fn, Arity);
mfa_to_word(_Prog0, Mod, Fn, Arity) ->
    iolist_to_binary(io_lib:format("~s:~s/~p", [Mod, Fn, Arity])).

%% @doc Adds a word to the dictionary, records current program length (program
%% counter position) as the word address.
-spec prog_add_word(program(), Word :: binary()) -> program().
prog_add_word(Prog0 = #program{pc=PC, dict=Dict}, Word) ->
    Dict1 = orddict:store(Word, PC, Dict),
    Prog0#program{dict=Dict1}.

%% @doc Adds a NIF name to the dictionary, does not register any other data
%% just marks the name as existing.
-spec prog_add_nif(program(), Word :: binary(), Index :: neg_integer())
                  -> program().
prog_add_nif(#program{}, Word, Index) when Index >= 0 ->
    compile_error("E4 J1C: Bad NIF index ~p in :NIF directive", [Index, Word]);
prog_add_nif(Prog0 = #program{dict_nif=Dict}, Word, Index) ->
    Dict1 = orddict:store(Word, Index, Dict),
    Prog0#program{dict_nif=Dict1}.


%% @doc Looks up a word in the dictionary, returns its address or 'not_found'
-spec prog_find_word(program(), forth_word()) -> integer() | not_found.
prog_find_word(#program{dict_nif=Nifs, dict=Dict}, Word) ->
    case orddict:find(Word, Nifs) of
        {ok, Index} -> Index; % nifs have negative indexes
        error ->
            case orddict:find(Word, Dict) of
                {ok, Index} -> Index;
                error -> not_found
            end
    end.

%% @doc Emits a CALL instruction with Index (signed) into the code.
%% Negative indices point to NIF functions
emit_call(Prog0 = #program{}, Index)
    when Index < 1 bsl ?J1OP_INDEX_WIDTH, Index > -(1 bsl ?J1OP_INDEX_WIDTH)
    ->
    emit(Prog0, <<?J1INSTR_CALL:?J1OP_CMD_WIDTH,
                  Index:?J1OP_INDEX_WIDTH/signed>>).

emit(Prog0 = #program{output=Out, pc=PC}, IOList) ->
    Prog0#program{output=[IOList | Out],
                  pc=PC + iolist_size(IOList)}.

-spec emit_alu(program(), alu() | [alu()]) -> program().
emit_alu(Prog = #program{}, ALUList) when is_list(ALUList) ->
    lists:foldl(fun(ALU, P) -> emit_alu(P, ALU) end, Prog, ALUList);
emit_alu(Prog = #program{}, ALU = #alu{ds=-1}) ->
    emit_alu(Prog, ALU#alu{ds=2});
emit_alu(Prog = #program{}, ALU = #alu{rs=-1}) ->
    emit_alu(Prog, ALU#alu{rs=2});
emit_alu(Prog = #program{}, #alu{op=Op0, tn=TN, rpc=RPC, tr=TR, nti=NTI,
                                 ds=Ds, rs=Rs}) ->
    %% Operation consists of 4 4-bit nibbles, tag goes first (3 bits) followed
    %% by the RPC flag, then goes operation, then combination of TN,TR,NTI
    %% flags and 1 unused bit, and then Ds/Rs 2 bits each
    %%
    %% 15 14 13 12 | 11 10 09 08 | 07 06 05 04 | 03 02 01 00 |
    %% InstrTag RPC| Op--------- | TN TR NTI ? | DS--- RS--- |
    %%
    Op1 = <<?J1INSTR_ALU:3, RPC:1,
            Op0:4,
            TN:1, TR:1, NTI:1, 0:1,
            Rs:2, Ds:2>>,
    emit(Prog, Op1).

emit_base_word(Prog0, <<"+">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T_PLUS_N, ds=-1});
emit_base_word(Prog0, <<"XOR">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T_XOR_N, ds=-1});
emit_base_word(Prog0, <<"AND">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T_AND_N, ds=-1});
emit_base_word(Prog0, <<"OR">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T_OR_N, ds=-1});

emit_base_word(Prog0, <<"INVERT">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_INVERT_T});

emit_base_word(Prog0, <<"=">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N_EQ_T, ds=-1});
emit_base_word(Prog0, <<"<">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N_LESS_T, ds=-1});
emit_base_word(Prog0, <<"U<">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N_UNSIGNED_LESS_T, ds=-1});

emit_base_word(Prog0, <<"SWAP">>) -> % swap data stack top 2 elements
    emit_alu(Prog0, #alu{op=?J1OP_N, tn=1});
emit_base_word(Prog0, <<"DUP">>) -> % clone data stack top
    emit_alu(Prog0, #alu{op=?J1OP_T, tn=1});
emit_base_word(Prog0, <<"DROP">>) -> % drop top on data stack
    emit_alu(Prog0, #alu{op=?J1OP_N, ds=-1});
emit_base_word(Prog0, <<"OVER">>) -> % clone second on data stack
    emit_alu(Prog0, #alu{op=?J1OP_N, tn=1, ds=1});
emit_base_word(Prog0, <<"NIP">>) -> % drops second on data stack
    emit_alu(Prog0, #alu{op=?J1OP_T, ds=-1});

emit_base_word(Prog0, <<">R">>) -> % place onto Rstack
    emit_alu(Prog0, #alu{op=?J1OP_N, tr=1, ds=-1, rs=1});
emit_base_word(Prog0, <<"R>">>) -> % take from Rstack
    emit_alu(Prog0, #alu{op=?J1OP_R, tn=1, ds=1, rs=-1});
emit_base_word(Prog0, <<"R@">>) -> % read Rstack top
    emit_alu(Prog0, #alu{op=?J1OP_R, tn=1, ds=1, rs=0});
emit_base_word(Prog0, <<"@">>) -> % read address
    emit_alu(Prog0, #alu{op=?J1OP_INDEX_T});
emit_base_word(Prog0, <<"!">>) -> % write address
    emit_alu(Prog0, [
        #alu{op=?J1OP_T, ds=-1},
        #alu{op=?J1OP_N, ds=-1}
    ]);

emit_base_word(Prog0, <<"DSP">>) -> % get stack depth
    emit_alu(Prog0, #alu{op=?J1OP_DEPTH, tn=1, ds=1});

emit_base_word(Prog0, <<"LSHIFT">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N_LSHIFT_T, ds=-1});
emit_base_word(Prog0, <<"RSHIFT">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N_RSHIFT_T, ds=-1});
emit_base_word(Prog0, <<"1-">>) -> % decrement stack top
    emit_alu(Prog0, #alu{op=?J1OP_T_MINUS_1});
emit_base_word(Prog0, <<"2r>">>) ->
    emit_alu(Prog0, [
        #alu{op=?J1OP_R, tn=1, ds=1, rs=-1},
        #alu{op=?J1OP_R, tn=1, ds=1, rs=-1},
        #alu{op=?J1OP_N, tn=1}
    ]);
emit_base_word(Prog0, <<"2>r">>) ->
    emit_alu(Prog0, [
        #alu{op=?J1OP_N, tn=1},
        #alu{op=?J1OP_N, tr=1, ds=-1, rs=1},
        #alu{op=?J1OP_N, tr=1, ds=-1, rs=1}
    ]);
emit_base_word(Prog0, <<"2r@">>) ->
    emit_alu(Prog0, [
        #alu{op=?J1OP_R, tn=1, ds=1, rs=-1},
        #alu{op=?J1OP_R, tn=1, ds=1, rs=-1},
        #alu{op=?J1OP_N, tn=1, ds=1},
        #alu{op=?J1OP_N, tn=1, ds=1},
        #alu{op=?J1OP_N, tr=1, ds=-1, rs=1},
        #alu{op=?J1OP_N, tr=1, ds=-1, rs=1},
        #alu{op=?J1OP_N, tn=1}
    ]);
emit_base_word(Prog0, <<"dup@">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_INDEX_T, tn=1, ds=1});
emit_base_word(Prog0, <<"dup>r">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T, tr=1, rs=1});
emit_base_word(Prog0, <<"2dupxor">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T_XOR_N, tn=1, ds=1});
emit_base_word(Prog0, <<"2dup=">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N_EQ_T, tn=1, ds=1});
emit_base_word(Prog0, <<"!nip">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T, nti=1, ds=-1});
emit_base_word(Prog0, <<"2dup!">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T, nti=1});
emit_base_word(Prog0, <<"up1">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T, ds=1});
emit_base_word(Prog0, <<"down1">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T, ds=-1});
emit_base_word(Prog0, <<"copy">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_N});

emit_base_word(Prog0, <<"NOOP">>) ->
    emit_alu(Prog0, #alu{op=?J1OP_T});
emit_base_word(_Prog0, Word) ->
    compile_error("E4 J1C: word is not defined: ~p", [Word]).

emit_lit(Prog0 = #program{atom_id=AId, atoms=Atoms}, atom, Word) ->
    Prog1 = Prog0#program{
        atom_id=AId+1,
        atoms=orddict:store(Word, AId, Atoms)
    },
    emit(Prog1, <<1:1, AId:?J1_LITERAL_BITS>>);
emit_lit(Prog0 = #program{}, integer, X) ->
    emit(Prog0, <<1:1, X:?J1_LITERAL_BITS/signed>>).
