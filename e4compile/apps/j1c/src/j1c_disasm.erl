-module(j1c_disasm).

%% API
-export([disasm/2]).

-include_lib("j1c/include/j1.hrl").

disasm(Prog, Bin) ->
    disasm(Prog, 0, iolist_to_binary(Bin), []).

disasm(_Prog, _Pc, <<>>, Accum) -> lists:reverse(Accum);
disasm(Prog, Pc, <<H:2/binary, Tail/binary>> = Data, Accum) ->
    <<InstrTag:?J1INSTR_WIDTH, _:?J1OP_INDEX_WIDTH>> = H,
    case InstrTag of
        ?J1INSTR_ALU ->
            disasm(Prog, Pc + 1, Tail, [
                format_j1c_op(Prog, H),
                io_lib:format("~4.16.0B: ", [Pc]) | Accum
            ]);
        _ ->
            <<H2:4/binary, Tail2/binary>> = Data,
            disasm(Prog, Pc + 2, Tail2, [
                format_j1c_large(Prog, H2),
                io_lib:format("~4.16.0B: ", [Pc]) | Accum
            ])
    end.

%% TODO: Refactor this into varint arg
format_j1c_large(_Prog, <<Type:?J1_LITERAL_TAG_BITS,
                          Lit:?J1_LITERAL_BITS/big>>) when Type >= ?J1LITERAL ->
    io_lib:format("~s ~p~n", [color:blueb("LIT"), Lit]).

%% Format a normal opcode, 16bit
format_j1c_op(Prog, <<?J1INSTR_CALL:?J1INSTR_WIDTH,
                      Addr:(?J1OP_INDEX_WIDTH + 16)/signed>>) ->
    io_lib:format("~s ~s~n", [color:green("CALL"), whereis_addr(Prog, Addr)]);
format_j1c_op(_Prog, <<?J1INSTR_JUMP:?J1INSTR_WIDTH,
                       Addr:?J1OP_INDEX_WIDTH/signed>>) ->
    io_lib:format("~s ~4.16.0B~n", [color:green("JMP"), Addr]);
format_j1c_op(_Prog, <<?J1INSTR_JUMP_COND:?J1INSTR_WIDTH,
                       Addr:?J1OP_INDEX_WIDTH/signed>>) ->
    io_lib:format("~s ~4.16.0B~n", [color:green("JZ"), Addr]);
format_j1c_op(_Prog, <<?J1INSTR_ALU:3, RPC:1, Op:4, TN:1, TR:1, NTI:1,
                       _Unused:1, Ds:2, Rs:2>>) ->
    format_j1c_alu(RPC, Op, TN, TR, NTI, Ds, Rs);
format_j1c_op(_Prog, <<Cmd:?J1BITS>>) ->
    io_lib:format("?UNKNOWN ~4.16.0B~n", [Cmd]).

format_j1c_alu(RPC, Op, TN, TR, NTI, Ds, Rs) ->
    FormatOffset =
    fun(_, 0) -> [];
        (Prefix, 1) -> Prefix ++ "++";
        (Prefix, 2) -> Prefix ++ "--"
    end,
    [
        io_lib:format("~s", [color:red("ALU." ++ j1_op(Op))]),
        case RPC of 0 -> []; _ -> " RET" end,
        case TN of 0 -> []; _ -> " T->N" end,
        case TR of 0 -> []; _ -> " T->R" end,
        case NTI of 0 -> []; _ -> " [T]" end,
        FormatOffset(" DS", Ds),
        FormatOffset(" RS", Rs),
        "\n"
    ].

%%%-----------------------------------------------------------------------------

j1_op(?J1OP_T)                  -> "T";
j1_op(?J1OP_N)                  -> "N";
j1_op(?J1OP_T_PLUS_N)           -> "T+N";
j1_op(?J1OP_T_AND_N)            -> "T&N";
j1_op(?J1OP_T_OR_N)             -> "T|N";
j1_op(?J1OP_T_XOR_N)            -> "T^N";
j1_op(?J1OP_INVERT_T)           -> "~T";
j1_op(?J1OP_N_EQ_T)             -> "N==T";
j1_op(?J1OP_N_LESS_T)           -> "N<T";
j1_op(?J1OP_N_RSHIFT_T)         -> "N>>T";
j1_op(?J1OP_T_MINUS_1)          -> "T-1";
j1_op(?J1OP_R)                  -> "R";
j1_op(?J1OP_INDEX_T)            -> "[T]";
j1_op(?J1OP_N_LSHIFT_T)         -> "N<<T";
j1_op(?J1OP_DEPTH)              -> "DEPTH";
j1_op(?J1OP_N_UNSIGNED_LESS_T)  -> "UN<T".

whereis_addr(#j1bin_prog{dict = Words}, Addr) when Addr >= 0 ->
    case lists:keyfind(Addr, 2, Words) of
        {Name, _} -> io_lib:format("'~s'", [Name]);
        false -> "?"
    end;
whereis_addr(#j1bin_prog{dict_nif = Nifs}, Addr) when Addr < 0 ->
    case lists:keyfind(Addr, 2, Nifs) of
        {Name, _} -> io_lib:format("~s '~s'", [color:blackb("NIF"), Name]);
        false -> "?"
    end.
