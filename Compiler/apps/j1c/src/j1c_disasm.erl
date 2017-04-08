-module(j1c_disasm).

%% API
-export([disasm/3]).

-include_lib("j1c/include/j1.hrl").
-include_lib("j1c/include/j1bytecode.hrl").

disasm(Prog, Pc, Bin) ->
    Out = disasm(Prog, Pc, iolist_to_binary(Bin), []),
    unicode:characters_to_binary(Out, utf8).

disasm(_Prog, _Pc, <<>>, Accum) -> lists:reverse(Accum);

disasm(Prog, Pc, <<?J1BYTE_INSTR_JUMP:8, F:24/big-signed>> = Op, Accum) ->
    Out = io_lib:format("~s ~s ~B~n", [format_pc_and_opcode(Pc, Op),
                                       color:green("JMP"), F]),
    disasm(Prog, Pc + 4, [Out | Accum]);
disasm(Prog, Pc, <<?J1BYTE_INSTR_JUMP_COND:8, F:24/big-signed>> = Op, Accum) ->
    Out = io_lib:format("~s ~s ~B~n", [format_pc_and_opcode(Pc, Op),
                                       color:green("JZ"), F]),
    disasm(Prog, Pc + 4, [Out | Accum]);

disasm(Prog, Pc, <<ByteOp:8, Tail/binary>>, Accum)
    when ?IS_SINGLE_BYTE_OPCODE(ByteOp) ->
    disasm(Prog, Pc + 1, Tail, [
        format_j1c_op8(Prog, ByteOp),
        format_pc_and_opcode(Pc, <<ByteOp:8>>) | Accum
    ]);
disasm(Prog, Pc, <<H:2/binary, Tail/binary>>, Accum) ->
    <<H1:16>> = H,
    disasm(Prog, Pc + 2, Tail, [
        format_j1c_op16(Prog, H),
        format_pc_and_opcode(Pc, <<H1:16>>) | Accum
    ]).

format_pc_and_opcode(Pc, <<Op:8>>) ->
    io_lib:format("[~4.16.0B]   ~2.16.0B: ", [Pc, Op]);
format_pc_and_opcode(Pc, <<Op:16>>) ->
    io_lib:format("[~4.16.0B] ~4.16.0B: ", [Pc, Op]);
format_pc_and_opcode(Pc, <<Op:32>>) ->
    io_lib:format("[~4.16.0B] ~8.16.0B: ", [Pc, Op]).

%%% ---------------------------------------------------------------------------

format_j1c_op8(_Prog, ?J1BYTE_INSTR_LEAVE) ->
    io_lib:format("~s; ~s~n", [color:green("LEAVE"), color:red("RET")]);

format_j1c_op8(_Prog, ?J1BYTE_INSTR_ERL_CALL) ->
    io_lib:format("erl-~s~n", [color:green("CALL")]);
format_j1c_op8(_Prog, ?J1BYTE_INSTR_ERL_TAIL_CALL) ->
    io_lib:format("erl-~s~n", [color:green("TAIL")]);
format_j1c_op8(_Prog, ?J1BYTE_INSTR_NIL) ->
    io_lib:format("~s []~n", [color:green("LIT")]);
format_j1c_op8(_Prog, ?J1BYTE_INSTR_VARINT) ->
    io_lib:format("~s~n", [color:green("VARINT")]);
format_j1c_op8(_Prog, ?J1BYTE_INSTR_VARINT_NEG) ->
    io_lib:format("~s~n", [color:green("VARINT-NEG")]);

format_j1c_op8(_Prog, X) when X bsr 4 == ?J1INSTR_SMALL_POS ->
    io_lib:format("~s i:~B~n", [color:blueb("LIT"), X band 15]);

format_j1c_op8(_Prog, X) when X bsr 4 == ?J1INSTR_LD_SMALL ->
    <<_:?J1INSTR_WIDTH, Index:?J1INSTR_WIDTH/signed>> = <<X:8>>,
    io_lib:format("~s ~s~n", [color:green("LD"), annotate_ldst(Index)]);
format_j1c_op8(_Prog, X) when X bsr 4 == ?J1INSTR_ST_SMALL ->
    <<_:?J1INSTR_WIDTH, Index:?J1INSTR_WIDTH/signed>> = <<X:8>>,
    io_lib:format("~s ~s~n", [color:green("ST"), annotate_ldst(Index)]);

format_j1c_op8(_Prog, Op) ->
    io_lib:format("?UNKNOWN-BYTEOP ~2.16.0B~n", [Op]).

%%% ---------------------------------------------------------------------------

format_j1c_op16(_Prog, <<LitTag:?J1INSTR_WIDTH,
                         Lit:?J1OP_INDEX_WIDTH/signed-big>>)
    when LitTag == ?J1LIT_INTEGER
         orelse LitTag == ?J1LIT_ATOM
         orelse LitTag == ?J1LIT_LITERAL
    ->
    LitType = fun(?J1LIT_ATOM) -> "a";
                 (?J1LIT_INTEGER) -> "i";
                 (?J1LIT_LITERAL) -> "L" end,
    io_lib:format("~s ~s:~B~n", [color:blueb("LIT"), LitType(LitTag), Lit]);

format_j1c_op16(_Prog, <<?J1INSTR_ENTER:?J1INSTR_WIDTH,
                         Size:?J1OP_INDEX_WIDTH/signed-big>>) ->
    io_lib:format("~s ~B~n", [color:green("ENTER"), Size]);

format_j1c_op16(_Prog, <<?J1INSTR_GETELEMENT:?J1INSTR_WIDTH,
                         Index:?J1OP_INDEX_WIDTH/signed-big>>) ->
    io_lib:format("~s ~B~n", [color:green("GET-ELEMENT"), Index]);

%% Format a normal opcode, 16bit
format_j1c_op16(Prog, <<?J1INSTR_CALL:?J1INSTR_WIDTH,
                        Addr:?J1OP_INDEX_WIDTH/signed-big>>) ->
    io_lib:format("~s ~s~n", [color:green("CALL"), whereis_addr(Prog, Addr)]);
%%format_j1c_op16(_Prog, <<?J1INSTR_JUMP:?J1INSTR_WIDTH,
%%                         Addr:?J1OP_INDEX_WIDTH/signed-big>>) ->
%%    io_lib:format("~s ~4.16.0B~n", [color:green("JMP"), Addr]);
%%format_j1c_op16(_Prog, <<?J1INSTR_JUMP_COND:?J1INSTR_WIDTH,
%%                         Addr:?J1OP_INDEX_WIDTH/signed-big>>) ->
%%    io_lib:format("~s ~4.16.0B~n", [color:green("JZ"), Addr]);
format_j1c_op16(_Prog, <<?J1INSTR_ALU:?J1INSTR_WIDTH,
                         Op:?J1OPCODE_WIDTH,
                         RPC:1,
                         TN:1, TR:1, NTI:1,
                         Ds:2, Rs:2>>) ->
    [format_j1c_alu(#j1alu{rpc = RPC, op = Op, tn = TN, tr = TR,
                           nti = NTI, ds = Ds, rs = Rs}),
     "\n"];

format_j1c_op16(_Prog, <<?J1INSTR_LD:?J1INSTR_WIDTH,
                         Index:?J1OP_INDEX_WIDTH/signed-big>>) ->
    io_lib:format("~s ~s~n", [color:green("LD"), annotate_ldst(Index)]);
format_j1c_op16(_Prog, <<?J1INSTR_ST:?J1INSTR_WIDTH,
                         Index:?J1OP_INDEX_WIDTH/signed-big>>) ->
    io_lib:format("~s ~s~n", [color:green("ST"), annotate_ldst(Index)]);

format_j1c_op16(_Prog, <<Cmd:?J1LARGE_OPCODE_BITS>>) ->
    io_lib:format("?UNKNOWN ~4.16.0B~n", [Cmd]).

annotate_ldst(I) when I =< 0 ->
    io_lib:format("loc[~p]", [-I]);
annotate_ldst(I) when I > 0 ->
    io_lib:format("arg[~p]", [I]).

%%% ---------------------------------------------------------------------------

format_j1c_alu(#j1alu{op = ?J1ALU_N, tn = 1}) -> color:red("SWAP");
format_j1c_alu(#j1alu{op = ?J1ALU_T, rpc = 1, rs = 2}) -> color:red("RET");
format_j1c_alu(#j1alu{rpc = RPC, op = Op, tn = TN, tr = TR, nti = NTI,
                      ds = Ds, rs = Rs}) ->
    FormatOffset =
    fun(_, 0) -> [];
        (Prefix, 1) -> Prefix ++ "++";
        (Prefix, 2) -> Prefix ++ "--"
    end,
    [
        io_lib:format("~s", [color:red("ALU." ++ j1_op(Op))]),
        case RPC of 0 -> []; _ -> " R->PC" end,
        case TN  of 0 -> []; _ -> " T->N" end,
        case TR  of 0 -> []; _ -> " T->R" end,
        case NTI of 0 -> []; _ -> " [T]" end,
        FormatOffset(" DS", Ds),
        FormatOffset(" RS", Rs)
    ].

%%%-----------------------------------------------------------------------------

j1_op(?J1ALU_T)                  -> "T";
j1_op(?J1ALU_N)                  -> "N";
j1_op(?J1ALU_T_PLUS_N)           -> "T+N";
j1_op(?J1ALU_T_AND_N)            -> "T&N";
j1_op(?J1ALU_T_OR_N)             -> "T|N";
j1_op(?J1ALU_T_XOR_N)            -> "T^N";
j1_op(?J1ALU_INVERT_T)           -> "~T";
j1_op(?J1ALU_N_EQ_T)             -> "N=T";
j1_op(?J1ALU_N_LESS_T)           -> "N<T";
j1_op(?J1ALU_N_RSHIFT_T)         -> "N>>T";
j1_op(?J1ALU_T_MINUS_1)          -> "T-1";
j1_op(?J1ALU_R)                  -> "R";
j1_op(?J1ALU_INDEX_T)            -> "[T]";
j1_op(?J1ALU_N_LSHIFT_T)         -> "N<<T";
j1_op(?J1ALU_DEPTH)              -> "DEPTH";
j1_op(?J1ALU_N_UNSIGNED_LESS_T)  -> "uN<T".

whereis_addr(#j1prog{dict = Words}, Addr) when Addr >= 0 ->
%%    io:format("~p~n", [Words]),
    case lists:keyfind(Addr, 2, Words) of
        {{Name1, _Arity}, _Addr1} ->
            io_lib:format("'~s'", [Name1]);
        {Name2, _Addr2} when is_binary(Name2) ->
            io_lib:format("'~s'", [Name2]);
        false -> "?"
    end;
whereis_addr(#j1prog{dict_nif = Nifs}, Addr) when Addr < 0 ->
    case lists:keyfind(Addr, 2, Nifs) of
        {Name, _} -> io_lib:format("~s '~s'", [color:blackb("NIF"), Name]);
        false -> "?"
    end.
