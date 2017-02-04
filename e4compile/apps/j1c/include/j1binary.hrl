%%% Definitions and helpers for binary J1Forth output
%%%
-define(J1LARGE_OPCODE_BITS, 16).
%%-define(J1SMALL_OPCODE_BITS, 8).

-define(J1INSTR_WIDTH, 4).
-define(J1OPCODE_WIDTH, 4). % alu op size

-define(J1OP_INDEX_WIDTH, (?J1LARGE_OPCODE_BITS - ?J1INSTR_WIDTH)).

%% Bit values for the first nibble (Instruction type)
%% Literal tag with value type + bits for literal body
%%-define(J1_LITERAL_TAG_BITS, 4).
%%-define(J1_LITERAL_BITS, (?J1BITS - ?J1_LITERAL_TAG_BITS)).

-define(J1INSTR_JUMP,       0). % may be extended by 16 bit varint
-define(J1INSTR_JUMP_COND,  1). % may be extended by 16 bit varint
-define(J1INSTR_CALL,       2). % may be extended by 16 bit varint
-define(J1INSTR_ALU,        3). % always 16 bit
-define(J1INSTR_GETELEMENT, 4). % ( Tuple -- Tuple[Index] , index hardcoded )

-define(J1INSTR_LD,         5). % value is 1.. for args or ..0 for stack frame
-define(J1INSTR_ST,         6). % value is 1.. for args or ..0 for stack frame
%% TODO: 8-bit LD small, maybe separate LDARG with 4-bit index and LDLOC?
%% TODO: 8-bit ST small, same idea

-define(J1INSTR_ENTER,      7). % value is stack frame size
% 8
-define(J1INSTR_SMALL_POS,  9). % 4-bit positive int literal (8 bit instr)
-define(J1INSTR_LD_SMALL,   10). % 3bit signed
-define(J1INSTR_ST_SMALL,   11). % 3bit signed

-define(J1LITERAL,          12).
-define(J1LIT_ATOM,         (?J1LITERAL+0)).
-define(J1LIT_LITERAL,      (?J1LITERAL+1)).
-define(J1LIT_INTEGER,      (?J1LITERAL+2)).

-define(J1INSTR_SINGLE_BYTE, 15). % marks 1-byte opcode (those 16#F?)

-define(J1BYTE_INSTR_LEAVE,         16#F0). % Leaves stack frame and returns
-define(J1BYTE_INSTR_ERL_TAIL_CALL, 16#F1). % ( Args... Fun -- , jump to fun
-define(J1BYTE_INSTR_ERL_CALL,      16#F2). % ( Args... Fun -- Result , call )
-define(J1BYTE_INSTR_NIL,           16#F3).

%% 4-bit values (ALU Operation)
-define(J1ALU_T,             0).
-define(J1ALU_N,             1).
-define(J1ALU_T_PLUS_N,      2).
-define(J1ALU_T_AND_N,       3).
-define(J1ALU_T_OR_N,        4).
-define(J1ALU_T_XOR_N,       5).
-define(J1ALU_INVERT_T,      6).
-define(J1ALU_N_EQ_T,        7).
-define(J1ALU_N_LESS_T,      8).
-define(J1ALU_N_RSHIFT_T,    9).
-define(J1ALU_T_MINUS_1,     10).
-define(J1ALU_R,             11).
-define(J1ALU_INDEX_T,       12).
-define(J1ALU_N_LSHIFT_T,    13).
-define(J1ALU_DEPTH,         14).
-define(J1ALU_N_UNSIGNED_LESS_T, 15).

-define(IS_SINGLE_BYTE_OPCODE(ByteOp),
           ByteOp bsr ?J1INSTR_WIDTH == ?J1INSTR_SINGLE_BYTE
    orelse ByteOp bsr ?J1INSTR_WIDTH == ?J1INSTR_SMALL_POS
    orelse ByteOp bsr ?J1INSTR_WIDTH == ?J1INSTR_LD_SMALL
    orelse ByteOp bsr ?J1INSTR_WIDTH == ?J1INSTR_ST_SMALL).
