%%% @doc Bytecode abstraction layer
%%% @end

-module(e4asm_bytecode).

%% API
-export([
      alu/1,
      call_signed/1,
      enter/1,
      erl_call/0,
      erl_tail_call/0,
      get_element/1,
      jump_signed/1,
      jump_z_signed/1,
      leave/0,
      literal_arbitrary/1,
      literal_atom/1,
      literal_integer/1,
      literal_nil/0,
      load/1,
      signed_value_fits/2,
      store/1,
      unsigned_value_fits/2
]).

-include_lib("e4compiler/include/e4c.hrl").
-include_lib("e4assembler/include/j1.hrl").
-include_lib("e4assembler/include/j1bytecode.hrl").

signed_value_fits(Val, Bits) ->
    <<OutVal:Bits/signed-big>> = <<Val:Bits/signed-big>>,
    Val =:= OutVal.

unsigned_value_fits(Val, Bits) ->
    <<OutVal:Bits/unsigned-big>> = <<Val:Bits/unsigned-big>>,
    Val =:= OutVal.

literal_nil() -> <<?J1INSTR_SINGLE_BYTE:?J1INSTR_WIDTH,
                   ?J1BYTE_INSTR_NIL:?J1INSTR_WIDTH>>.

lit_varint(Val) when Val >= 0 ->
    <<?J1BYTE_INSTR_VARINT:8, (e4c:varint(Val))/binary>>;
lit_varint(Val) when Val < 0 ->
    <<?J1BYTE_INSTR_VARINT_NEG:8, (e4c:varint(Val))/binary>>.

lit(Type, Val) ->
    ?ASSERT(unsigned_value_fits(Type, ?J1INSTR_WIDTH),
            "Literal type won't fit into designated bits"),
    ?ASSERT(unsigned_value_fits(Val, ?J1OP_INDEX_WIDTH),
            "Literal value won't fit into designated bits"),
    <<Type:?J1INSTR_WIDTH, Val:?J1OP_INDEX_WIDTH/big-unsigned>>.

lit_signed(Type, Val) ->
    ?ASSERT(unsigned_value_fits(Type, ?J1INSTR_WIDTH),
            "Literal type won't fit into designated bits"),
    ?ASSERT(signed_value_fits(Val, ?J1OP_INDEX_WIDTH),
            "Literal value won't fit into designated bits"),
    <<Type:?J1INSTR_WIDTH, Val:?J1OP_INDEX_WIDTH/big-signed>>.

literal_atom(Index)  -> lit(?J1LIT_ATOM, Index).
literal_arbitrary(Lit) -> lit(?J1LIT_LITERAL, Lit).

literal_integer(Int) ->
    case e4asm_bytecode:unsigned_value_fits(Int, ?J1INSTR_WIDTH) of
        true ->
            <<?J1INSTR_SMALL_POS:?J1INSTR_WIDTH,
              Int:?J1INSTR_WIDTH/big-unsigned>>;
        false ->
            lit_signed(?J1LIT_INTEGER, Int)
    end.

alu(#j1alu{op = Op0, rpc = RPC,
           tn = TN, tr = TR, nti = NTI,
           ds = Ds, rs = Rs}) when Ds >= 0, Rs >= 0 ->
    %% 15 14 13 12 | 11 10 09 08 |  07 06 05  04 | 03 02 01 00 |
    %% InstrTag--- | Op--------- | RPC TN TR NTI | DS--- RS--- |l
    %%
    ?ASSERT(unsigned_value_fits(Op0, ?J1OPCODE_WIDTH),
            "Opcode for ALU does not fit into designated bits"),
    ?ASSERT(unsigned_value_fits(Rs, 2),
            "RS for ALU does not fit into designated bits"),
    ?ASSERT(unsigned_value_fits(Ds, 2),
            "DS for ALU does not fit into designated bits"),
    <<?J1INSTR_ALU:?J1INSTR_WIDTH/big-unsigned,
      Op0:?J1OPCODE_WIDTH/big-unsigned,
      RPC:1,
      TN:1,
      TR:1,
      NTI:1,
      Rs:2/big-unsigned,
      Ds:2/big-unsigned>>.

call_signed(Index) ->
    ?ASSERT(signed_value_fits(Index, ?J1OP_INDEX_WIDTH),
            "Index for a CALL instruction is too large"),
    <<?J1INSTR_CALL:?J1INSTR_WIDTH,
      Index:?J1OP_INDEX_WIDTH/big-signed>>.

jump_signed(F) ->
    %% Later (load time?) labels are replaced with addresses
    <<?J1BYTE_INSTR_JUMP:8, F:24/big-signed>>.

jump_z_signed(F) ->
    %% Later (load time?) labels are replaced with addresses
    <<?J1BYTE_INSTR_JUMP_COND:8, F:24/big-signed>>.

erl_tail_call() ->
    <<?J1INSTR_SINGLE_BYTE:?J1INSTR_WIDTH,
      ?J1BYTE_INSTR_ERL_TAIL_CALL:?J1INSTR_WIDTH>>.

erl_call() ->
    <<?J1INSTR_SINGLE_BYTE:?J1INSTR_WIDTH,
      ?J1BYTE_INSTR_ERL_CALL:?J1INSTR_WIDTH>>.

leave() ->
    <<?J1INSTR_SINGLE_BYTE:?J1INSTR_WIDTH,
      ?J1BYTE_INSTR_LEAVE:?J1INSTR_WIDTH>>.

enter(Size) ->
    ?ASSERT(unsigned_value_fits(Size, ?J1OP_INDEX_WIDTH),
            "Size for an ENTER instruction is too large"),
    <<?J1INSTR_ENTER:?J1INSTR_WIDTH,
      Size:?J1OP_INDEX_WIDTH/big-unsigned>>.

get_element(Index) ->
    ?ASSERT(e4asm_bytecode:unsigned_value_fits(Index, ?J1OP_INDEX_WIDTH),
            "GET-ELEMENT opcode index is too large"),
    <<?J1INSTR_GETELEMENT:?J1INSTR_WIDTH,
      Index:?J1OP_INDEX_WIDTH/big-unsigned>>.

store(Index) ->
    ?ASSERT(e4asm_bytecode:signed_value_fits(Index, ?J1OP_INDEX_WIDTH),
            "ST opcode index is too large"),
    case signed_value_fits(Index, ?J1INSTR_WIDTH) of
        true ->
            <<?J1INSTR_ST_SMALL:?J1INSTR_WIDTH,
              Index:?J1INSTR_WIDTH/signed>>;
        false ->
            <<?J1INSTR_ST:?J1INSTR_WIDTH,
              Index:?J1OP_INDEX_WIDTH/big-signed>>
    end.

load(Index) ->
    ?ASSERT(e4asm_bytecode:signed_value_fits(Index, ?J1OP_INDEX_WIDTH),
            "LD opcode index is too large"),
    case signed_value_fits(Index, ?J1INSTR_WIDTH) of
        true ->
            <<?J1INSTR_LD_SMALL:?J1INSTR_WIDTH,
              Index:?J1INSTR_WIDTH/signed>>;
        false ->
            <<?J1INSTR_LD:?J1INSTR_WIDTH,
              Index:?J1OP_INDEX_WIDTH/big-signed>>
    end.
