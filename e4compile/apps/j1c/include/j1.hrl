%%
%% Definitions for J1 Forth virtual machine
%%
-ifndef(J1_HEADER).
-define(J1_HEADER, 1).

-define(J1BITS, 16).
-define(J1INSTR_WIDTH, 3).
-define(J1_LITERAL_BITS, (?J1BITS-1)). % how many bits remain after lit flag bit
-define(J1OP_INDEX_WIDTH, (?J1BITS-?J1INSTR_WIDTH)).
%% Bit values for the first nibble (Instruction type)
-define(J1LITERAL,          8). % top bit set for literals
-define(J1INSTR_JUMP,       0).
-define(J1INSTR_JUMP_COND,  1).
-define(J1INSTR_CALL,       2).
-define(J1INSTR_ALU,        3).
%% Bit values for the second nibble (Operation)
-define(J1OP_T,             0).
-define(J1OP_N,             1).
-define(J1OP_T_PLUS_N,      2).
-define(J1OP_T_AND_N,       3).
-define(J1OP_T_OR_N,        4).
-define(J1OP_T_XOR_N,       5).
-define(J1OP_INVERT_T,      6).
-define(J1OP_N_EQ_T,        7).
-define(J1OP_N_LESS_T,      8).
-define(J1OP_N_RSHIFT_T,    9).
-define(J1OP_T_MINUS_1,     10).
-define(J1OP_R,             11).
-define(J1OP_INDEX_T,       12).
-define(J1OP_N_LSHIFT_T,    13).
-define(J1OP_DEPTH,         14).
-define(J1OP_N_UNSIGNED_LESS_T, 15).

-type uint() :: non_neg_integer().
-type uint16() :: 0..65535.

-record(j1alu, {
    op = 0 :: 0..15,    % one of ?J1OP_* macros.
    tn = 0 :: 0..1,     % copy T (stack top) -> N (next after stack top)
    rpc = 0 :: 0..1,    % copy R (return stack top) to PC (program counter)
    tr = 0 :: 0..1,     % copy T (stack top) to R (return stack top)
    nti = 0 :: 0..1,    % indexed RAM access N->[T]
    ds = 0 :: -1..3,    % 2 bits, signed increment of data stack
    rs = 0 :: -1..3     % 2 bits, signed increment of return stack
}).
-type j1alu() :: #j1alu{}.

-record(j1label, {
    label :: uint()
}).
-type j1label() :: #j1label{}.

-record(j1jump, {
    condition = false :: false | z,
    label :: uint()
}).
-type j1jump() :: #j1jump{}.

%%-record(j1comment, {comment}).
%%-type j1comment() :: #j1comment{}.

%% Output from the pass1 forth
-type j1forth_code() :: [j1forth_code()] | binary() | j1label() | j1jump().

%% Output from the pass forth to bin
-type j1bin_code() :: [j1bin_code()] | binary().

%% Patch instruction is inserted into the code during compilation.
%% The second pass will find these patch instructions and replace them with
%% machine code instruction (op | patch_table[id]) where the patch_table is
%% updatedduring the same 1st pass but possibly later.
%%-record(j1patch, {
%%    id = 0 :: integer(),    % patch id to search later (equals PC value)
%%    op = 0 :: integer()     % instruction to insert without address part
%%}).
%%-type j1patch() :: #j1patch{}.

-type j1dict() :: orddict:orddict(binary() | tuple(), uint()).
-type j1nif_dict() :: orddict:orddict(binary(), uint()).

%% Output of the J1 forth compiler (J1C Pass 1)
-record(j1prog, {
    %labels = [] :: [{non_neg_integer(), non_neg_integer()}],
    label_id = 0 :: uint(), % counter to be used as label generator

    mod :: atom(),
    dict = orddict:new() :: j1dict(),
    dict_nif = orddict:new() :: j1nif_dict(),
    exports = [] :: [{binary(), uint()}],

    %% a literal value is the key, and the index in the lit table is the value
    lit_id = 0 :: integer(),
    literals = orddict:new() :: orddict:orddict(any(), uint()),

    vars = orddict:new() :: orddict:orddict(),
    modules = orddict:new() :: orddict:orddict(),

    condstack = [] :: [uint()],
    loopstack = [] :: [uint()],
    %% maps code address (where jump instruction is written) to another code
    %% address (jump destination) processed on the 2nd pass
%%    patch_table = orddict:new() :: orddict:orddict(integer(), integer()),

    atom_id = 0 :: uint(),
    atoms = orddict:new() :: orddict:orddict(binary(), uint()),

    output = [] :: j1forth_code()
}).
-type j1prog() :: #j1prog{}.

-endif. % J1_HEADER
