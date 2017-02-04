%%
%% Definitions for J1 Forth virtual machine
%%
-ifndef(J1_HEADER).
-define(J1_HEADER, 1).

-define(J1BITS, 16).
-define(J1INSTR_WIDTH, 4).
-define(J1OPCODE_WIDTH, 4).

-define(J1OP_INDEX_WIDTH, (?J1BITS - ?J1INSTR_WIDTH)).

%% Bit values for the first nibble (Instruction type)
%% Literal tag with value type + bits for literal body
%%-define(J1_LITERAL_TAG_BITS, 4).
%%-define(J1_LITERAL_BITS, (?J1BITS - ?J1_LITERAL_TAG_BITS)).

-define(J1INSTR_JUMP,       0). % may be extended by 16 bit varint
-define(J1INSTR_JUMP_COND,  1). % may be extended by 16 bit varint
-define(J1INSTR_CALL,       2). % may be extended by 16 bit varint
-define(J1INSTR_ALU,        3). % always 16 bit
-define(J1INSTR_GETELEMENT, 4). % ( Tuple -- Tuple[Index] , index hardcoded )

-define(J1INSTR_LD,         6). % value is 1.. for args or ..0 for stack frame
-define(J1INSTR_ST,         7). % value is 1.. for args or ..0 for stack frame
-define(J1INSTR_ENTER,      8). % value is stack frame size

-define(J1LITERAL,          12).
-define(J1LIT_ATOM,         (?J1LITERAL+0)).
-define(J1LIT_LITERAL,      (?J1LITERAL+1)).
-define(J1LIT_INTEGER,      (?J1LITERAL+2)).

-define(J1INSTR_SINGLE_BYTE, 15). % marks 1-byte opcode (those 16#F?)

%-define(J1BYTE_INSTR_SMALL_INT,     16#F0). % Signed 3-bit integer literal
-define(J1BYTE_INSTR_LEAVE,         16#F1). % Leaves stack frame and returns
-define(J1BYTE_INSTR_ERL_TAIL_CALL, 16#F2). % ( Args... Fun -- , jump to fun
-define(J1BYTE_INSTR_ERL_CALL,      16#F3). % ( Args... Fun -- Result , call )

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

-type uint() :: non_neg_integer().
-type uint16() :: 0..65535.

-record(j1alu, {
    op = 0 :: 0..15,    % one of ?J1ALU_* macros.
    rpc = 0 :: 0..1,    % copy R (return stack top) to PC (program counter)
    tn = 0 :: 0..1,     % copy T (stack top) -> N (next after stack top)
    tr = 0 :: 0..1,     % copy T (stack top) to R (return stack top)
    nti = 0 :: 0..1,    % indexed RAM access N->[T]
    ds = 0 :: -1..3,    % 2 bits, signed increment of data stack ds=2 == -1
    rs = 0 :: -1..3     % 2 bits, signed increment of return stack rs=2 == -1
}).
-type j1alu() :: #j1alu{}.

-record(j1ld, { index :: integer() }).
-type j1ld() :: #j1ld{}.

-record(j1st, { index :: integer() }).
-type j1st() :: #j1st{}.

-record(j1getelement, { index :: uint16() }).
-type j1getelement() :: #j1getelement{}.

-record(j1enter, { size :: uint16() }).
-type j1enter() :: #j1enter{}.

-record(j1leave, {}).
-type j1leave() :: #j1leave{}.

-record(j1erl_call, { lit :: uint16() }).
-type j1erl_call() :: #j1erl_call{}.

-record(j1erl_tailcall, { lit :: uint16() }).
-type j1erl_tailcall() :: #j1erl_tailcall{}.

-record(j1label, {label :: uint() }).
-type j1label() :: #j1label{}.

-record(j1jump, {
    condition = false :: false | z,
    label :: uint()
}).
-type j1jump() :: #j1jump{}.

%%-record(j1comment, {comment}).
%%-type j1comment() :: #j1comment{}.

-record(j1lit, {
    id :: uint(),
    debug :: any()
}).
-type j1lit() :: #j1lit{}.

-record(j1atom, {
    id :: uint(),
    debug :: atom()
}).
-type j1atom() :: #j1atom{}.

%% Output from the pass1 forth
-type j1forth_code() :: [j1forth_code()] | binary() | j1label() | j1jump()
                        | j1atom() | j1lit().

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

-record(j1comment, {comment :: any()}).
-type j1comment() :: #j1comment{}.

%% Output of the J1 forth compiler (J1C Pass 1)
-record(j1prog, {
    %labels = [] :: [{non_neg_integer(), non_neg_integer()}],
    label_id = 0                :: uint(), % counter to be used as label generator

    mod                         :: atom(),
    dict = orddict:new()        :: j1dict(),
    dict_nif = orddict:new()    :: j1nif_dict(),
    exports = []                :: [{binary(), uint()}],

    %% a literal value is the key, and the index in the lit table is the value
    lit_id = 0                  :: integer(),
    literals = orddict:new()    :: orddict:orddict(any(), uint()),

    vars = orddict:new()        :: orddict:orddict(),
    modules = orddict:new()     :: orddict:orddict(),

    condstack = []              :: [uint()],
    loopstack = []              :: [uint()],
    %% maps code address (where jump instruction is written) to another code
    %% address (jump destination) processed on the 2nd pass
%%    patch_table = orddict:new() :: orddict:orddict(integer(), integer()),

    atom_id = 0                 :: uint(),
    atoms = orddict:new()       :: orddict:orddict(binary(), uint()),

    output = []                 :: j1forth_code()
}).
-type j1prog() :: #j1prog{}.

%% Output of J1C binary pass, contains copies of selected fields in #j1prog{}
-record(j1bin_prog, {
    labels = orddict:new()      :: orddict:orddict(uint(), uint()),
    lpatches = []               :: [integer()],

    literals = orddict:new()    :: orddict:orddict(any(), uint()),
    exports = []                :: [{binary(), uint()}],
    atoms = orddict:new()       :: orddict:orddict(binary(), uint()),

    dict = orddict:new()        :: j1dict(),
    dict_nif = orddict:new()    :: j1nif_dict(),
    pc = 0                      :: integer(),
    output = []                 :: j1bin_code()
}).
-type j1bin_prog() :: #j1bin_prog{}.

-endif. % J1_HEADER
