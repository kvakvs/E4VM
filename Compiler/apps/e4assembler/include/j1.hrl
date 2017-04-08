%%
%% Definitions for J1 Forth virtual machine
%%
-ifndef(J1_HEADER).
-define(J1_HEADER, 1).

-type uint() :: non_neg_integer().
-type uint16() :: 0..65535.

-record(j1alu, {
    op = 0 :: 0..15,    % one of ?J1ALU_* macros, ALU operation
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
    debug :: atom() | binary()
}).
-type j1atom() :: #j1atom{}.

%% Output from the pass1 forth
-type j1forth_code() :: [j1forth_code()] | binary() | integer()
    | j1label() | j1jump()
    | j1atom() | j1lit() | j1erl_call() | j1erl_tailcall()
    | j1ld() | j1st() | j1getelement() | j1enter() | j1leave() | j1comment().

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

    output = []                 :: j1forth_code(),

    %%
    %% Compile to bytecode stuff
    %%

    labels = orddict:new()      :: orddict:orddict(uint(), uint()),
    lpatches = []               :: [integer()],
    pc = 0                      :: integer()
}).
-type j1prog() :: #j1prog{}.

%% Output of J1C binary pass, contains copies of selected fields in #j1prog{}
%%-record(j1prog, {
%%    labels = orddict:new()      :: orddict:orddict(uint(), uint()),
%%    lpatches = []               :: [integer()],
%%
%%    literals = orddict:new()    :: orddict:orddict(any(), uint()),
%%    exports = []                :: [{binary(), uint()}],
%%    atoms = orddict:new()       :: orddict:orddict(binary(), uint()),
%%
%%    dict = orddict:new()        :: j1dict(),
%%    dict_nif = orddict:new()    :: j1nif_dict(),
%%    pc = 0                      :: integer(),
%%    output = []                 :: j1bin_code()
%%}).
%%-type j1prog() :: #j1prog{}.

%% Represents a processed block of Forth compiled to a list of binary opcodes.
%% Originally offsets for jumps in it are label ids, but Link pass rewrites
%% them to be relative addresses and expands opcodes if needed
%%-record(j1compiled, {
%%    bin :: j1bin_code()
%%}).
%%-type j1compiled() :: #j1compiled{}.

-endif. % J1_HEADER
