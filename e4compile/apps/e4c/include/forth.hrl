%%%============================================================================
%%%     Core Forth definitions
%%%
%%%     Core Erlang very approximately maps to Core Forth with some
%%%     simplifications and generalizations on the way
%%%============================================================================
-ifndef(E4_FORTH_HRL).
-define(E4_FORTH_HRL, 1).

-include_lib("e4c/include/kernel_erl.hrl").
-include_lib("ecpp/include/ecpp_ast.hrl").

%%-record(f_mfa, {mod :: atom(), fn :: atom(), arity :: integer()}).
%%-type cf_mfa() :: #f_mfa{}.

%%-record(k_literal, {val :: any()}).
%%-type k_literal() :: #k_literal{}.

-record(f_comment, {comment :: any()}).
-type f_comment() :: #f_comment{}.

%% A code block with prefix and suffix
%% TODO: Have var scopes merged with this? Not sure how to find scope parent
%%-record(cpp_block, {
%%    before=[] :: forth_ic(),
%%    %% A copy of upper level scope + own variables
%%    scope=[] :: [binary()],
%%    code=[] :: forth_ic(), % forth program output
%%    aftr=[] :: forth_ic()
%%}).
%%-type cpp_block() :: #cpp_block{}.

%% TODO: not sure if used in this pass
%%-record(f_mod_pass1, {
%%    module=undefined,
%%    atom_counter=0,
%%    atoms=dict:new(),
%%    lit_counter=0,
%%    literals=dict:new(),
%%    %% list of nested code blocks, containing code
%%    code=[] :: intermediate_forth_code()
%%%%    %% Compile-time state
%%%%    stack=[] :: [e4var()]
%%}).

%%-type cf_mod() :: #cf_mod{}.

%% Marking for variable operation, either read or write
-record(f_ld, {var :: k_var()}).
-type f_ld() :: #f_ld{}.
-record(f2_ld, {index :: integer()}).
-type f2_ld() :: #f2_ld{}.

-record(f_st, {var :: k_var()}).
-type f_st() :: #f_st{}.
-record(f2_st, {index :: integer()}).
-type f2_st() :: #f2_st{}.

-record(f_stacktop, {}). % denotes the value currently on the stack top
-type f_stacktop() :: #f_stacktop{}.

-record(f_apply, {funobj, args=[]}).

%% introduce a new variable (produces no code, but allocates storage in a
%% further pass)
-record(f_decl_var, {var :: k_var()}).
-type f_decl_var() :: #f_decl_var{}.

%% introduce a new variable which is already on stack, and should not have
%% storage allocated.
-record(f_decl_arg, {var :: k_var()}).
-type f_decl_arg() :: #f_decl_arg{}.

%% introduce a new name to existing variable (produces no code)
-record(f_var_alias, {var :: k_var(), existing :: k_var()}).

% where is the allocated memory or if it was a given arg
-type f_var_alloc_type() :: stack_frame | pre_existing | alias.

%% Stores list of allocated vars and separate pre-existing vars
-record(f_var_storage, {
    stack_frame=[] :: [k_var()],
    args=[] :: [k_var()],
    aliases=orddict:new() % maps var to var
}).
-type f_var_storage() :: #f_var_storage{}.

%% A block for pass 2 transformed from f_block of the pass 1
-record(f2_block, {
    code=[] :: forth_ic(), % forth program output
    alloc_vars=#f_var_storage{} :: f_var_storage()
}).

%% Forth module equivalent to Erlang module
%% Contains stack and variable scope from #cf_block's, stack is used to
%% calculate variable positions.
-record(f_module, {
    scope=[]    :: [k_var()],
    alloc_vars=#f_var_storage{}, % allocated vars go here and args also go here
    output=[]   :: forth_ic()
}).
-type f_module() :: #f_module{}.

-record(f_enter, {size=0 :: pos_integer()}).
-type f_enter() :: #f_enter{}.

-record(f_leave, {size=0 :: pos_integer()}).
-type f_leave() :: #f_leave{}.

%%
%% Grouping types
%%
-type forth_word() :: binary().
-define(IS_FORTH_WORD(X), is_binary(X)).

%% Intermediate code element
-type forth_ic_op() ::
    forth_word() | f_comment() | k_literal() | cpp_block() | k_remote()
    | f_decl_var() | f_decl_arg() | f_ld() | f_st() | f_enter() | f_leave().
%% Forth intermediate code in general as list of ops or list of itself
-type forth_ic() :: forth_ic_op() | [forth_ic_op()] | [forth_ic()].

-type forth_op() :: forth_word() | k_literal().
-type forth_code() :: forth_op() | [forth_op()] | [forth_code()].

-record(f_include, {filename="" :: string()}).
-type f_include() :: #f_include{}.

-record(f_export, {fn :: binary(), arity = 0 :: integer()}).
-type f_export() :: #f_export{}.

%% Literal codes, not really opcodes - processed at source load time
-define(F_LIT_FUNA,     <<"'FUNA">>).   % Fun/Arity local literal
-define(F_LIT_MFA,      <<"'MFA">>).    % M:F/Arity remote literal
-define(F_LIT_ATOM,     <<"'ATOM">>).   % literal atom
-define(F_LIT_NIL,      <<"'NIL">>).    % literal nil []

%% Words and opcodes used for various stuff
-define(F_IS_NIL,       <<".NIL?">>).   % compare top with literal nil []

-define(F_GETELEMENT,    <<"GET-ELEMENT">>). % ( Tuple Index -- Element )
-define(F_LD,            <<"LD">>).      % ALU opcode to load a variable or arg
-define(F_ST,            <<"ST">>).      % ALU opcode to store a variable or arg
-define(F_ENTER,         <<"ENTER">>).   % instr to create a stack frame
-define(F_LEAVE,         <<"LEAVE">>).   % instr to drop a stack frame
-define(F_RET,           <<"RET">>).     % drops stack frame and returns
-define(F_ERL_CALL,      <<"ERL-CALL">>).
-define(F_ERL_TAIL_CALL, <<"ERL-TAIL-CALL">>).

-define(TAG_LIT_MFARITY,    '+mfa').
-define(TAG_LIT_FUNARITY,   '+fa').

-endif. % E4_FORTH_HRL
