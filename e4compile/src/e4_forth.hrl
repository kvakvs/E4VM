%%%============================================================================
%%%     Core Forth definitions
%%%
%%%     Core Erlang very approximately maps to Core Forth with some
%%%     simplifications and generalizations on the way
%%%============================================================================
-ifndef(E4_FORTH_HRL).
-define(E4_FORTH_HRL, 1).

-record(f_var, {name :: atom()}).
-type f_var() :: #f_var{}.

%%-record(cf_funarity, {fn :: atom(), arity :: integer()}).

-record(f_mfa, {mod :: atom(), fn :: atom(), arity :: integer()}).
-type cf_mfa() :: #f_mfa{}.

-record(f_lit, {val :: any()}).
-type f_lit() :: #f_lit{}.

-record(f_comment, {comment :: any()}).
-type f_comment() :: #f_comment{}.

%% A code block with prefix and suffix
%% TODO: Have var scopes merged with this? Not sure how to find scope parent
-record(f_block, {
    before=[] :: intermediate_forth_code(),
    %% A copy of upper level scope + own variables
    scope=[] :: [f_var()],
    code=[] :: intermediate_forth_code(), % forth program output
    'after'=[] :: intermediate_forth_code()
}).
-type f_block() :: #f_block{}.

%% TODO: not sure if used in this pass
-record(f_mod_pass1, {
    module=undefined,
    atom_counter=0,
    atoms=dict:new(),
    lit_counter=0,
    literals=dict:new(),
    %% list of nested code blocks, containing code
    code=[] :: intermediate_forth_code()
%%    %% Compile-time state
%%    stack=[] :: [e4var()]
}).

%%-type cf_mod() :: #cf_mod{}.

%% Marking for variable operation, either read or write
-record(f_ld, {var :: f_var()}).
-type f_ld() :: #f_ld{}.

-record(f_st, {var :: f_var()}).
-type f_st() :: #f_st{}.

-record(f_stacktop, {}). % denotes the value currently on the stack top
-type f_stacktop() :: #f_stacktop{}.

-record(f_apply, {funobj, args=[]}).

%% introduce a new variable (produces no code, but allocates storage in a
%% further pass)
-record(f_decl_var, {var :: f_var()}).
-type f_decl_var() :: #f_decl_var{}.

%% introduce a new variable which is already on stack, and should not have
%% storage allocated.
-record(f_decl_arg, {var :: f_var()}).
-type f_decl_arg() :: #f_decl_arg{}.

%% introduce a new name to existing variable (produces no code)
-record(f_var_alias, {var :: f_var(), existing :: f_var()}).

% where is the allocated memory or if it was a given arg
-type f_var_alloc_type() :: stack_frame | pre_existing | alias.

%% Stores list of allocated vars and separate pre-existing vars
-record(f_var_storage, {
    stack_frame=[] :: [f_var()],
    args=[] :: [f_var()],
    aliases=orddict:new() % maps var to var
}).
-type f_var_storage() :: #f_var_storage{}.

%% Forth module equivalent to Erlang module
%% Contains stack and variable scope from #cf_block's, stack is used to
%% calculate variable positions.
-record(f_module, {
    scope=[]    :: [f_var()],
    alloc_vars=#f_var_storage{}, % allocated vars go here and args also go here
    output=[]   :: intermediate_forth_code(),
    cfgraph     :: digraph:graph()
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

-type intermediate_forth_op() ::
    forth_word() | f_comment() | f_lit() | f_block() | cf_mfa() | f_decl_var()
    | f_decl_arg() | f_ld() | f_st() | f_enter() | f_leave().
-type intermediate_forth_code() :: intermediate_forth_op()
    | [intermediate_forth_op()] | [intermediate_forth_code()].

-type forth_op() :: forth_word() | f_lit().
-type forth_code() :: forth_op() | [forth_op()] | [forth_code()].

-record(f_include, {filename="" :: string()}).
-type f_include() :: #f_include{}.

-endif. % E4_FORTH_HRL
