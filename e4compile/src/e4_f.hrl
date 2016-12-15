%%%============================================================================
%%%     Forth definitions
%%%
%%%     Core Forth compiles to Erl-Forth (e4) program consisting of words
%%%     and literals
%%%============================================================================

%% Forth code definitions
-include("e4_cf.hrl").

-type forth_op() :: atom() | cf_lit() | cf_comment().
-type forth_code() :: forth_op() | [forth_op()] | [forth_code()].

% where is the allocated memory or if it was a given arg
-type f_var_alloc_type() :: stack_frame | pre_existing | alias.

%% Stores list of allocated vars and separate pre-existing vars
-record(f_var_storage, {
    stack_frame=[] :: [cf_var()],
    args=[] :: [cf_var()],
    aliases=orddict:new() % maps var to var
}).
-type f_var_storage() :: #f_var_storage{}.

%% Forth module equivalent to Erlang module
%% Contains stack and variable scope from #cf_block's, stack is used to
%% calculate variable positions.
-record(f_module, {
    scope=[]    :: [cf_var()],
    alloc_vars=#f_var_storage{}, % allocated vars go here and args also go here
    output=[]   :: forth_code(),
    cfgraph     :: digraph:graph()
}).
-type f_module() :: #f_module{}.
