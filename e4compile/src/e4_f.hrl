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

%% Forth module equivalent to Erlang module
%% Contains stack and variable scope from #cf_block's, stack is used to
%% calculate variable positions.
-record(f_mod, {
    scope=[]    :: [cf_var()],
    stack=[]    :: [cf_var()],
    output=[]   :: forth_code()
}).
