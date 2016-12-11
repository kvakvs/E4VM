%%-define(Lazy(EmitCode), fun(St) -> e4_c2f:emit(St, EmitCode) end).

-record(e4var, {name :: atom()}).
-type e4var() :: #e4var{}.

-record(e4funarity, {fn :: atom(), arity :: integer()}).

-record(e4mfa, {mod :: atom(), fn :: atom(), arity :: integer()}).

-record(e4lit, {val :: any()}).
-type e4lit() :: #e4lit{}.

-record(e4comment, {comment :: any()}).
-type e4comment() :: #e4comment{}.

%%-record(e4varscope, {
%%    vars=[] :: [e4var()]
%%}).
%%-type e4varscope() :: #e4varscope{}.

%% A code block with prefix and suffix
%% TODO: Have var scopes merged with this? Not sure how to find scope parent
-record(e4block, {
    code=[] :: forth_code(),                % forth program output
    before=[] :: forth_code(),
    'after'=[] :: forth_code(),
    %% A copy of upper level scope + own variables
    scope=[] :: [e4var()]
}).
-type e4block() :: #e4block{}.

-type forth_word() :: atom().
-type forth_op() :: forth_word() | e4comment() | e4lit() | e4block().
-type forth_code() :: [forth_op() | forth_code()].

-record(e4module, {
    module=undefined,
    atom_counter=0,
    atoms=dict:new(),
    lit_counter=0,
    literals=dict:new(),
    %% list of nested code blocks, containing code
    code=[] :: forth_code()
%%    %% Compile-time state
%%    stack=[] :: [e4var()]
}).

-type e4module() :: #e4module{}.

-record(e4retrieve_op, {var :: e4var()}).
-record(e4store_op, {var :: e4var()}).
