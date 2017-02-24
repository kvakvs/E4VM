-ifndef(ECPP_AST_HRL).
-define(ECPP_AST_HRL, 1).

-type cpp_code() :: list() | tuple().

-record(cpp_var, {
    anno = [] :: any(),
    name :: binary()
}).
-type cpp_var() :: #cpp_var{}.

%% Is a block subtype
-record(cpp_block, {
    before  :: cpp_code(),
    code    :: cpp_code(),
    aftr    :: cpp_code(),
    scope   :: [cpp_var()]
}).
-type cpp_block() :: #cpp_block{}.

%% IF block, is a block subtype
-record(cpp_if, {
    condition,
    true,
    false = [],
    scope = []
}).
-type cpp_if() :: #cpp_if{}.

-record(cpp_try, {}).
-type cpp_try() :: #cpp_try{}.

-record(cpp_catch, {
    clauses = []
}).
-type cpp_catch() :: #cpp_catch{}.

-record(cpp_assign, {
    lhs,
    rhs :: cpp_var()
}).
-type cpp_assign() :: #cpp_assign{}.

-record(cpp_comment, {
    comment = [] :: iolist()
}).
-type cpp_comment() :: #cpp_comment{}.

-record(cpp_call, {
    target,
    args = [],
    tailcall = default :: default | true
}).
-type cpp_call() :: #cpp_call{}.

%% &var reference
-record(cpp_ref, { ref }).
-type cpp_ref() :: #cpp_ref{}.

%% operator return X;
-record(cpp_return, { val }).
-type cpp_return() :: #cpp_return{}.

%% literal value X
-record(cpp_lit, { val }).
-type cpp_lit() :: #cpp_lit{}.

%%-record(cpp_atom, { val }).
%%-type cpp_atom() :: #cpp_atom{}.

%% function declaration, args and body
-record(cpp_fun, {
    type,
    name,
    args = [],
    code = []
}).
-type cpp_fun() :: #cpp_fun{}.

-record(cpp_module, {
    name,
    body
}).
-type cpp_module() :: #cpp_module{}.

-endif. % ECPP_AST_HRL
