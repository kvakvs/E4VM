-define(Lazy(EmitCode), fun(St) -> e4_c2f:emit(St, EmitCode) end).

-record(e4scope, {vars=[] :: [e4var()]}).
-record(e4var, {name :: atom()}).
-record(e4funarity, {fn :: atom(), arity :: integer()}).
-record(e4mfa, {mod :: atom(), fn :: atom(), arity :: integer()}).
-record(e4lit, {val :: any()}).
-record(e4comment, {txt :: any()}).

-type e4scope() :: #e4scope{}.
-type e4var() :: #e4var{}.
-type e4lit() :: #e4lit{}.
-type e4comment() :: #e4comment{}.
