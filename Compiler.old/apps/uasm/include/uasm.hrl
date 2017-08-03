-ifndef(E4_ASSEMBLY_INC).
-define(E4_ASSEMBLY_INC, 1).

-type label() :: {f, integer()}.
-type xreg() :: {x, integer()}.

-type e4mod() :: #{
  '$' => module,
  funs => orddict:orddict()
}.

-type e4fun() :: #{
  '$' => e4fun,
  name => atom(),
  arity => integer(),
  code => list()
}.

-type e4bif() :: #{
  '$' => e4bif,
  name => atom(),
  fail => label() | ignore, % default ignore
  args => list(),
  result => xreg() | ignore, % default ignore
  gc => integer() % heap size to ensure, default 0
}.

-type e4call() :: #{
  '$' => e4call,
  target => label(),
  arity => integer(),
  tailcall => boolean(), % default false
  dealloc => integer() % default 0
}.

-type e4ret() :: #{
  '$' => e4ret,
  dealloc => integer() % default 0
}.

-endif.
