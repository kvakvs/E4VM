-ifndef(E4_ASSEMBLY_INC).
-define(E4_ASSEMBLY_INC, 1).

-type label() :: {f, integer()}.
-type xreg() :: {x, integer()}.

-type e4fun() :: {e4fun, #{
  name => atom(),
  arity => integer(),
  code => list()
}}.

-type e4bif() :: {e4bif, #{
  name => atom(),
  fail => label() | ignore,
  args => list(),
  result => xreg() | ignore
}}.

-type e4call() :: {e4call, #{
  target => label(),
  arity => integer(),
  tailcall => boolean()
}}.

-type e4ret() :: {e4ret, #{
  dealloc => integer()
}}.

-endif.
