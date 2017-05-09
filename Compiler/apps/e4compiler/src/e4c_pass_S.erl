
-module(e4c_pass_S).

%% API
-export([process/1]).

-include_lib("e4compiler/include/e4c.hrl").
-include_lib("e4assembler/include/e4asm.hrl").

process({Module, Exports, Attrs, Forms, _}) ->
  Funs = process_forms(Forms, orddict:new()),

  %e4c:debug_write_term("e4c_pass_S.txt", Funs),
  %io:format("~s~n~p~n", [color:redb("E4C PASS S"), Funs]),

  #{'$' => e4mod,
    mod => Module,
    funs => Funs,
    exports => Exports,
    attrs => Attrs
  }.

%%%-----------------------------------------------------------------------------

process_forms([], Out) -> Out;
process_forms([{function, Name, Arity, _, Code} | Forms], Out) ->
%%  io:format("FUNC ~s/~B~n~p~n", [Name, Arity, Code]),
  F = #{
    '$' => e4fun,
    name => Name,
    arity => Arity,
    code => process_fun(Code, [])
  },
  Out1 = orddict:store({Name, Arity}, F, Out),
  process_forms(Forms, Out1);
process_forms([X | _Forms], _Out) ->
  ?COMPILE_ERROR("Unknown form ~p", [X]).

process_fun([], Out) -> lists:reverse(Out);
process_fun([{gc_bif, Name, Fail, Heap, Args, Result} | Tail], Out) ->
  Cmd = #{
    '$' => e4bif,
    args => Args,
    fail => Fail,
    gc => Heap,
    name => Name,
    result => Result
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{bif, Name, Fail, Args, Result} | Tail], Out) ->
  Cmd = #{
    '$' => e4bif,
    args => Args,
    fail => Fail,
    name => Name,
    result => Result
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{test, F, Fail, Args} | Tail], Out) ->
  Cmd = #{
    '$' => e4bif,
    args => Args,
    fail => Fail,
    name => F
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{call, Arity, Label} | Tail], Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    target => Label
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{call_last, Arity, Label, Dealloc} | Tail], Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    dealloc => Dealloc,
    tailcall => true,
    target => Label
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{call_only, Arity, Label} | Tail], Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    tailcall => true,
    target => Label
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{call_ext, Arity, {extfunc, M, F, Arity}} | Tail], Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    target => {extfunc, M, F, Arity}
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{call_ext_only, Arity, {extfunc, M, F, Arity}} | Tail], Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    tailcall => true,
    target => {extfunc, M, F, Arity}
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{call_ext_last, Arity, {extfunc, M, F, Arity}, De} | Tail], Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    dealloc => De,
    tailcall => true,
    target => {extfunc, M, F, Arity}
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{deallocate, N}, return | Tail], Out) ->
  Cmd = #{
    '$' => e4ret,
    dealloc => N
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([return | Tail], Out) ->
  Cmd = #{
    '$' => e4ret,
    dealloc => 0
  },
  process_fun(Tail, [Cmd | Out]);
process_fun([{line, _} | Tail], Out) -> process_fun(Tail, Out);
process_fun([Code | Tail], Out) -> process_fun(Tail, [Code | Out]).
