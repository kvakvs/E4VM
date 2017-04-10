
-module(e4c_pass_S).

%% API
-export([process/1]).

-include_lib("e4compiler/include/e4c.hrl").
-include_lib("e4assembler/include/e4asm.hrl").

process({_Module, _Exports, _Attrs, Forms, _}) ->
  E4Asm = process_forms(Forms, []),

  e4c:debug_write_term("e4c_pass_S.txt", E4Asm),
  io:format("~s~n~p~n", [color:redb("E4C PASS S"), E4Asm]),
  E4Asm.

%%%-----------------------------------------------------------------------------

process_forms([], Out) -> lists:reverse(Out);
process_forms([{function, Name, Arity, _, Code} | Forms], Out) ->
  io:format("FUNC ~s/~B~n~p~n", [Name, Arity, Code]),
  F = {e4fun, #{
    name => Name,
    arity => Arity,
    code => process_fun(Code, [])
  }},
  Out1 = [F | Out],
  process_forms(Forms, Out1);
process_forms([X | _Forms], _Out) ->
  ?COMPILE_ERROR("Unknown form ~p", [X]).

process_fun([], Out) -> lists:reverse(Out);
process_fun([{gc_bif, Name, Fail, _1, Args, Result} | Tail], Out) ->
  Cmd = {e4bif, #{
    name => Name,
    fail => Fail,
    args => Args,
    result => Result
  }},
  process_fun(Tail, [Cmd | Out]);
process_fun([{test, F, Fail, Args} | Tail], Out) ->
  Cmd = {e4bif, #{
    name => F,
    fail => Fail,
    args => Args,
    result => ignore
  }},
  process_fun(Tail, [Cmd | Out]);
process_fun([{func_info, _M, F, Arity} | Tail], Out) ->
  Cmd = {e4bif, #{
    name => function_clause,
    fail => ignore,
    args => [F, Arity],
    result => ignore
  }},
  process_fun(Tail, [Cmd | Out]);
process_fun([{call, Arity, Label} | Tail], Out) ->
  Cmd = {e4call, #{
    arity => Arity,
    target => Label,
    tailcall => false
  }},
  process_fun(Tail, [Cmd | Out]);
process_fun([{call_only, Arity, Label} | Tail], Out) ->
  Cmd = {e4call, #{
    arity => Arity,
    target => Label,
    tailcall => true
  }},
  process_fun(Tail, [Cmd | Out]);
process_fun([{deallocate, N}, return | Tail], Out) ->
  Cmd = {e4ret, #{dealloc => N}},
  process_fun(Tail, [Cmd | Out]);
process_fun([return | Tail], Out) ->
  Cmd = {e4ret, #{dealloc => 0}},
  process_fun(Tail, [Cmd | Out]);
process_fun([{line, _} | Tail], Out) -> process_fun(Tail, Out);
process_fun([Code | Tail], Out) -> process_fun(Tail, [Code | Out]).
