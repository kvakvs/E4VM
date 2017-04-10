
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
  Out1 = [process_fun(Code, []) | Out],
  process_forms(Forms, Out1);
process_forms([X | Forms], _Out) ->
  ?COMPILE_ERROR("Unknown form ~p", [X]).

process_fun([], Out) -> lists:reverse(Out);
process_fun([Code | Tail], Out) ->
  process_fun(Tail, [Code | Out]);
process_fun([X | _Code], _Out) ->
  ?COMPILE_ERROR("Unknown instruction ~p", [X]).
