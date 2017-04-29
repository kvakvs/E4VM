%%% @doc BEAM Compact Term encoding, also used in Ericsson's BEAM files
%%% @end

-module(e4asm_cte).

%% API
-export([encode/2]).

-include_lib("e4compiler/include/e4c.hrl").
-include_lib("compiler/src/beam_opcodes.hrl").

%% @doc See docs/compact-encoding.rst
%% The logic is based on beam_asm:encode_arg in the compiler app
encode({x, X}, _Mod) -> beam_asm:encode(?tag_x, X);
encode({y, Y}, _Mod) -> beam_asm:encode(?tag_y, Y);
encode({f, F}, _Mod) -> beam_asm:encode(?tag_f, F);
encode(nil, _Mod) -> beam_asm:encode(?tag_a, 0);
encode([], _Mod) -> beam_asm:encode(?tag_a, 0);
encode({atom, Atom}, Mod = #{'$' := e4mod}) ->
  %% Assume atom already exists, will crash if it doesn't
  beam_asm:encode(?tag_a, atom_index(Atom, Mod) + 1);
encode({extfunc, Mod, Fun, Arity}, Mod0 = #{'$' := e4mod}) ->
  ImportIndex = import_index(Mod, Fun, Arity, Mod0),
  beam_asm:encode(?tag_u, ImportIndex);
encode({jumptab, JTab}, Mod0 = #{'$' := e4mod}) ->
  JTabIndex = jumptab_index(JTab, Mod0),
  beam_asm:encode(?tag_u, JTabIndex);
encode(X, _Mod) when is_integer(X) -> beam_asm:encode(?tag_u, X);
encode({integer, X}, _Mod) -> beam_asm:encode(?tag_u, X);
encode(X, _Mod) ->
  ?COMPILE_ERROR("do not know how to encode ~p", [X]).


atom_index(A, #{'$' := e4mod, atoms := Atoms}) when is_atom(A) ->
  case orddict:find(A, Atoms) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("atom must be registered: '~s'", [A])
  end.


jumptab_index(JTab, #{'$' := e4mod, jumptabs := JTabs}) ->
  case orddict:find(JTab, JTabs) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("jump table must be registered: '~s'", [JTab])
  end.


import_index(Mod, Fun, Arity, #{'$' := e4mod, imports := Imps})
  when is_atom(Mod), is_atom(Fun), is_integer(Arity) ->
    case orddict:find({Mod, Fun, Arity}, Imps) of
      {ok, X} -> X;
      error -> ?COMPILE_ERROR("extfunc must be registered: '~s:~s/~B'",
                              [Mod, Fun, Arity])
    end.
