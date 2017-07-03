%%% @doc BEAM Compact Term encoding, also used in Ericsson's BEAM files
%%% @end

-module(uasm_encode_cte).

%% API
-export([encode/2]).

-include_lib("uerlc/include/uerlc.hrl").
-include_lib("compiler/src/beam_opcodes.hrl").

%% @doc See docs/compact-encoding.rst
%% The logic is based on beam_asm:encode_arg in the compiler app
encode({x, X}, _Mod) -> beam_asm:encode(?tag_x, X);

encode({y, Y}, _Mod) -> beam_asm:encode(?tag_y, Y);

encode({f, F}, _Mod) -> beam_asm:encode(?tag_f, F);

encode(nil, Mod) -> encode([], Mod);

% Special value used in BIF opcode and others, to mark unused arg
encode(ignore, Mod) -> encode([], Mod);

encode([], _Mod) -> beam_asm:encode(?tag_a, 0);

encode({atom, Atom}, Mod = #{'$' := module}) ->
  %% Assume atom already exists, will crash if it doesn't
  beam_asm:encode(?tag_a, index_of(Atom, atoms, Mod) + 1);

encode({extfunc, Mod, Fun, Arity}, Mod0 = #{'$' := module}) ->
  ImportIndex = index_of({Mod, Fun, Arity}, imports, Mod0),
  beam_asm:encode(?tag_u, ImportIndex);

encode({lambda, Label, NumFree}, Mod0 = #{'$' := module}) ->
  LambdaIndex = index_of({Label, NumFree}, lambdas, Mod0),
  beam_asm:encode(?tag_u, LambdaIndex);

encode({jumptab, JTab}, Mod0 = #{'$' := module}) ->
  JTabIndex = index_of(JTab, jumptabs, Mod0),
  beam_asm:encode(?tag_u, JTabIndex);

encode({literal, Lit}, Mod0 = #{'$' := module}) ->
  LitIndex = index_of(Lit, literals, Mod0),
  beam_asm:encode(?tag_u, LitIndex);

encode(X, _Mod) when is_integer(X) -> beam_asm:encode(?tag_u, X);

encode({integer, X}, _Mod) -> beam_asm:encode(?tag_u, X);

encode(X, _Mod) ->
  ?COMPILE_ERROR("do not know how to encode ~p", [X]).


index_of(Value, Key, Mod) ->
  case orddict:find(Value, maps:get(Key, Mod)) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("value '~p' must be registered in module ~s",
                            [Value, Key])
  end.
