%%% @doc Integer encoding for any value with fixed bit header and user-defined
%%% bit width for the value. Returns an integer.
%%% @end

-module(e4asm_encode_int).

%% API
-export([encode/2]).

-include_lib("e4compiler/include/e4c.hrl").

%% TODO: Maybe use huffman tree for tags based on their frequency.
%% Use fixed width tag for now
-define(BIT_HEADER_SIZE,  3).
-define(BIT_TAG_X,        0).
-define(BIT_TAG_Y,        1).
-define(BIT_TAG_NIL,      2).

encode({x, X}, Bits) ->
  tagged_value(?BIT_TAG_X, X, Bits);

encode({y, Y}, Bits) ->
  tagged_value(?BIT_TAG_Y, Y, Bits);

encode({f, F}, Bits) ->  % labels cannot intermix with data, so just plain int
  <<F:Bits/unsigned-big>>;

%% Ignore is a special value used to mark an unused arg
encode(NIL, _Bits) when NIL =:= nil orelse
                        NIL =:= [] orelse
                        NIL =:= ignore ->
  <<?BIT_TAG_NIL:?BIT_HEADER_SIZE>>;

encode({atom, Atom}, Mod = #{'$' := e4mod}) ->
  %% Assume atom already exists, will crash if it doesn't
  beam_asm:encode(?tag_a, index_of(Atom, atoms, Mod) + 1);

encode({extfunc, Mod, Fun, Arity}, Mod0 = #{'$' := e4mod}) ->
  ImportIndex = index_of({Mod, Fun, Arity}, imports, Mod0),
  beam_asm:encode(?tag_u, ImportIndex);

encode({lambda, Label, NumFree}, Mod0 = #{'$' := e4mod}) ->
  LambdaIndex = index_of({Label, NumFree}, lambdas, Mod0),
  beam_asm:encode(?tag_u, LambdaIndex);

encode({jumptab, JTab}, Mod0 = #{'$' := e4mod}) ->
  JTabIndex = index_of(JTab, jumptabs, Mod0),
  beam_asm:encode(?tag_u, JTabIndex);

encode({literal, Lit}, Mod0 = #{'$' := e4mod}) ->
  LitIndex = index_of(Lit, literals, Mod0),
  beam_asm:encode(?tag_u, LitIndex);

encode(X, _Mod) when is_integer(X) -> beam_asm:encode(?tag_u, X);

encode({integer, X}, _Mod) -> beam_asm:encode(?tag_u, X);

encode(X, _Mod) ->
  ?COMPILE_ERROR("do not know how to encode ~p", [X]).


tagged_value(Tag, Value, Bits) ->
  <<Tag:?BIT_HEADER_SIZE, Value:Bits/unsigned-big>>.


index_of(Value, Key, Mod) ->
  case orddict:find(Value, maps:get(Key, Mod)) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("value '~p' must be registered in module ~s",
                            [Value, Key])
  end.
