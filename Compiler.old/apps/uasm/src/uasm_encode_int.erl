%%% @doc Integer encoding for any value with fixed bit header and user-defined
%%% bit width for the value. Returns an integer.
%%% @end

-module(uasm_encode_int).

%% API
-export([
  encode/2,
  encode/3,
  varlength_unsigned/1
]).

-include_lib("uerlc/include/uerlc.hrl").

%% TODO: Maybe use huffman tree for tags based on their frequency.
%% Use fixed width tag for now
-define(BIT_HEADER_SIZE,  3).
-define(BIT_TAG_X,        0).
-define(BIT_TAG_Y,        1).
-define(BIT_TAG_NIL,      2).
-define(BIT_TAG_ATOM,     3).
-define(BIT_TAG_IMPORT,   4).
-define(BIT_TAG_LAMBDA,   5).
-define(BIT_TAG_LITERAL,  6).
-define(BIT_TAG_INTEGER,  7).

encode(Val, Bits) when is_integer(Bits); Bits =:= auto_bits ->
  encode(Val, Bits, #{}).


encode({x, X}, auto_bits, _Mod) ->
  ?ASSERT(X < ?LIMIT_MAXARITY, "regx value is too big"),
  uasm_stats:count_value(regx),
  tagged_value(?BIT_TAG_X, X, 8);

encode({y, Y}, auto_bits, Mod) ->
  ?ASSERT(Y < ?LIMIT_MAX_REGY, "regy value is too big"),
  uasm_stats:count_value(regy),
  tagged_value(?BIT_TAG_Y, Y, 8);

encode({f, F}, Bits, _Mod) ->
  uasm_util:assert_unsigned_fits("for a label", F, Bits),
  uasm_stats:count_value(label),
  %% labels cannot intermix with data, so just plain int
  varlength_unsigned(F);

%% Ignore is a special value used to mark an unused arg
encode(NIL, _Bits, _Mod) when NIL =:= nil orelse
                              NIL =:= [] orelse
                              NIL =:= ignore ->
  uasm_stats:count_value(nil_ignore),
  <<?BIT_TAG_NIL:?BIT_HEADER_SIZE>>;

encode({atom, Atom}, _Bits, Mod = #{'$' := module}) ->
  %% Assume atom already exists, will crash if it doesn't
  uasm_stats:count_value(atom),
  AtomIndex = index_of(Atom, atoms, Mod) + 1,
  tagged_varlength_unsigned(?BIT_TAG_ATOM, AtomIndex);

encode({extfunc, Mod, Fun, Arity}, _Bits, Mod0 = #{'$' := module}) ->
  uasm_stats:count_value(import),
  ImportIndex = index_of({Mod, Fun, Arity}, imports, Mod0),
  tagged_varlength_unsigned(?BIT_TAG_IMPORT, ImportIndex);

encode({lambda, Label, NumFree}, _Bits, Mod0 = #{'$' := module}) ->
  uasm_stats:count_value(lambda),
  LambdaIndex = index_of({Label, NumFree}, lambdas, Mod0),
  tagged_varlength_unsigned(?BIT_TAG_LAMBDA, LambdaIndex);

encode({jumptab, JTab}, _Bits, Mod0 = #{'$' := module}) ->
  uasm_stats:count_value(jumptab),
  JTabIndex = index_of(JTab, jumptabs, Mod0),
  varlength_unsigned(JTabIndex);

encode({literal, Lit}, _Bits, Mod0 = #{'$' := module}) ->
  uasm_stats:count_value(literal),
  LitIndex = index_of(Lit, literals, Mod0),
  tagged_varlength_unsigned(?BIT_TAG_LITERAL, LitIndex);

encode(X, _Bits, _Mod) when is_integer(X) ->
  tagged_varlength_signed(?BIT_TAG_INTEGER, X);

encode({integer, X}, _Bits, _Mod) ->
  tagged_varlength_signed(?BIT_TAG_INTEGER, X);

encode(X, Bits, _Mod) ->
  ?COMPILE_ERROR("do not know how to encode ~p in '~p' bits", [X, Bits]).


index_of(Value, Key, Mod) ->
  case orddict:find(Value, maps:get(Key, Mod)) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("value '~p' must be registered in module ~s",
                            [Value, Key])
  end.


tagged_value(Tag, Value, Bits) ->
  <<Tag:?BIT_HEADER_SIZE, Value:Bits/unsigned-big>>.

-define(VARLENGTH_LENGTH_0, 4).
-define(VARLENGTH_LIMIT_0, (1 bsl ?VARLENGTH_LENGTH_0)).

-define(VARLENGTH_LENGTH_1, 8).
-define(VARLENGTH_LIMIT_1, (1 bsl ?VARLENGTH_LENGTH_1)).

-define(VARLENGTH_LENGTH_2, 16).
-define(VARLENGTH_LIMIT_2, (1 bsl ?VARLENGTH_LENGTH_2)).

-define(VARLENGTH_LENGTH_3, 32).
-define(VARLENGTH_LIMIT_3, (1 bsl ?VARLENGTH_LENGTH_3)).

%% @doc Compact unsigned integer encoding where 2 bits define byte size for the
%% following unsigned value.
tagged_varlength_unsigned(Tag, Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_0 ->
    <<Tag:?BIT_HEADER_SIZE, 0:2, Val:?VARLENGTH_LENGTH_0/big-unsigned>>;
tagged_varlength_unsigned(Tag, Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_1 ->
    <<Tag:?BIT_HEADER_SIZE, 1:2, Val:?VARLENGTH_LENGTH_1/big-unsigned>>;
tagged_varlength_unsigned(Tag, Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_2 ->
    <<Tag:?BIT_HEADER_SIZE, 2:2, Val:?VARLENGTH_LENGTH_2/big-unsigned>>;
tagged_varlength_unsigned(Tag, Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_3 ->
    <<Tag:?BIT_HEADER_SIZE, 3:2, Val:?VARLENGTH_LENGTH_3/big-unsigned>>.


tagged_varlength_signed(Tag, Val)
  when Val >= -?VARLENGTH_LIMIT_0 div 2+1, Val =< ?VARLENGTH_LIMIT_0 div 2 ->
    <<Tag:?BIT_HEADER_SIZE, 0:2, Val:?VARLENGTH_LENGTH_0/big-signed>>;
tagged_varlength_signed(Tag, Val)
  when Val >= -?VARLENGTH_LIMIT_1 div 2+1, Val =< ?VARLENGTH_LIMIT_1 div 2 ->
    <<Tag:?BIT_HEADER_SIZE, 1:2, Val:?VARLENGTH_LENGTH_1/big-signed>>;
tagged_varlength_signed(Tag, Val)
  when Val >= -?VARLENGTH_LIMIT_2 div 2+1, Val =< ?VARLENGTH_LIMIT_2 div 2 ->
    <<Tag:?BIT_HEADER_SIZE, 2:2, Val:?VARLENGTH_LENGTH_2/big-signed>>;
tagged_varlength_signed(Tag, Val)
  when Val >= -?VARLENGTH_LIMIT_3 div 2+1, Val =< ?VARLENGTH_LIMIT_3 div 2 ->
    <<Tag:?BIT_HEADER_SIZE, 3:2, Val:?VARLENGTH_LENGTH_3/big-signed>>.


%% @doc For those moments when the situation requires exclusively an unsigned
%% integer, so the tag is not needed
varlength_unsigned(Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_0 ->
    <<0:2, Val:?VARLENGTH_LENGTH_0>>;
varlength_unsigned(Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_1 ->
    <<1:2, Val:?VARLENGTH_LENGTH_1>>;
varlength_unsigned(Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_2 ->
    <<2:2, Val:?VARLENGTH_LENGTH_2>>;
varlength_unsigned(Val)
  when Val >= 0, Val < ?VARLENGTH_LIMIT_3 ->
    <<3:2, Val:?VARLENGTH_LENGTH_3>>.
