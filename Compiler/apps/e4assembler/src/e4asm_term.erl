%%% @doc Encode terms into VM memory format, suitable for raw loading and ready
%%% to be executed.
%%% Because we're building module image, runnable as soon as its loaded (and
%%% after the references updated), it must have all terms correctly sized, and
%%% small values must be compacted.
%%% @end

-module(e4asm_term).

%% API
-export([encode/2]).


-define(TAG_IMMED, 3).

-define(IMM1_IMM2, 2).
-define(IMM1_SMALLINT, 3).

-define(IMM2_ATOM, 0).


%% @doc How wide are the terms.
term_bits() ->
  %% TODO: Asking for trouble here but hell is this fast
  get(e4_machine_word_bits).

imm1_bits() -> term_bits() - 4.

imm2_bits() -> term_bits() - 6.


imm1(Imm1Tag, Value) ->
  <<Value:(imm1_bits()), Imm1Tag:2, ?TAG_IMMED:2>>.


imm2(Imm2Tag, Value) ->
  <<Value:(imm2_bits()), Imm2Tag:2, ?IMM1_IMM2:2, ?TAG_IMMED:2>>.


%% @doc Construct a word-sized bit structure resembling memory term format for
%% fast use in VM without recoding and rewriting
encode({x, X}, _Mod) -> beam_asm:encode(?tag_x, X);

encode({y, Y}, _Mod) -> beam_asm:encode(?tag_y, Y);

encode({f, F}, _Mod) -> beam_asm:encode(?tag_f, F);

encode(nil, _Mod) -> beam_asm:encode(?tag_a, 0);

encode([], _Mod) -> beam_asm:encode(?tag_a, 0);

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


index_of(Value, Key, Mod) ->
  case orddict:find(Value, maps:get(Key, Mod)) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("value '~p' must be registered in module ~s",
                            [Value, Key])
  end.
