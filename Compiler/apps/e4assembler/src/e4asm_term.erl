%%% @doc Encode terms into VM memory format, suitable for raw loading and ready
%%% to be executed.
%%% Because we're building module image, runnable as soon as its loaded (and
%%% after the references updated), it must have all terms correctly sized, and
%%% small values must be compacted.
%%% @end

-module(e4asm_term).

%% API
-export([encode/2, encode/3]).

-include_lib("e4compiler/include/e4c.hrl").
-include_lib("compiler/src/beam_opcodes.hrl").


-define(TAG_IMMED, 3).

-define(IMM1_PID, 0).
-define(IMM1_PORT, 1).
-define(IMM1_IMMED2, 2).
-define(IMM1_SMALL, 3).

-define(IMM2_ATOM, 0).
-define(IMM2_CATCH, 1).
-define(IMM2_IMMED3, 2).
-define(IMM2_SPECIAL, 3).

-define(IMM3_XREG, 0).
-define(IMM3_YREG, 1).
-define(IMM3_LABEL, 2).
-define(IMM3_FLOATREG, 3).


%% @doc How wide are the terms.
term_bits() ->
  %% TODO: Asking for trouble here but hell is this fast
  get(e4_machine_word_bits).

imm1_bits() -> term_bits() - 4.

imm2_bits() -> term_bits() - 6.

imm3_bits() -> term_bits() - 8.


imm1(Imm1Tag, Value) ->
  case integer_fits_imm1(Value) of
    true -> <<Value:(imm1_bits()), Imm1Tag:2, ?TAG_IMMED:2>>;
    false -> ?COMPILE_ERROR("encode imm2 value won't fit ~p", [Value])
  end.


imm2(Imm2Tag, Value) ->
  case integer_fits_imm2(Value) of
    true -> <<Value:(imm2_bits()),
              Imm2Tag:2, ?IMM1_IMMED2:2, ?TAG_IMMED:2>>;
    false -> ?COMPILE_ERROR("encode imm2 value won't fit ~p", [Value])
  end.


imm3(Imm3Tag, Value) ->
  <<Value:(imm3_bits()),
    Imm3Tag:2, ?IMM2_IMMED3:2, ?IMM1_IMMED2:2, ?TAG_IMMED:2>>.


%% @doc Construct a word-sized bit structure resembling memory term format for
%% fast use in VM without recoding and rewriting

encode(nil, Mod) ->
  encode([], Mod);

encode({x, X}, _Mod) ->
  imm3(?IMM3_XREG, X);

encode({y, Y}, _Mod) ->
  imm3(?IMM3_YREG, Y);

%%encode({fp, Fp}, _Mod) ->
%%  imm3(?IMM3_FLOATREG, Fp);

encode({f, Label}, _Mod) ->
  imm3(?IMM3_LABEL, Label);

encode([], _Mod) ->
  imm2(?IMM2_SPECIAL, 0);

encode({atom, Atom}, Mod = #{'$' := e4mod}) ->
  %% Assume atom already exists, will crash if it doesn't
  imm2(?IMM2_ATOM, index_of(Atom, atoms, Mod) + 1);

encode({extfunc, Mod, Fun, Arity}, Mod0 = #{'$' := e4mod}) ->
  ImportIndex = index_of({Mod, Fun, Arity}, imports, Mod0),
  encode({integer, ImportIndex}, Mod0);

encode({lambda, Label, NumFree}, Mod0 = #{'$' := e4mod}) ->
  LambdaIndex = index_of({Label, NumFree}, lambdas, Mod0),
  encode({integer, LambdaIndex}, Mod0);

%%encode({jumptab, JTab}, Mod0 = #{'$' := e4mod}) ->
%%  JTabIndex = index_of(JTab, jumptabs, Mod0),
%%  beam_asm:encode(?tag_u, JTabIndex);

encode({literal, Lit}, Mod0 = #{'$' := e4mod}) ->
  LitIndex = index_of(Lit, literals, Mod0),
  imm2(?IMM2_ATOM, LitIndex);

encode(X, Mod) when is_integer(X) ->
  encode({integer, X}, Mod);

encode({integer, X}, Mod) ->
  case integer_fits_imm1(X) of
    true -> imm1(?IMM1_SMALL, X); % fits into small
    false -> encode({literal, X}, Mod) % too big, save to literals area
  end;

encode(X, _Mod) ->
  ?COMPILE_ERROR("do not know how to encode ~p", [X]).


tagged_val(Tag, Val, Width) when Tag >= (1 bsl Width) ->
  ?COMPILE_ERROR("encode ~p is too large for ~B bits", [Val, Width]);

tagged_val(Tag, Val, Width) ->
  <<Val:(Width - 3), Tag:3>>.


%% @doc Encode a smaller value into 'Width' bit space
encode({x, X}, Width, _Mod) ->
  tagged_val(?tag_x, X, Width);

encode({y, Y}, Width, _Mod) ->
  tagged_val(?tag_y, Y, Width);

encode({f, Label}, Width, _Mod) ->
  tagged_val(?tag_f, Label, Width);

encode({atom, Atom}, Width, Mod = #{'$' := e4mod}) ->
  %% Assume atom already exists, will crash if it doesn't
  AIndex = index_of(Atom, atoms, Mod) + 1,
  tagged_val(?tag_a, AIndex, Width);

encode(X, Width, _Mod) ->
  ?COMPILE_ERROR("do not know how to encode16 ~p", [X]).


index_of(Value, Key, Mod) ->
  case orddict:find(Value, maps:get(Key, Mod)) of
    {ok, X} -> X;
    error -> ?COMPILE_ERROR("value '~p' must be registered in module ~s",
                            [Value, Key])
  end.


integer_fits_imm1(X) ->
  X < (1 bsl imm1_bits()).


integer_fits_imm2(X) ->
  X < (1 bsl imm2_bits()).
