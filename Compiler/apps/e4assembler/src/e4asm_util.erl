%%% @doc
%%% @end

-module(e4asm_util).

-include_lib("e4compiler/include/e4c.hrl").

%% API
-export([assert_unsigned_fits/3, assert_signed_fits/3]).

assert_unsigned_fits(Name, N, auto_bits) ->
  ok; % no check
assert_unsigned_fits(Name, N, Bits) when is_integer(N), is_integer(Bits) ->
  Bin = <<N:Bits>>,
  <<N1:Bits>> = Bin,
  case N == N1 of
    true -> ok;
    false -> ?COMPILE_ERROR("Value ~s does not fit into ~B bits", [Name, Bits])
  end.


assert_signed_fits(Name, N, Bits) ->
  Bin = <<N:Bits/signed>>,
  <<N1:Bits/signed>> = Bin,
  case N == N1 of
    true -> ok;
    false -> ?COMPILE_ERROR("Value ~s does not fit into ~B bits", [Name, Bits])
  end.
