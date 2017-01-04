-module(e4_encode).

-include("e4_forth.hrl").

%% API
-export([
    varint/1
]).

%% @doc Variable length UTF8-like encoding, highest bit is set to 1 for every
%% encoded 7+1 bit sequence, and is set to 0 in the last 7+1 bits
varint(N) when N < 0 -> erlang:error("varint: n<0");
varint(N) when N =< 127 -> <<0:1, N:7>>; % last byte
varint(N) ->
    Bytes = [<<0:1, (N rem 128):7>>, varint_with_bit(N bsr 7)],
    lists:reverse(Bytes).

%% @doc Same as varint with high bit always set
varint_with_bit(N) when N =< 127 -> <<1:1, N:7>>; % last byte
varint_with_bit(N) ->
    Bytes = [<<1:1, (N rem 128):7>>, varint_with_bit(N bsr 7)],
    lists:reverse(Bytes).
