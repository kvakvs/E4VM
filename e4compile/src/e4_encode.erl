-module(e4_encode).

-include("e4.hrl").

%% API
-export([
    varint/1
]).

%% Variable length UTF8-like encoding, highest bit is set to 1 for every
%% encoded 7+1 bit sequence, and is set to 0 in the last 7+1 bits
varint(N) when N < 0 -> erlang:error("varint: n<0");
varint(N) when N =< 127 -> <<0:1, N:7>>; % last byte
varint(N) -> [<<1:1, (N rem 128):7>>, varint(N bsr 7)].
