-module('3eamc_encode').

-include("3eamc.hrl").

%% API
-export([
    val_int/1,
    val_xreg/1,
    val_zreg/1,
    varint/1
]).

%% Variable length UTF8-like encoding, highest bit is set to 1 for every
%% encoded 7+1 bit sequence, and is set to 0 in the last 7+1 bits
varint(N) when N < 0 -> erlang:error("varint: n<0");
varint(N) when N =< 127 -> <<0:1, N:7>>; % last byte
varint(N) -> [<<1:1, (N rem 128):7>>, varint(N bsr 7)].

val_xreg(X) when X >= 0 andalso X =< 15 ->
    ?VAL_X + (X bsr 4).

val_zreg(Z) when Z >= 0 andalso Z =< 15 ->
    ?VAL_Z + (Z bsr 4).

val_int(N) ->
    [?VAL_INTEGER, varint(N)].
