%%% @doc Binary module file writer.
-module(e4_file).

%% API
-export([to_iolist/1, bin_filename/1]).
-include("e4_j1.hrl").

to_iolist(#j1prog{output=Code0, literals=LitTab, atoms=AtomTab}) ->
    Code    = encode_code(Code0),
    Lit     = encode_literals(LitTab),
    Atom    = encode_atoms(AtomTab),
    Content = [
        "CODE", <<(byte_size(Code)):32/big>>, Code,
        "LTRL", <<(byte_size(Lit)):32/big>>, Lit,
        "ATOM", <<(byte_size(Atom)):32/big>>, Atom
    ],
    [
        "E4J1", <<(iolist_size(Content)):32/big>>, Content
    ].

bin_filename(F) ->
    RDot = string:rchr(F, $.),
    string:sub_string(F, 1, RDot) ++ "4bin".

encode_literals(LitTab) ->
    LitTab1 = lists:keysort(2, LitTab),
    LitTab2 = [term_to_binary(Val) || {Val, _Index} <- LitTab1],
    Enc1 = [e4_encode:varint(iolist_size(LitTab2)), zlib:gzip(LitTab2)],
    iolist_to_binary(Enc1).

encode_atoms(AtomTab) ->
    AtomTab1 = lists:keysort(2, AtomTab),
    EncodeAtom = fun(Str) ->
            <<(e4_encode:varint(byte_size(Str)))/binary, Str/binary>>
        end,
    AtomTab2 = [EncodeAtom(Val) || {Val, _Index} <- AtomTab1],
    Enc1 = [e4_encode:varint(iolist_size(AtomTab2)), zlib:gzip(AtomTab2)],
    iolist_to_binary(Enc1).

encode_code(Code) ->
    Code2 = [e4_encode:varint(iolist_size(Code)), zlib:gzip(Code)],
    iolist_to_binary(Code2).