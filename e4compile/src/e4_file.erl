%%% @doc Binary module file writer.
-module(e4_file).

%% API
-export([to_iolist/1, bin_filename/1]).
-include("e4_j1.hrl").

to_iolist(#j1prog{output=Code0, literals=LitTab, atoms=AtomTab}) ->
    Compr = uncompressed, % value: uncompressed | gzipped
    Content = [
        section("CODE", Compr, encode_code(Compr, Code0)),
        section("LTRL", Compr, encode_literals(Compr, LitTab)),
        section("ATOM", Compr, encode_atoms(Compr, AtomTab))
    ],
    [
        "E4J1", e4_encode:varint(iolist_size(Content)), Content
    ].

section(Tag, Compression, Data) ->
    [encode_tag(Compression, Tag), e4_encode:varint(byte_size(Data)), Data].

bin_filename(F) ->
    RDot = string:rchr(F, $.),
    string:sub_string(F, 1, RDot) ++ "e4b".

encode_tag(uncompressed, T) -> T;
encode_tag(gzipped, T) -> string:to_lower(T).

encode_block(uncompressed, D) -> D;
encode_block(gzipped, D) -> [e4_encode:varint(iolist_size(D)), zlib:gzip(D)].

encode_literals(Compr, LitTab) ->
    LitTab1 = lists:keysort(2, LitTab),
    LitTab2 = [
        e4_encode:varint(length(LitTab1)),
        [term_to_binary(Val) || {Val, _Index} <- LitTab1]
    ],
%%    io:format("~p~n", [LitTab1]),
    Enc1 = encode_block(Compr, LitTab2),
    iolist_to_binary(Enc1).

encode_atoms(Compr, AtomTab) ->
    AtomTab1 = lists:keysort(2, AtomTab),
    EncodeAtom = fun(Str) ->
            <<(e4_encode:varint(byte_size(Str)))/binary, Str/binary>>
        end,
    AtomTab2 = [
        e4_encode:varint(length(AtomTab1)),
        [EncodeAtom(Val) || {Val, _Index} <- AtomTab1]
    ],
    Enc1 = encode_block(Compr, AtomTab2),
    iolist_to_binary(Enc1).

encode_code(Compr, Code) ->
    Code2 = encode_block(Compr, Code),
    iolist_to_binary(Code2).
